{lib}: let
  declarations = set:
    if builtins.isList set
    then set
    else throw "cssSource: `set` must be a list of [property value] pairs, because declaration order matters in CSS";

  declaration = pair: "  ${builtins.elemAt pair 0}: ${builtins.elemAt pair 1};\n";

  indent = text: lib.concatMapStringsSep "\n" (line: lib.optionalString (line != "") "  ${line}") (lib.splitString "\n" text);

  node = n:
    if n ? select
    then "${n.select} {\n${lib.concatMapStrings declaration (declarations n.set)}}"
    else if n ? rules
    then "${n.at} {\n${indent (nodes n.rules)}\n}"
    else "${n.at};";

  nodes = lib.concatMapStringsSep "\n\n" node;

  jsonValue = prefix: value: let
    inner = prefix + "  ";
    items = open: close: list:
      if list == []
      then open + close
      else "${open}\n${lib.concatMapStringsSep ",\n" (item: inner + item) list}\n${prefix}${close}";
  in
    if builtins.isAttrs value
    then items "{" "}" (map (key: "${builtins.toJSON key}: ${jsonValue inner value.${key}}") (value.__keys or (builtins.attrNames value)))
    else if builtins.isList value
    then items "[" "]" (map (jsonValue inner) value)
    else builtins.toJSON value;

  tomlValue = value:
    if builtins.isList value
    then "[${lib.concatMapStringsSep ", " tomlValue value}]"
    else if builtins.isBool value
    then lib.boolToString value
    else if builtins.isString value
    then builtins.toJSON value
    else toString value;

  tomlLine = item:
    if builtins.isList item
    then "${builtins.elemAt item 0} = ${tomlValue (builtins.elemAt item 1)}"
    else if item ? comment
    then "# ${item.comment}"
    else "[[${item.arrayTable}]]";

  keyValueSource = prefix: lib.concatMapStringsSep "\n" (lib.concatMapStrings (pair: "${prefix}${builtins.elemAt pair 0}=${builtins.elemAt pair 1}\n"));

  xmlElement = prefix: element:
    if element ? children
    then "${prefix}<${element.name}>\n${lib.concatMapStrings (xmlElement "${prefix} ") element.children}${prefix}</${element.name}>\n"
    else "${prefix}<${element.name}>${element.text}</${element.name}>\n";

  wrap = width: text:
    lib.optionalString (text != "") "${builtins.substring 0 width text}\n${wrap width (builtins.substring width (builtins.stringLength text) text)}";

  kindOf = value:
    if builtins.isAttrs value
    then builtins.head (builtins.attrNames value)
    else builtins.typeOf value;

  spelling = lib.removeSuffix "'";

  boundBlock = {
    language,
    name,
    declared,
  }: body: let
    formals = builtins.attrNames (builtins.functionArgs body);
    result = body (lib.genAttrs formals (formal: name (spelling formal)));
    missing = lib.subtractLists (declared result) (map spelling formals);
  in
    if !builtins.isFunction body
    then body
    else if missing != []
    then throw "${language}: ${lib.concatStringsSep ", " missing} bound but not declared in its block"
    else result;

  infixTree = {
    language,
    precedence,
    right ? [],
    operand,
  }: let
    parse = items: let
      count = builtins.length items;
      operators = lib.genList (index: builtins.elemAt items (2 * index + 1)) (count / 2);
      levels = map (symbol: precedence.${symbol} or (throw "${language}: `${toString symbol}` is not a binary operator")) operators;
      lowest = lib.foldl' lib.min 1000 levels;
      indices = builtins.filter (index: builtins.elemAt levels index == lowest) (lib.range 0 (builtins.length operators - 1));
      at =
        if builtins.elem (builtins.elemAt operators (builtins.head indices)) right
        then builtins.head indices
        else lib.last indices;
    in
      if count == 1
      then operand (builtins.head items)
      else if lib.mod count 2 == 0
      then throw "${language}: an infix list alternates operands and operators and ends in an operand"
      else {
        binary = {
          operator = builtins.elemAt operators at;
          left = parse (lib.take (2 * at + 1) items);
          right = parse (lib.drop (2 * at + 2) items);
        };
      };
  in
    parse;

  doc = let
    cons = margin: flat: part: tail: {
      head = {inherit margin flat part;};
      inherit tail;
    };

    push = margin: flat: parts: rest: builtins.foldl' (tail: part: cons margin flat part tail) rest (lib.reverseList parts);

    multiline = lib.hasInfix "\n";

    firstLine = string: builtins.head (lib.splitString "\n" string);

    lastLine = string: lib.last (lib.splitString "\n" string);

    inner = part: part.group or part.align or part.nest.part;

    fitting = {
      string = width: frame: rest:
        if multiline frame.part
        then !frame.flat && builtins.stringLength (firstLine frame.part) <= width
        else fits (width - builtins.stringLength frame.part) rest;
      list = width: frame: rest: fits width (push frame.margin frame.flat frame.part rest);
      break = width: frame: rest: !frame.flat || frame.part.break != null && fits (width - builtins.stringLength frame.part.break) rest;
      nest = width: frame: rest: fits width (cons frame.margin frame.flat (inner frame.part) rest);
      align = fitting.nest;
      group = fitting.nest;
    };

    fits = width: stack: width >= 0 && (stack == null || fitting.${kindOf stack.head.part} width stack.head stack.tail);

    emit = state: out: column: pad: stack:
      builtins.seq column (builtins.seq out {
        key = state.key + 1;
        inherit (state) unit;
        inherit out column pad stack;
      });

    start = state:
      if state.pad == null
      then state.column
      else state.pad;

    after = state: string:
      if multiline string
      then builtins.stringLength (lastLine string)
      else start state + builtins.stringLength string;

    write = state: string:
      if string == ""
      then emit state "" state.column state.pad
      else emit state (lib.optionalString (state.pad != null) (lib.strings.replicate state.pad state.unit) + string) (after state string) null;

    move = state: emit state "" state.column state.pad;

    steps = {
      string = _: state: frame: write state frame.part;
      list = _: state: frame: rest: move state (push frame.margin frame.flat frame.part rest);
      break = _: state: frame:
        if frame.flat && frame.part.break != null
        then write state frame.part.break
        else emit state "\n" 0 frame.margin;
      nest = _: state: frame: rest: move state (cons (frame.margin + frame.part.nest.by) frame.flat frame.part.nest.part rest);
      align = _: state: frame: rest: move state (cons (start state) frame.flat frame.part.align rest);
      group = width: state: frame: rest: let
        flat = frame.flat || width == null || fits (width - start state) (cons frame.margin true frame.part.group rest);
      in
        move state (cons frame.margin flat frame.part.group rest);
    };

    step = width: state: steps.${kindOf state.stack.head.part} width state state.stack.head state.stack.tail;

    renderWith = unit: width: part:
      lib.concatMapStrings (state: state.out) (builtins.genericClosure {
        startSet = [
          {
            key = 0;
            out = "";
            column = 0;
            pad = null;
            inherit unit;
            stack = cons 0 false part null;
          }
        ];
        operator = state: lib.optional (state.stack != null) (step width state);
      });
  in {
    line = {break = " ";};

    softline = {break = "";};

    hardline = {break = null;};

    nest = by: part: {nest = {inherit by part;};};

    align = part: {align = part;};

    group = part: {group = part;};

    render = renderWith " ";

    inherit renderWith;
  };

  lisp = let
    inherit (doc) hardline line nest align group;

    prefixes = {
      quote = "'";
      quasi = "`";
      unquote = ",";
      splice = ",@";
      function = "#'";
    };

    bodies = {
      block = 1;
      case = 1;
      define-extension = 3;
      defconstant = 1;
      defgeneric = 2;
      defmacro = 2;
      defmethod = 2;
      defpackage = 1;
      defparameter = 1;
      defstruct = 1;
      defun = 2;
      defvar = 1;
      destructuring-bind = 2;
      dolist = 1;
      dotimes = 1;
      ecase = 1;
      etypecase = 1;
      flet = 1;
      handler-bind = 1;
      handler-case = 1;
      ignore-errors = 0;
      labels = 1;
      lambda = 1;
      "let" = 1;
      "let*" = 1;
      macrolet = 1;
      multiple-value-bind = 2;
      prog1 = 1;
      progn = 0;
      typecase = 1;
      unless = 1;
      unwind-protect = 1;
      when = 1;
      with-input-from-string = 1;
      with-open-file = 1;
      with-output-to-string = 1;
      with-slots = 2;
    };

    locals = ["flet" "labels" "macrolet"];

    definers = ["defconstant" "defmacro" "defparameter" "defun" "defvar"];

    operators = [
      "*"
      "+"
      "-"
      "/"
      "/="
      "1+"
      "1-"
      "<"
      "<="
      "="
      ">"
      ">="
      "char="
      "let*"
      "list*"
      "string="
    ];

    cl = [
      "abs"
      "acons"
      "and"
      "append"
      "apply"
      "assoc"
      "atom"
      "car"
      "case"
      "cddr"
      "cdr"
      "ceiling"
      "char"
      "char-code"
      "code-char"
      "concatenate"
      "cond"
      "cons"
      "copy-list"
      "count-if"
      "declare"
      "defpackage"
      "defparameter"
      "defun"
      "defvar"
      "destructuring-bind"
      "digit-char-p"
      "do"
      "dolist"
      "dotimes"
      "ecase"
      "eq"
      "eql"
      "equal"
      "evenp"
      "every"
      "exp"
      "expt"
      "fifth"
      "find"
      "find-if"
      "first"
      "flet"
      "float"
      "floor"
      "format"
      "fourth"
      "get-internal-real-time"
      "getf"
      "if"
      "ignore"
      "in-package"
      "incf"
      "integerp"
      "intern"
      "internal-time-units-per-second"
      "intersection"
      "labels"
      "lambda"
      "last"
      "ldiff"
      "length"
      "let"
      "list"
      "listp"
      "loop"
      "map"
      "mapcar"
      "max"
      "member"
      "min"
      "mod"
      "most-positive-fixnum"
      "multiple-value-bind"
      "nconc"
      "nil"
      "not"
      "nreverse"
      "nth"
      "nthcdr"
      "null"
      "oddp"
      "or"
      "parse-integer"
      "plusp"
      "position"
      "position-if"
      "progn"
      "push"
      "read-line"
      "realp"
      "reduce"
      "remove"
      "remove-duplicates"
      "remove-if"
      "remove-if-not"
      "rest"
      "return"
      "reverse"
      "rotatef"
      "round"
      "second"
      "setf"
      "sin"
      "sort"
      "sqrt"
      "string"
      "string-downcase"
      "string-right-trim"
      "string-upcase"
      "stringp"
      "subseq"
      "subsetp"
      "substitute"
      "t"
      "third"
      "truncate"
      "unless"
      "values"
      "when"
      "with-input-from-string"
      "zerop"
    ];

    loop = [
      "across"
      "below"
      "by"
      "collect"
      "downto"
      "for"
      "from"
      "in"
      "on"
      "repeat"
      "sum"
      "thereis"
      "to"
      "while"
      "with"
    ];

    tomoe = [
      "+bar+"
      "+cassette+"
      "+gap+"
      "+glass+"
      "+hud+"
      "+hud-theme+"
      "+layout+"
      "+layout-bindings+"
      "+peek+"
      "+policy-bindings+"
      "+policy-displays+"
      "+policy-hidden-window+"
      "+policy-launcher+"
      "+policy-launches+"
      "+policy-settings+"
      "+policy-terminal+"
      "+shader-wallpaper+"
      "+sounds+"
      "adjust-brightness"
      "bind-key"
      "cassette--c"
      "cassette--degrees"
      "cassette--fit"
      "cassette--gib"
      "cassette--grille"
      "cassette--height"
      "cassette--hot-p"
      "cassette--led"
      "cassette--module"
      "cassette--plate"
      "cassette--px"
      "cassette--reel"
      "cassette--split"
      "cassette--window"
      "cassette--zone"
      "clock-text"
      "close-window"
      "configure-output"
      "context"
      "deck--narrow-p"
      "define-extension"
      "exec-async"
      "focus"
      "fullscreen"
      "hide-window"
      "hud--alpha"
      "hud--c"
      "hud--decay"
      "hud--dim"
      "hud--heat"
      "hud--icon"
      "hud--mix"
      "hud--now"
      "hud--off"
      "hud--panel"
      "hud--text"
      "interval"
      "keyboard-grab"
      "launch"
      "media-control"
      "menu-choice"
      "output-power"
      "place"
      "policy--area"
      "policy--centred"
      "policy--launch"
      "policy--terminal"
      "policy--unmanaged-p"
      "previous-context"
      "probe--gpu-p"
      "probe--sample"
      "publish-state"
      "quit"
      "raise-window"
      "rules-for"
      "screenshot"
      "service-state"
      "setting"
      "settings"
      "shell-surface"
      "show-window"
      "sound"
      "state-value"
      "theme"
      "ui"
      "watch-file"
      "window-properties"
      "window-rule"
    ];

    ekko = [
      "action"
      "register-command"
      "register-component"
      "register-keymap"
      "register-layout-provider"
      "registry"
      "set-option"
      "value"
    ];

    nixKeywords = ["assert" "else" "false" "if" "in" "inherit" "let" "null" "or" "rec" "then" "true" "with"];

    convention = spelled: let
      constant = builtins.match "[+](.+)[+]" spelled;
      special = builtins.match "[*](.+)[*]" spelled;
    in
      if constant != null
      then builtins.head constant
      else if special != null
      then builtins.head special
      else null;

    escaped = name:
      if builtins.elem name nixKeywords || constructors ? ${name}
      then "${name}'"
      else name;

    global = spelled: let
      base = convention spelled;
    in
      if base == null
      then lib.nameValuePair (escaped spelled) {symbol = spelled;}
      else lib.nameValuePair (escaped base) {inherit base spelled;};

    globals = builtins.listToAttrs (map global (cl ++ loop ++ tomoe ++ ekko));

    muffed = mark: name: let
      base = name.base or name.symbol;
      spelled = "${mark}${base}${mark}";
    in
      if name ? spelled && name.spelled != spelled
      then throw "lisp: ${name.spelled} is not ${spelled}"
      else {
        symbol = spelled;
        inherit base;
      };

    symbolName = value:
      if builtins.isString value && builtins.elem value operators
      then value
      else value.symbol or null;

    nodeKind = value:
      if value ? symbol
      then "symbol"
      else kindOf value;

    bound = boundBlock {
      language = "lisp";
      name = spelled: {symbol = spelled;};
      declared = declares;
    };

    listy = value: builtins.isList value || value ? rows || value ? wrap;

    elements = value: let
      resolved = bound value;
    in
      if builtins.isList resolved
      then resolved
      else if resolved ? rows
      then lib.concatMap rowItems resolved.rows
      else resolved.wrap or [];

    nameOf = value: lib.optional (value ? symbol) (value.base or value.symbol);

    marker = value: lib.hasPrefix "&" (toString (symbolName value));

    first = value: lib.concatMap nameOf (lib.take 1 (elements value));

    lambdaList = value:
      lib.concatMap (part:
        if listy part
        then first part
        else if marker part
        then []
        else nameOf part)
      (elements value);

    heads = value:
      lib.concatMap (part:
        if listy part
        then first part
        else nameOf part)
      (elements value);

    entryNames = defaulted: part:
      if defaulted && listy part
      then first part
      else pattern part;

    patternStep = acc: part:
      if marker part
      then acc // {defaulted = symbolName part != "&rest";}
      else acc // {names = acc.names ++ entryNames acc.defaulted part;};

    pattern = value:
      if listy value
      then
        (builtins.foldl' patternStep {
          defaulted = false;
          names = [];
        } (elements value)).names
      else if marker value || builtins.elem (symbolName value) ["." "nil"]
      then []
      else nameOf value;

    loopNames = parts:
      lib.concatLists (lib.imap0 (index: part:
        if builtins.elem (symbolName part) ["as" "for" "into" "with"] && index + 1 < builtins.length parts
        then pattern (builtins.elemAt parts (index + 1))
        else [])
      parts);

    declares = result: let
      parts = elements result;
      at = index:
        if index < builtins.length parts
        then builtins.elemAt parts index
        else [];
      definitions = lib.concatMap first (elements (at 1));
      table = {
        defun = lambdaList (at 2);
        defmacro = lambdaList (at 2);
        lambda = lambdaList (at 1);
        define-extension = lambdaList (at 3);
        "let" = heads (at 1);
        "let*" = heads (at 1);
        flet = definitions;
        labels = definitions;
        macrolet = definitions;
        destructuring-bind = pattern (at 1);
        multiple-value-bind = pattern (at 1);
        dolist = first (at 1);
        dotimes = first (at 1);
        with-input-from-string = first (at 1);
        with-open-file = first (at 1);
        with-output-to-string = first (at 1);
        loop = loopNames parts;
      };
      head = symbolName (builtins.head parts);
    in
      if parts != [] && head != null && table ? ${head}
      then table.${head}
      else lambdaList (at 1);

    defined = lib.concatMap (value: let
      parts = elements value;
    in
      if parts != [] && builtins.elem (symbolName (builtins.head parts)) definers
      then nameOf (builtins.elemAt parts 1)
      else []);

    bodyOffset = distinguished: index:
      if index > distinguished
      then 2
      else 4;

    headOffset = head: firstRow: let
      name = symbolName head;
    in
      if name != null && firstRow >= 2
      then builtins.stringLength name + 2
      else if name == "cond"
      then 2
      else 1;

    offset = head: local: index: firstRow: let
      name = symbolName head;
    in
      if local
      then bodyOffset 1 index
      else if name != null && bodies ? ${name}
      then bodyOffset bodies.${name} index
      else headOffset head firstRow;

    escape = lib.replaceStrings ["\\" "\""] ["\\\\" "\\\""];

    rowShapes = {
      list = lib.id;
      blank = _: [];
      at = row: rowItems row.row;
    };

    rowItems = row: (rowShapes.${kindOf row} or lib.toList) row;

    spaced = lib.intersperse " ";

    forms =
      {
        string = _: value:
          if builtins.elem value operators
          then value
          else throw "lisp: \"${value}\" is a bare string, not one of the operator symbols; a name is a Nix variable and a string literal (str ...)";
        int = _: toString;
        float = _: builtins.toJSON;
        lambda = local: value: form local (bound value);
        list = _: plain false;
        symbol = _: value: value.symbol;
        base = _: value:
          throw "lisp: ${value.spelled} is written (${
            if lib.hasPrefix "+" value.spelled
            then "constant"
            else "special"
          } ${value.base})";
        keyword = _: value: ":${value.keyword}";
        uninterned = _: value: "#:${value.uninterned}";
        char = _: value: "#\\${value.char}";
        ratio = _: value: "${toString value.ratio.numerator}/${toString value.ratio.denominator}";
        double = _: value: "${toString value.double}d0";
        hex = _: value: "#x${lib.toHexString value.hex}";
        str = _: value: "\"${escape value.str}\"";
        comment = _: value: value.comment;
        rows = local: value: broken local false value.rows;
        wrap = local: value: fill local value.wrap;
      }
      // lib.mapAttrs (kind: mark: _: value: [mark (form false value.${kind})]) prefixes;

    form = local: value: (forms.${nodeKind value} or (throw "lisp: no form ${nodeKind value}")) local value;

    bindingList = value: builtins.isList value || value ? rows;

    definitionForms = value:
      if builtins.isList value
      then plain true value
      else broken false true value.rows;

    items = children: values: let
      binds = builtins.elem (symbolName (builtins.head values)) locals;
    in
      lib.imap0 (index: value:
        if binds && index == 1 && bindingList value
        then definitionForms value
        else form children value)
      values;
    plain = children: values: ["(" (spaced (items children values)) ")"];

    broken = local: children: rowList: let
      firstRow = rowItems (builtins.head rowList);
      head = builtins.head firstRow;
      rendered = items children (lib.concatMap rowItems rowList);
      starts = builtins.foldl' (acc: row: acc ++ [(lib.last acc + builtins.length (rowItems row))]) [0] rowList;
      by = index: row: row.at or (offset head local (builtins.elemAt starts index) (builtins.length firstRow));
      written = index: row: spaced (lib.sublist (builtins.elemAt starts index) (builtins.length (rowItems row)) rendered);
      placed = index: row:
        if index == 0
        then written index row
        else if row ? blank
        then hardline
        else nest (by index row) [hardline (written index row)];
    in
      if lib.any (value: value ? comment) (rowItems (lib.last rowList))
      then throw "lisp: a comment cannot end a list's last row"
      else align ["(" (lib.imap0 placed rowList) ")"];

    fill = local: values: let
      head = builtins.head values;
      name = symbolName head;
      kept =
        if name != null && bodies ? ${name}
        then bodies.${name} + 1
        else lib.min 2 (builtins.length values);
      rendered = items false values;
    in
      group (align [
        "("
        (spaced (lib.take kept rendered))
        (lib.imap0 (index: value: nest (offset head local (index + kept) kept) [line value]) (lib.drop kept rendered))
        ")"
      ]);

    top = value:
      if value ? blank
      then [hardline]
      else [(form false value) hardline];

    constructors = {
      blank = {blank = true;};

      str = string: {str = string;};

      comment = string: {comment = string;};

      rows = rowList: {rows = rowList;};

      at = by: row: {
        at = by;
        inherit row;
      };

      wrap = values: {wrap = values;};

      inherit (lib.genAttrs ["quote" "quasi" "unquote" "splice" "function" "double" "hex"] (name: value: {${name} = value;})) quote quasi unquote splice function double hex;

      kw = name: {keyword = name;};

      uninterned = name: {uninterned = name;};

      char = name: {char = name;};

      ratio = numerator: denominator: {ratio = {inherit numerator denominator;};};

      constant = muffed "+";

      special = muffed "*";

      dot = {symbol = ".";};

      and-key = {symbol = "&key";};

      and-optional = {symbol = "&optional";};

      and-rest = {symbol = "&rest";};

      render = tree:
        doc.render 80 (map top (boundBlock {
            language = "lisp";
            name = spelled: {symbol = spelled;};
            declared = defined;
          }
          tree));
    };
  in
    globals // constructors;

  shell = let
    inherit (doc) hardline nest;

    spaced = lib.intersperse " ";

    named = spelled: {name = spelled;};

    free = ["HOME" "IFS" "LANG" "OLDPWD" "PATH" "PWD" "SHELL" "TERM" "TMPDIR" "USER" "XDG_CACHE_HOME" "XDG_CONFIG_HOME" "XDG_DATA_HOME" "XDG_RUNTIME_DIR" "XDG_STATE_HOME"];

    specials = {
      args = "@";
      argsJoined = "*";
      argCount = "#";
      exitStatus = "?";
    };

    expansions = {
      default = ":-";
      required = ":?";
      dropPrefix = "#";
      dropLongestPrefix = "##";
      dropSuffix = "%";
      dropLongestSuffix = "%%";
    };

    unaryTests = {
      nonEmpty = "-n";
      isEmpty = "-z";
      exists = "-e";
      isDir = "-d";
      isFile = "-f";
    };

    binaryTests = ["=" "!=" "-eq" "-ne" "-lt" "-le" "-gt" "-ge"];

    redirections = ["<" ">" ">>" "2>" "2>>" ">&" "2>&" "&>"];

    unsafe = {
      bare = ["$" "`" "\"" "'" "\\" ";" "&" "|" "<" ">" "(" ")" " " "\t" "\n"];
      operand = ["$" "`" "\"" "'" "\\" "}"];
    };

    spellingOf = target: target.name or (throw "shell: a ${kindOf target} where a name is expected");

    parameterOf = target:
      if builtins.isInt target && target >= 0
      then toString target
      else target.name or target.special or (throw "shell: a ${kindOf target} where a variable or parameter is expected");

    expansion = operator: target: operand: {
      expand = {
        parameter = parameterOf target;
        inherit operator operand;
      };
    };

    rejected = context: text: context == "bare" && (text == "" || lib.hasPrefix "#" text) || lib.any (symbol: lib.hasInfix symbol text) unsafe.${context};

    literal = context: text:
      if context == "quoted"
      then builtins.replaceStrings ["\\" "\"" "$" "`"] ["\\\\" "\\\"" "\\$" "\\`"] text
      else if rejected context text
      then throw "shell: `${text}` cannot be written as it is here; quote it with sq or dq, or build it from nodes"
      else text;

    leading = part:
      if builtins.isString part || builtins.isInt part
      then builtins.substring 0 1 (toString part)
      else "";

    braced = next: parameter:
      if builtins.match "[0-9]+" parameter != null
      then builtins.stringLength parameter > 1 || builtins.match "[0-9]" next != null
      else builtins.match "[A-Za-z_][A-Za-z0-9_]*" parameter != null && builtins.match "[A-Za-z0-9_]" next != null;

    within = contexts: kind: context: output:
      if builtins.elem context contexts
      then output
      else throw "shell: ${kind} is not written inside ${context} text";

    pieces = {
      name = context: _: value: within ["bare"] "a name (use var for its value)" context value.name;
      expand = _: next: value: let
        inherit (value.expand) parameter operator operand;
      in
        if operator == "[@]"
        then "\${${parameter}[@]}"
        else if operator != null
        then ["\${${parameter}${operator}" (written "operand" operand) "}"]
        else if braced next parameter
        then "\${${parameter}}"
        else "$" + parameter;
      sub = _: _: value: ["$(" (sequence value.sub) ")"];
      dq = context: _: value: within ["bare"] "dq" context ["\"" (joined "quoted" (lib.toList value.dq)) "\""];
      sq = context: _: value:
        within ["bare"] "sq" context (
          if lib.hasInfix "'" value.sq
          then throw "shell: sq cannot hold a single quote"
          else "'${value.sq}'"
        );
      ansic = context: _: value: within ["bare"] "ansic" context "$'${builtins.replaceStrings ["\\" "'" "\t" "\n"] ["\\\\" "\\'" "\\t" "\\n"] value.ansic}'";
      word = context: _: value: within ["bare" "operand"] "word" context (joined context value.word);
      array = context: _: value: within ["bare"] "array" context ["(" (spaced (map (written "bare") value.array)) ")"];
      assign = context: _: value:
        within ["bare"] "assign" context [
          value.assign.name
          "="
          (lib.optional (value.assign.value != "") (written "bare" value.assign.value))
        ];
      redir = context: _: value: let
        inherit (value.redir) operator target;
      in
        within ["bare"] "redir" context (
          if builtins.elem operator redirections
          then [operator (lib.optionalString (quoted target) " ") (written "bare" target)]
          else throw "shell: `${operator}` is not a redirection"
        );
      heredoc = context: _: value: within ["bare"] "heredoc" context "<<'${value.heredoc.delimiter}'";
    };

    piece = context: next: value:
      if builtins.isString value
      then literal context value
      else if builtins.isInt value
      then toString value
      else (pieces.${kindOf value} or (throw "shell: no word ${kindOf value}")) context next value;

    joined = context: parts: let
      count = builtins.length parts;
    in
      lib.imap0 (index: piece context (lib.optionalString (index + 1 < count) (leading (builtins.elemAt parts (index + 1))))) parts;

    written = context: piece context "";

    quoted = value: builtins.elem (kindOf value) ["dq" "sq" "ansic"] || (kindOf value == "word" && quoted (builtins.head value.word));

    documents = {
      list = lib.concatMap heredocs;
      heredoc = value: ["\n${value.heredoc.body}${value.heredoc.delimiter}"];
      pipe = value: heredocs (value.pipe.rows or value.pipe);
      chain = value: heredocs (value.chain.rows or value.chain);
    };

    heredocs = value: (documents.${kindOf value} or (_: [])) value;

    sequence = values: lib.intersperse "; " (map statement values);

    stages = operator: value:
      if value ? rows
      then [(statement (builtins.head value.rows)) (map (stage: nest 2 [operator hardline (statement stage)]) (builtins.tail value.rows))]
      else lib.intersperse "${operator} " (map statement value);

    link = broke: index: value:
      if lib.mod index 2 == 0
      then statement value
      else if !builtins.elem value ["&&" "||"]
      then throw "shell: a chain alternates statements and && or ||"
      else if broke
      then nest 2 [" ${value}" hardline]
      else " ${value} ";

    chain = value: lib.imap0 (link (value ? rows)) (value.rows or value);

    entry = value:
      if value ? blank
      then []
      else if value ? inline
      then sequence value.inline
      else [(statement value) (heredocs value)];

    indented = values: map (value: [hardline (entry value)]) values;

    block = values: nest 2 (indented values);

    enclosed = open: close: value:
      if value ? inline
      then [open " " (sequence value.inline) "; " close]
      else [open (block value) hardline close];

    branch = value:
      if value ? inline
      then [" " (sequence value.inline) "; "]
      else [(block value) hardline];

    alternative = value:
      if value ? inline
      then ["else " (sequence value.inline) "; "]
      else ["else" (block value) hardline];

    arm = value: let
      patterns = lib.intersperse "|" (map (written "bare") (lib.toList value.arm.patterns));
    in
      if value.arm.body ? inline
      then [patterns ") " (sequence value.arm.body.inline) " ;;"]
      else [patterns ")" (nest 2 [(indented value.arm.body) hardline ";;"])];

    armList = value:
      if value ? inline
      then [" in " (spaced (map arm value.inline)) " esac"]
      else [" in" (nest 2 (map (one: [hardline (arm one)]) value)) hardline "esac"];

    comparison = value: builtins.isList value && builtins.length value == 3 && builtins.elem (builtins.elemAt value 1) binaryTests;

    condition = value:
      if value ? unaryTest
      then [value.unaryTest.operator " " (written "bare" value.unaryTest.operand)]
      else if comparison value
      then [(written "bare" (builtins.head value)) " ${builtins.elemAt value 1} " (written "bare" (lib.last value))]
      else throw "shell: a test is a unary test or [left operator right] with an operator of ${toString binaryTests}";

    statementForms = {
      list = value:
        if value == []
        then throw "shell: an empty command"
        else spaced (map (written "bare") value);
      comment = value: "#${lib.optionalString (value.comment != "") " "}${value.comment}";
      shebang = value: "#!${value.shebang}";
      assign = written "bare";
      test = value: ["[ " (condition value.test) " ]"];
      pipe = value: stages " |" value.pipe;
      chain = value: chain value.chain;
      fn = value: [value.fn.name "() " (enclosed "{" "}" value.fn.body)];
      braces = value: enclosed "{" "}" value.braces;
      "if" = value: ["if " (statement value."if".test) "; then" (branch value."if".body) (lib.optional (value."if" ? otherwise) (alternative value."if".otherwise)) "fi"];
      for = value: ["for ${value.for.name} in " (spaced (map (written "bare") value.for.words)) "; " (enclosed "do" "done" value.for.body)];
      while = value: ["while " (statement value.while.test) "; " (enclosed "do" "done" value.while.body)];
      case = value: ["case " (written "bare" value.case.subject) (armList value.case.arms)];
    };

    statement = value: (statementForms.${kindOf value} or (throw "shell: no statement ${kindOf value}")) value;

    statementsOf = value: value.inline or value;

    inner = {
      inline = value: value.inline;
      pipe = value: value.pipe.rows or value.pipe;
      chain = value: builtins.filter (one: !builtins.isString one) (value.chain.rows or value.chain);
      braces = value: statementsOf value.braces;
      "if" = value: [value."if".test] ++ statementsOf value."if".body ++ lib.optionals (value."if" ? otherwise) (statementsOf value."if".otherwise);
      for = value: statementsOf value.for.body;
      while = value: [value.while.test] ++ statementsOf value.while.body;
      case = value: lib.concatMap (one: statementsOf one.arm.body) (statementsOf value.case.arms);
    };

    reachable = lib.concatMap (one: [one] ++ reachable ((inner.${kindOf one} or (_: [])) one));

    commandWords = one:
      if builtins.isList one
      then one
      else lib.optional (one ? assign) one;

    assigned = words: map (word: word.assign.name) (builtins.filter (word: word ? assign) words);

    namesIn = words: map (word: word.name) (builtins.filter (word: word ? name) words);

    leader = lib.findFirst (word: !(word ? assign)) null;

    givenBy = one: let
      words = commandWords one;
    in
      assigned words
      ++ lib.optionals (builtins.isString (leader words)) (namesIn words)
      ++ lib.optional (one ? for) one.for.name
      ++ lib.optional (one ? fn) one.fn.name
      ++ lib.optionals (one ? fn) (lib.subtractLists one.fn.locals (lib.concatMap givenBy (reachable (statementsOf one.fn.body))));

    localsBy = one: let
      words = commandWords one;
    in
      lib.optionals (leader words == "local") (assigned words ++ namesIn words);

    bound = declared:
      boundBlock {
        language = "shell";
        name = named;
        inherit declared;
      };
  in
    lib.genAttrs free named
    // builtins.mapAttrs (_: special: {inherit special;}) specials
    // builtins.mapAttrs (_: expansion) expansions
    // builtins.mapAttrs (_: operator: operand: {unaryTest = {inherit operator operand;};}) unaryTests
    // {
      blank = {blank = true;};

      inherit (lib.genAttrs ["comment" "shebang" "sq" "ansic"] (name: string: {${name} = string;})) comment shebang sq ansic;

      inherit (lib.genAttrs ["inline" "rows" "sub" "array" "pipe" "chain"] (name: values: {${name} = values;})) inline rows sub array pipe chain;

      var = target: expansion null target null;

      elements = target: {
        expand = {
          parameter = spellingOf target;
          operator = "[@]";
          operand = null;
        };
      };

      dq = parts: {dq = parts;};

      word = parts: {word = parts;};

      assign = target: value: {
        assign = {
          name = spellingOf target;
          inherit value;
        };
      };

      redir = operator: target: {redir = {inherit operator target;};};

      heredoc = delimiter: body:
        if body != "" && !(lib.hasSuffix "\n" body)
        then throw "shell: a heredoc body ends with a newline"
        else if builtins.elem delimiter (lib.splitString "\n" body)
        then throw "shell: a heredoc body holds its delimiter `${delimiter}` as a line"
        else {heredoc = {inherit delimiter body;};};

      test = expression: {test = expression;};

      fn = target: body: {
        fn = {
          name = spellingOf target;
          locals = lib.optionals (builtins.isFunction body) (map spelling (builtins.attrNames (builtins.functionArgs body)));
          body = bound (statements: lib.concatMap localsBy (reachable (statementsOf statements))) body;
        };
      };

      braces = value: {braces = value;};

      if' = test: body: {"if" = {inherit test body;};};

      ifElse = test: body: otherwise: {"if" = {inherit test body otherwise;};};

      for = target: words: body: {
        for = {
          name = spellingOf target;
          inherit words body;
        };
      };

      while = test: body: {while = {inherit test body;};};

      case = subject: arms: {case = {inherit subject arms;};};

      arm = patterns: body: {arm = {inherit patterns body;};};

      render = script: doc.render 80 (map (value: [(entry value) hardline]) (bound (statements: lib.concatMap givenBy (reachable statements)) script));
    };

  luau = let
    inherit (doc) hardline;

    newline = depth: [hardline (lib.strings.replicate depth "\t")];

    comma = lib.intersperse ", ";

    spaced = lib.intersperse " ";

    count = needle: string: builtins.length (lib.splitString needle string) - 1;

    plain = parts: lib.concatStrings (builtins.filter builtins.isString parts);

    mark = string:
      if count "\"" string > count "'" string
      then "'"
      else "\"";

    escape = quote: lib.replaceStrings ["\\" quote "\n" "\t" "\r"] ["\\\\" "\\${quote}" "\\n" "\\t" "\\r"];

    quoted = value: let
      parts = lib.toList value;
      quote = mark (plain parts);
    in [
      quote
      (map (part:
        if builtins.isString part
        then escape quote part
        else "\\u{${part.unicode}}")
      parts)
      quote
    ];

    binary = {
      "or" = 1;
      "and" = 2;
      "<" = 3;
      ">" = 3;
      "<=" = 3;
      ">=" = 3;
      "~=" = 3;
      "==" = 3;
      ".." = 4;
      "+" = 5;
      "-" = 5;
      "*" = 6;
      "/" = 6;
      "//" = 6;
      "%" = 6;
      "^" = 8;
    };

    right = [".." "^"];

    tree = infixTree {
      language = "luau";
      precedence = binary;
      inherit right;
      operand = lib.id;
    };

    globals = [
      "AgentBlock"
      "AgentFinishedEvent"
      "AgentStateEvent"
      "CommandFinishedEvent"
      "FocusEvent"
      "PaneInfo"
      "PickItem"
      "ProcessResult"
      "StatusPaneInfo"
      "StatusSegment"
      "TabEvent"
      "TabInfo"
      "WindowCx"
      "any"
      "boolean"
      "ipairs"
      "math"
      "number"
      "os"
      "pairs"
      "pcall"
      "proproblems"
      "require"
      "string"
      "table"
      "tern"
      "tonumber"
      "tostring"
      "type"
      "utf8"
    ];

    word = "[A-Za-z_][A-Za-z0-9_]*";

    identifier = value:
      if builtins.isString value && builtins.match word value != null
      then value
      else throw "luau: `${toString value}` is not a name";

    named = spelled: {name = identifier spelled;};

    nameOf = value: value.name or (throw "luau: a declaration names a bound name, not a ${kindOf value}");

    declaredName = value:
      nameOf (
        if value ? typed
        then value.typed.value
        else value
      );

    declaring = {
      local = value: map declaredName (lib.toList value.local.names);
      localFunction = value: [(nameOf value.localFunction.name)];
      type = value: [(nameOf value.type.name)];
      trailing = value: declares value.trailing.statement;
    };

    declares = value: (declaring.${kindOf value} or (_: [])) value;

    single = value:
      if builtins.isAttrs value && builtins.length (builtins.attrNames value) == 1
      then builtins.head (builtins.attrNames value)
      else throw "luau: a parameter or loop variable is a one-attribute set, {name = type;} or {name = null;}";

    bound = value: identifier (spelling (single value));

    resolve = given: body: let
      formals =
        if builtins.isFunction body
        then map spelling (builtins.attrNames (builtins.functionArgs body))
        else [];
      statements =
        boundBlock {
          language = "luau";
          name = named;
          declared = listed: given ++ lib.concatMap declares listed;
        }
        body;
      unbound = lib.subtractLists formals (lib.concatMap declares statements);
    in
      if unbound != []
      then throw "luau: ${lib.concatStringsSep ", " unbound} declared but not bound by its block"
      else statements;

    modes = {
      value = {
        separator = " = ";
        item = expression;
      };
      type = {
        separator = ": ";
        item = type;
      };
    };

    entryForms = mode: {
      field = depth: value: [(identifier value.field.name) mode.separator (mode.item depth value.field.value)];
      pair = depth: value: ["[" (mode.item depth value.pair.key) "]" mode.separator (mode.item depth value.pair.value)];
    };

    entry = mode: depth: value: ((entryForms mode).${kindOf value} or mode.item) depth value;

    table = mode: depth: value:
      if value.table == []
      then "{}"
      else ["{ " (comma (map (entry mode depth) value.table)) " }"];

    tableRow = mode: depth: value:
      if value ? blank
      then hardline
      else if value ? comment
      then [(newline depth) value.comment]
      else [(newline depth) (entry mode depth value) ","];

    tableRows = mode: depth: value: ["{" (map (tableRow mode (depth + 1)) value.table) (newline depth) "}"];

    keyword = last: index:
      if index == 0
      then "if"
      else if lib.mod index 2 == 1
      then "then"
      else if index == last
      then "else"
      else "elseif";

    clauses = depth: parts: lib.imap0 (index: part: [(keyword (builtins.length parts - 1) index) " " (expression depth part)]) parts;

    signed = value:
      if value < 0
      then 7
      else 9;

    strengths = {
      int = signed;
      float = signed;
      binary = value: binary.${value.binary.operator};
      infix = value: strength (tree value.infix);
      unary = _: 7;
      ifExpr = _: 0;
      cast = _: 0;
      rows = value: strength value.rows;
    };

    strength = value: (strengths.${kindOf value} or (_: 9)) value;

    grouped = depth: needed: value:
      if needed
      then ["(" (expression depth value) ")"]
      else expression depth value;

    side = depth: operator: left: value: let
      level = strength value;
      tight = binary.${operator};
    in
      grouped depth (level < tight || level == tight && left == builtins.elem operator right) value;

    prefix = value: builtins.elem (kindOf value) ["list" "name" "dot" "idx" "paren"] || kindOf value == "rows" && prefix value.rows;

    prefixed = depth: value: grouped depth (!prefix value) value;

    suffix = path:
      if builtins.match ":${word}|${word}([.]${word})*(:${word})?" path == null
      then throw "luau: `${toString path}` is not a field path"
      else if lib.hasPrefix ":" path
      then path
      else ".${path}";

    sameOperator = items: let
      operators = lib.unique (lib.genList (index: builtins.elemAt items (2 * index + 1)) (builtins.length items / 2));
    in
      if builtins.length operators == 1
      then builtins.head operators
      else throw "luau: rows breaks an infix run of one operator";

    brokenInfix = depth: items: let
      operator = sameOperator items;
      operands = lib.genList (index: builtins.elemAt items (2 * index)) (builtins.length items / 2 + 1);
      last = builtins.length operands - 1;
      leaning = builtins.elem operator right;
      left = index:
        if leaning
        then index < last
        else index == 0;
    in [
      (side (depth + 1) operator true (builtins.head operands))
      (lib.imap1 (index: operand: [(newline (depth + 1)) operator " " (side (depth + 1) operator (left index) operand)]) (builtins.tail operands))
    ];

    callDepth = depth: head:
      if head ? rows && head.rows ? dot
      then depth + 1
      else depth;

    brokenCall = depth: value: let
      inner = callDepth depth (builtins.head value);
    in [(prefixed depth (builtins.head value)) "(" (lib.intersperse "," (map (argument: [(newline (inner + 1)) (expression (inner + 1) argument)]) (builtins.tail value))) (newline inner) ")"];

    rowForms = {
      list = brokenCall;
      table = tableRows modes.value;
      infix = depth: value: brokenInfix depth value.infix;
      ifExpr = depth: value: let
        rows = value.ifExpr;
        parts = clauses (depth + 1) (lib.concatLists rows);
        starts = builtins.foldl' (acc: row: acc ++ [(lib.last acc + builtins.length row)]) [0] rows;
      in
        lib.imap0 (index: row: [(lib.optional (index > 0) (newline (depth + 1))) (spaced (lib.sublist (builtins.elemAt starts index) (builtins.length row) parts))]) rows;
      dot = depth: value: [(prefixed depth value.dot.value) (newline (depth + 1)) (suffix value.dot.path)];
    };

    unary = depth: value: let
      inherit (value.unary) operator operand;
      level = strength operand;
      doubled = operator == "-" && level == 7 && (!builtins.isAttrs operand || operand.unary.operator == "-");
    in [
      operator
      (lib.optionalString (operator == "not") " ")
      (grouped depth (level < 7 || doubled) operand)
    ];

    parameters = params:
      if params ? typed
      then params.typed.value
      else params;

    variable = depth: value: let
      kind = value.${single value};
    in [
      (bound value)
      (lib.optionals (kind != null) [": " (type depth kind)])
    ];

    signature = depth: value:
      if value ? typed
      then [(signature depth value.typed.value) ": " (type depth value.typed.type)]
      else ["(" (comma (map (variable depth) value)) ")"];

    function = depth: params: body: let
      statements = resolve (map bound (parameters params)) body;
    in [
      (signature depth params)
      (
        if statements == []
        then " "
        else lines depth statements
      )
      "end"
    ];

    expressionForms = {
      string = _: value: throw "luau: the string `${value}` where an expression is expected; a name is a Nix variable, a literal is str";
      int = _: toString;
      float = _: value: let
        text = builtins.toJSON value;
      in
        if builtins.match "-?[0-9]+[.][0-9]+" text != null
        then text
        else throw "luau: the float ${text} has no decimal spelling";
      bool = _: value:
        if value
        then "true"
        else "false";
      name = _: value: value.name;
      nil = _: _: "nil";
      hex = _: value: "0x${lib.toHexString value.hex}";
      list = depth: value: [(prefixed depth (builtins.head value)) "(" (comma (map (expression (callDepth depth (builtins.head value))) (builtins.tail value))) ")"];
      str = _: value: quoted value.str;
      long = _: value: "[[${value.long}]]";
      table = table modes.value;
      rows = depth: value: rowForms.${kindOf value.rows} depth value.rows;
      infix = depth: value: expression depth (tree value.infix);
      binary = depth: value: [(side depth value.binary.operator true value.binary.left) " ${value.binary.operator} " (side depth value.binary.operator false value.binary.right)];
      inherit unary;
      paren = depth: value: ["(" (expression depth value.paren) ")"];
      dot = depth: value: [(prefixed depth value.dot.value) (suffix value.dot.path)];
      idx = depth: value: [(prefixed depth value.idx.value) "[" (expression depth value.idx.key) "]"];
      fn = depth: value: ["function" (function depth value.fn.params value.fn.body)];
      ifExpr = depth: value: spaced (clauses depth value.ifExpr);
      cast = depth: value: [(expression depth value.cast.value) " :: " (type depth value.cast.type)];
      tuple = depth: value: comma (map (expression depth) value.tuple);
      typed = depth: value: [(expression depth value.typed.value) ": " (type depth value.typed.type)];
    };

    expression = depth: value: (expressionForms.${kindOf value} or (throw "luau: no expression ${kindOf value}")) depth value;

    typeForms = {
      string = _: value: throw "luau: the string `${value}` where a type is expected; a type name is a Nix variable";
      name = _: value: value.name;
      nil = _: _: "nil";
      opt = depth: value: [(type depth value.opt) "?"];
      table = table modes.type;
      rows = depth: value: tableRows modes.type depth value.rows;
      tuple = depth: value: ["(" (comma (map (type depth) value.tuple)) ")"];
    };

    type = depth: value: (typeForms.${kindOf value} or (throw "luau: no type ${kindOf value}")) depth value;

    targets = depth: value:
      if builtins.isList value
      then comma (map (expression depth) value)
      else expression depth value;

    line = depth: value:
      if value ? blank
      then hardline
      else [(newline depth) (statement depth value)];

    lines = depth: statements: [(map (line (depth + 1)) statements) (newline depth)];

    block = depth: given: body: lines depth (resolve given body);

    alternative = depth: value:
      if value ? "if"
      then ["elseif " (clause depth value."if")]
      else ["else" (block depth [] value) "end"];

    clause = depth: value: [
      (expression depth value.test)
      " then"
      (block depth [] value.body)
      (
        if value ? otherwise
        then alternative depth value.otherwise
        else "end"
      )
    ];

    statementForms = {
      string = _: value: throw "luau: the string `${value}` where a statement is expected";
      list = expression;
      rows = expression;
      comment = _: value: value.comment;
      local = depth: value: ["local " (targets depth value.local.names) " = " (expression depth value.local.value)];
      assign = depth: value: [(targets depth value.assign.targets) " = " (expression depth value.assign.value)];
      update = depth: value: [(targets depth value.update.targets) " ${value.update.operator} " (expression depth value.update.value)];
      return = depth: value: ["return " (expression depth value.return)];
      localFunction = depth: value: ["local function " (nameOf value.localFunction.name) (function depth value.localFunction.params value.localFunction.body)];
      "if" = depth: value: ["if " (clause depth value."if")];
      for = depth: value: ["for " (comma (map (variable depth) value.for.names)) " in " (expression depth value.for.values) " do" (block depth (map bound value.for.names) value.for.body) "end"];
      forRange = depth: value: ["for " (variable depth value.forRange.name) " = " (expression depth value.forRange.bounds) " do" (block depth [(bound value.forRange.name)] value.forRange.body) "end"];
      while = depth: value: ["while " (expression depth value.while.test) " do" (block depth [] value.while.body) "end"];
      type = depth: value: ["type " (nameOf value.type.name) " = " (type depth value.type.value)];
      trailing = depth: value: [(statement depth value.trailing.statement) " " value.trailing.comment];
    };

    statement = depth: value:
      if value ? __functor
      then "return"
      else (statementForms.${kindOf value} or (throw "luau: no statement ${kindOf value}")) depth value;

    prefixOperator = operator: operand: {unary = {inherit operator operand;};};

    constructors = {
      blank = {blank = true;};

      comment = string: {comment = string;};

      trailing = command: note: {
        trailing = {
          statement = command;
          comment = note;
        };
      };

      inherit (lib.genAttrs ["rows" "str" "hex" "paren"] (name: value: {${name} = value;})) rows str hex paren;

      unicode = code: {unicode = code;};

      long = string: {long = string;};

      nil = {nil = true;};

      table = entries: {table = entries;};

      field = name: value: {field = {inherit name value;};};

      pair = key: value: {pair = {inherit key value;};};

      infix = values: {infix = values;};

      not = prefixOperator "not";

      neg = prefixOperator "-";

      len = prefixOperator "#";

      dot = value: path: {dot = {inherit value path;};};

      idx = value: key: {idx = {inherit value key;};};

      fn = params: body: {fn = {inherit params body;};};

      ifExpr = parts: {ifExpr = parts;};

      cast = value: kind: {
        cast = {
          inherit value;
          type = kind;
        };
      };

      tuple = values: {tuple = values;};

      typed = value: kind: {
        typed = {
          inherit value;
          type = kind;
        };
      };

      opt = kind: {opt = kind;};

      local = names: value: {local = {inherit names value;};};

      assign = names: value: {
        assign = {
          targets = names;
          inherit value;
        };
      };

      update = operator: names: value: {
        update = {
          inherit operator value;
          targets = names;
        };
      };

      return = {__functor = _: value: {return = value;};};

      localFunction = name: params: body: {localFunction = {inherit name params body;};};

      if' = test: body: {"if" = {inherit test body;};};

      ifElse = test: body: otherwise: {"if" = {inherit test body otherwise;};};

      for = names: values: body: {for = {inherit names values body;};};

      forRange = name: bounds: body: {forRange = {inherit name bounds body;};};

      while = test: body: {while = {inherit test body;};};

      type = name: value: {type = {inherit name value;};};

      render = body: doc.render null (map (value: [(lib.optional (!(value ? blank)) (statement 0 value)) hardline]) (resolve [] body));
    };
  in
    constructors
    // builtins.listToAttrs (map (global: {
        name =
          if constructors ? ${global}
          then "${global}'"
          else global;
        value = named global;
      })
      globals);

  vbs = let
    inherit (doc) hardline;

    binary = {
      Imp = 1;
      Eqv = 2;
      Xor = 3;
      Or = 4;
      And = 5;
      "=" = 7;
      "<>" = 7;
      "<" = 7;
      ">" = 7;
      "<=" = 7;
      ">=" = 7;
      Is = 7;
      "&" = 8;
      "+" = 9;
      "-" = 9;
      Mod = 10;
      "\\" = 11;
      "*" = 12;
      "/" = 12;
      "^" = 14;
    };

    globals = ["CBool" "CInt" "CLng" "CStr" "Chr" "CreateObject" "Empty" "Err" "GetObject" "InStr" "IsEmpty" "IsNull" "IsObject" "Join" "LCase" "Left" "Len" "Mid" "MsgBox" "Nothing" "Now" "Null" "Replace" "Right" "Split" "Trim" "UBound" "UCase" "WScript"];

    nameNode = spelled: {name = spelled;};

    identifier = text:
      if builtins.match "[A-Za-z_][A-Za-z0-9_]*" text != null
      then text
      else throw "vbs: `${text}` is not a name";

    memberPath = text:
      if builtins.match "[A-Za-z_][A-Za-z0-9_]*([.][A-Za-z_][A-Za-z0-9_]*)*" text != null
      then text
      else throw "vbs: `${text}` is not a member path";

    tabs = depth: lib.strings.replicate depth "\t";

    atom = part: {
      precedence = 15;
      doc = part;
    };

    paren = least: printed:
      if printed.precedence < least
      then ["(" printed.doc ")"]
      else printed.doc;

    infix = infixTree {
      language = "vbs";
      precedence = binary;
      operand = value:
        if builtins.isList value
        then infix value
        else value;
    };

    argumentList = values: lib.intersperse ", " (map (value: paren 0 (expression value)) values);

    forms = {
      int = value:
        if value < 0
        then {
          precedence = 13;
          doc = toString value;
        }
        else atom (toString value);
      bool = value:
        atom (
          if value
          then "True"
          else "False"
        );
      string = value: throw "vbs: the string `${value}` is not an expression: a name is a bound variable or a generator attribute, a literal is `str`";
      list = value: expression (infix value);
      name = value: atom (identifier value.name);
      str = value: atom "\"${lib.replaceStrings ["\""] ["\"\""] value.str}\"";
      dot = value: atom [(paren 15 (expression value.dot.base)) "." (memberPath value.dot.path)];
      call = value: atom [(paren 15 (expression value.call.function)) "(" (argumentList value.call.arguments) ")"];
      not = value: {
        precedence = 6;
        doc = ["Not " (paren 6 (expression value.not))];
      };
      neg = value: {
        precedence = 13;
        doc = ["-" (paren 13 (expression value.neg))];
      };
      binary = value: let
        precedence = binary.${value.binary.operator};
      in {
        inherit precedence;
        doc = [(paren precedence (expression value.binary.left)) " ${value.binary.operator} " (paren (precedence + 1) (expression value.binary.right))];
      };
    };

    expression = value: (forms.${kindOf value} or (throw "vbs: no expression ${kindOf value}")) value;

    written = value: paren 0 (expression value);

    assignable = value:
      if builtins.elem (kindOf value) ["name" "dot"]
      then written value
      else throw "vbs: an assignment's target is a name or a member, not ${kindOf value}";

    declaredName = value:
      if kindOf value == "name"
      then identifier value.name
      else throw "vbs: a declaration names a bound variable, not ${kindOf value}";

    branch = body:
      if builtins.isList body
      then body
      else throw "vbs: an If branch is a list of statements; a Dim in it declares a name of the script or procedure around it";

    declaring = {
      dim = value: map (one: one.name) value.dim;
      sub = value: [value.sub.name.name];
      function = value: [value.function.name.name];
      "if" = value: declaredIn (branch value."if".body) ++ declaredIn (branch (value."if".otherwise or []));
    };

    declaredIn = lib.concatMap (value: (declaring.${kindOf value} or (_: [])) value);

    scope = parameters:
      boundBlock {
        language = "vbs";
        name = nameNode;
        declared = result: parameters ++ declaredIn result;
      };

    modes = {
      null = "";
      byVal = "ByVal ";
      byRef = "ByRef ";
    };

    parameterName = parameter: spelling (builtins.head (builtins.attrNames parameter));

    parameterDoc = value: let
      mode = value.${builtins.head (builtins.attrNames value)};
    in
      if builtins.length (builtins.attrNames value) != 1
      then throw "vbs: a parameter is `{name = mode;}`"
      else [(modes.${kindOf mode} or (throw "vbs: a parameter's mode is null, byVal or byRef")) (identifier (parameterName value))];

    block = depth: map (value: [hardline (entry depth value)]);

    entry = depth: value:
      if value ? blank
      then []
      else [(tabs depth) (statement depth value)];

    procedure = keyword: depth: value: [
      "${keyword} "
      (declaredName value.name)
      "("
      (lib.intersperse ", " (map parameterDoc value.parameters))
      ")"
      (block (depth + 1) (scope (map parameterName value.parameters) value.body))
      hardline
      (tabs depth)
      "End ${keyword}"
    ];

    conditional = depth: value: [
      "If "
      (written value.test)
      " Then"
      (block (depth + 1) (branch value.body))
      (lib.optionals (value ? otherwise) [hardline (tabs depth) "Else" (block (depth + 1) (branch value.otherwise))])
      hardline
      (tabs depth)
      "End If"
    ];

    statementForms = {
      comment = _: value:
        if value.comment == ""
        then "'"
        else "' ${value.comment}";
      optionExplicit = _: _: "Option Explicit";
      onErrorResumeNext = _: _: "On Error Resume Next";
      onErrorGoTo0 = _: _: "On Error GoTo 0";
      dim = _: value: ["Dim " (lib.intersperse ", " (map declaredName value.dim))];
      assign = _: value: [(assignable value.assign.target) " = " (written value.assign.value)];
      set = _: value: ["Set " (assignable value.set.target) " = " (written value.set.value)];
      call = _: value: [(paren 15 (expression value.call.function)) (lib.optionals (value.call.arguments != []) [" " (argumentList value.call.arguments)])];
      "if" = depth: value: conditional depth value."if";
      sub = depth: value: procedure "Sub" depth value.sub;
      function = depth: value: procedure "Function" depth value.function;
    };

    statement = depth: value: (statementForms.${kindOf value} or (throw "vbs: no statement ${kindOf value}")) depth value;
  in
    lib.genAttrs globals nameNode
    // {
      blank = {blank = true;};

      comment = string: {comment = string;};

      inherit (lib.genAttrs ["optionExplicit" "onErrorResumeNext" "onErrorGoTo0" "byVal" "byRef"] (name: {${name} = {};})) optionExplicit onErrorResumeNext onErrorGoTo0 byVal byRef;

      str = string: {str = string;};

      dot = base: path: {dot = {inherit base path;};};

      call = function: arguments: {call = {inherit function arguments;};};

      not = value: {not = value;};

      neg = value: {neg = value;};

      dim = names: {dim = names;};

      assign = target: value: {assign = {inherit target value;};};

      set = target: value: {set = {inherit target value;};};

      if' = test: body: {"if" = {inherit test body;};};

      ifElse = test: body: otherwise: {"if" = {inherit test body otherwise;};};

      sub = name: parameters: body: {sub = {inherit name parameters body;};};

      function = name: parameters: body: {function = {inherit name parameters body;};};

      render = script: doc.render null (map (value: [(entry 0 value) hardline]) (scope [] script));
    };

  patch = let
    inherit (doc) hardline;

    marks = {
      string = text: " ${text}";
      add = value: "+${value.add}";
      del = value: "-${value.del}";
    };

    change = value: [((marks.${kindOf value} or (throw "patch: no change ${kindOf value}")) value) hardline];

    span = start: count:
      if count == 1
      then toString start
      else "${toString start},${toString count}";

    counted = kind: lib.count (value: kindOf value != kind);

    empty = count:
      if count == 0
      then 1
      else 0;

    header = delta: value: let
      old = counted "add" value.hunk.lines;
      new = counted "del" value.hunk.lines;
      start = value.hunk.start + delta + empty old - empty new;
    in {
      delta = delta + new - old;
      doc = ["@@ -${span value.hunk.start old} +${span start new} @@" (lib.optionalString (value.hunk ? heading) " ${value.hunk.heading}") hardline (map change value.hunk.lines)];
    };

    hunkDocs = values:
      (builtins.foldl' (state: value: let
          next = header state.delta value;
        in {
          inherit (next) delta;
          docs = state.docs ++ [next.doc];
        }) {
          delta = 0;
          docs = [];
        }
        values).docs;

    file = value: [
      (lib.optionals (value.file ? index) ["diff --git a/${value.file.path} b/${value.file.path}" hardline "index ${value.file.index}" hardline])
      "--- a/${value.file.path}"
      hardline
      "+++ b/${value.file.path}"
      hardline
      (hunkDocs value.file.hunks)
    ];
  in {
    file = path: hunks: {file = {inherit path hunks;};};

    git = path: index: hunks: {file = {inherit path index hunks;};};

    hunk = start: lines: {hunk = {inherit start lines;};};

    section = start: heading: lines: {hunk = {inherit start heading lines;};};

    add = text: {add = text;};

    del = text: {del = text;};

    render = values: doc.render null (map file values);
  };

  markdown = let
    inherit (doc) hardline line group nest;

    literal = text:
      if builtins.match ".*([`*\\<&]|~~|]\\(|]\\[|!\\[|(^|[^[:alnum:]])_|_($|[^[:alnum:]])).*" text != null
      then throw "markdown: the text `${text}` holds markup; code, emphasis and links are constructors, and text is only literal"
      else text;

    words = string: lib.intersperse (group line) (lib.splitString " " (literal string));

    codeSpan = text:
      if text == "" || lib.hasInfix "`" text || lib.hasPrefix " " text || lib.hasSuffix " " text
      then throw "markdown: the code span `${text}` is empty, holds a backtick or starts or ends with a space"
      else "`${text}`";

    destination = url:
      if builtins.match "[^ ()<>]+" url != null
      then url
      else throw "markdown: the link destination `${url}` needs escaping";

    inlineForms = {
      string = flow: value:
        if flow
        then words value
        else literal value;
      list = flow: map (inline flow);
      code = _: value: codeSpan value.code;
      strong = flow: value: ["**" (inline flow value.strong) "**"];
      em = flow: value: ["*" (inline flow value.em) "*"];
      link = flow: value: ["[" (inline flow value.link.text) "](${destination value.link.url})"];
    };

    inline = flow: value: (inlineForms.${kindOf value} or (throw "markdown: no inline ${kindOf value}")) flow value;

    paragraph = value:
      if value ? wrap
      then inline true value.wrap
      else inline false value;

    cells = width: row:
      if builtins.length row != width
      then throw "markdown: a table row of ${toString (builtins.length row)} cells under a header of ${toString width}"
      else ["| " (lib.intersperse " | " (map (inline false) row)) " |"];

    blockForms = {
      string = paragraph;
      list = paragraph;
      wrap = paragraph;
      heading = value:
        if value.heading.level < 1 || value.heading.level > 6
        then throw "markdown: no heading level ${toString value.heading.level}"
        else [(lib.strings.replicate value.heading.level "#") " " (inline false value.heading.text)];
      bullets = value: lib.intersperse hardline (map (item: ["- " (nest 2 (paragraph item))]) value.bullets);
      comment = value:
        if lib.hasInfix "-->" value.comment
        then throw "markdown: a comment holding `-->`"
        else "<!--${value.comment}-->";
      table = value: let
        width = builtins.length value.table.header;
      in
        lib.intersperse hardline ([(cells width value.table.header) "|${lib.strings.replicate width "---|"}"] ++ map (cells width) value.table.rows);
    };

    block = value:
      if value ? blank
      then []
      else (blockForms.${kindOf value} or (throw "markdown: no block ${kindOf value}")) value;
  in {
    blank = {blank = true;};

    comment = string: {comment = string;};

    inherit (lib.genAttrs ["wrap" "strong" "em"] (name: value: {${name} = value;})) wrap strong em;

    code = string: {code = string;};

    link = text: url: {link = {inherit text url;};};

    heading = level: text: {heading = {inherit level text;};};

    bullets = items: {bullets = items;};

    table = header: rows: {table = {inherit header rows;};};

    render = values: doc.render 78 (map (value: [(block value) hardline]) values);
  };

  glsl = let
    inherit (doc) hardline nest group line;

    binary = {
      "*" = 12;
      "/" = 12;
      "%" = 12;
      "+" = 11;
      "-" = 11;
      "<<" = 10;
      ">>" = 10;
      "<" = 9;
      ">" = 9;
      "<=" = 9;
      ">=" = 9;
      "==" = 8;
      "!=" = 8;
      "&" = 7;
      "^" = 6;
      "|" = 5;
      "&&" = 4;
      "^^" = 3;
      "||" = 2;
    };

    scalars = ["bool" "int" "uint" "float"];

    types =
      scalars
      ++ lib.concatMap (n: ["vec${n}" "ivec${n}" "uvec${n}" "bvec${n}" "mat${n}"] ++ map (m: "mat${n}x${m}") ["2" "3" "4"]) ["2" "3" "4"]
      ++ ["sampler2D" "void"];

    builtinFunctions = {
      same = {
        "1" = ["abs" "sign" "floor" "ceil" "fract" "round" "trunc" "sin" "cos" "tan" "asin" "acos" "atan" "exp" "log" "exp2" "log2" "sqrt" "inversesqrt" "normalize" "radians" "degrees" "fwidth" "dFdx" "dFdy"];
        "2" = ["min" "max" "mod" "pow" "reflect" "atan2"];
        "3" = ["clamp" "mix"];
      };
      float = {
        "1" = ["length"];
        "2" = ["distance" "dot"];
      };
    };

    prefixes = {
      float = "";
      int = "i";
      uint = "u";
      bool = "b";
    };

    palettes = [["x" "y" "z" "w"] ["r" "g" "b" "a"] ["s" "t" "p" "q"]];

    selectors = let
      words = letters: let
        grow = names: lib.concatMap (name: map (letter: name + letter) letters) names;
      in
        lib.concatLists (builtins.genList (n: lib.foldl' (names: _: grow names) [""] (lib.range 1 (n + 1))) 4);
    in
      builtins.genList (size: lib.concatMap (palette: words (lib.take size palette)) palettes) 5;

    vectorParts = type: builtins.match "([biu]?)vec([234])" type;

    dimension = type:
      if builtins.elem type scalars
      then 1
      else if vectorParts type != null
      then lib.toInt (builtins.elemAt (vectorParts type) 1)
      else null;

    scalarOf = type:
      if builtins.elem type scalars
      then type
      else if vectorParts type != null
      then lib.findFirst (scalar: prefixes.${scalar} == builtins.head (vectorParts type)) null scalars
      else if builtins.match "mat.*" type != null
      then "float"
      else null;

    vectorOf = scalar: size:
      if size == 1
      then scalar
      else "${prefixes.${scalar}}vec${toString size}";

    identifier = value:
      if builtins.match "[A-Za-z_][A-Za-z0-9_]*" value != null
      then value
      else throw "glsl: `${value}` is not a name";

    pointed = mantissa:
      if lib.hasInfix "." mantissa
      then mantissa
      else "${mantissa}.0";

    float = value: let
      text = builtins.toJSON value;
      parts = builtins.match "(-?[0-9.]+)e[+]?(-?)0*([0-9]+)" text;
    in
      if parts == null
      then text
      else "${pointed (builtins.head parts)}e${builtins.elemAt parts 1}${builtins.elemAt parts 2}";

    located = value: let
      self =
        value
        // lib.genAttrs (builtins.elemAt selectors (
          if value.type == null
          then 4
          else lib.defaultTo 0 (lib.mapNullable (parts: lib.toInt (builtins.elemAt parts 1)) (vectorParts value.type))
        )) (swizzle self);
    in
      self;

    swizzle = value: name:
      if value.type == null
      then throw "glsl: `.${name}` of a value whose type is not known"
      else
        located {
          ast.field = {
            base = value.ast;
            inherit name;
          };
          type = vectorOf (scalarOf value.type) (builtins.stringLength name);
        };

    comparisons = ["<" ">" "<=" ">=" "==" "!=" "&&" "||" "^^"];

    integers = ["int" "uint"];

    numbers = integers ++ ["float"];

    within = kinds: type: builtins.elem (scalarOf type) kinds;

    shape = type:
      lib.mapNullable (parts: {
        columns = lib.toInt (builtins.head parts);
        rows = lib.toInt (lib.defaultTo (builtins.head parts) (builtins.elemAt parts 2));
      }) (builtins.match "mat([234])(x([234]))?" type);

    vectorial = kinds: type: within kinds type && shape type == null;

    matrixOf = width: height:
      if width == height
      then "mat${toString width}"
      else "mat${toString width}x${toString height}";

    oriented = product:
      if product.columns == 1
      then vectorOf "float" product.rows
      else if product.rows == 1
      then vectorOf "float" product.columns
      else matrixOf product.columns product.rows;

    scalarWith = scalar: other: dimension scalar == 1 && scalarOf other == scalar;

    componentwise = left: right:
      if left == right
      then [left]
      else if scalarWith left right
      then [right]
      else lib.optional (scalarWith right left) left;

    linear = left: right: let
      leftShape = lib.defaultTo {
        columns = dimension left;
        rows = 1;
      } (shape left);
      rightShape = lib.defaultTo {
        columns = 1;
        rows = dimension right;
      } (shape right);
    in
      lib.optional (lib.all (type: scalarOf type == "float") [left right] && leftShape.columns == rightShape.rows) (oriented {
        inherit (rightShape) columns;
        inherit (leftShape) rows;
      });

    algebraic = left: right: lib.all (type: within numbers type && dimension type != 1) [left right] && lib.any (type: shape type != null) [left right];

    operations = let
      arithmetic = left: right: lib.optionals (within numbers left && within numbers right) (componentwise left right);
      integral = left: right: lib.optionals (vectorial integers left && vectorial integers right) (componentwise left right);
      shift = left: right: lib.optional (vectorial integers left && vectorial integers right && (dimension right == 1 || dimension right == dimension left)) left;
      relational = left: right: lib.optional (left == right && builtins.elem left numbers) "bool";
      equality = left: right: lib.optional (left == right) "bool";
      logical = left: right: lib.optional (left == "bool" && right == "bool") "bool";
    in {
      "*" = left: right:
        if algebraic left right
        then linear left right
        else arithmetic left right;
      "/" = arithmetic;
      "+" = arithmetic;
      "-" = arithmetic;
      "%" = integral;
      "&" = integral;
      "^" = integral;
      "|" = integral;
      "<<" = shift;
      ">>" = shift;
      "<" = relational;
      ">" = relational;
      "<=" = relational;
      ">=" = relational;
      "==" = equality;
      "!=" = equality;
      "&&" = logical;
      "||" = logical;
      "^^" = logical;
    };

    unknownResult = operator:
      if builtins.elem operator comparisons
      then "bool"
      else null;

    combine = operator: left: right:
      if left == null || right == null
      then unknownResult operator
      else lib.findFirst (_: true) (throw "glsl: ${left} ${operator} ${right} is not a GLSL operation") (operations.${operator} left right);

    typed' = type: value: {
      ast = value;
      inherit type;
    };

    termForms = {
      inherit (lib.genAttrs ["int" "float" "bool"] typed') int float bool;
      list = infix;
      set = value:
        if value ? ast
        then value
        else if value ? glslType
        then throw "glsl: the type ${value.glslType.name} is not a value"
        else throw "glsl: an attribute set that is not a GLSL value";
    };

    term = value: (termForms.${builtins.typeOf value} or (_: throw "glsl: ${builtins.typeOf value} `${toString value}` is not a GLSL value; a name is a bound value")) value;

    ast = value: (term value).ast;

    infix = let
      typedTree = parsed:
        if parsed ? binary
        then let
          left = typedTree parsed.binary.left;
          right = typedTree parsed.binary.right;
        in
          located {
            ast.binary = {
              inherit (parsed.binary) operator;
              left = left.ast;
              right = right.ast;
            };
            type = combine parsed.binary.operator left.type right.type;
          }
        else parsed;
    in
      items:
        typedTree (infixTree {
            language = "glsl";
            precedence = binary;
            operand = term;
          }
          items);

    curried = count: finish: let
      gather = collected:
        if builtins.length collected == count
        then finish collected
        else argument: gather (collected ++ [argument]);
    in
      gather [];

    invoke = function: type: arguments:
      located {
        ast.call = {
          inherit function;
          arguments = map ast arguments;
        };
        inherit type;
      };

    builtin = name: count: typing:
      curried count (arguments:
        invoke (
          if name == "atan2"
          then "atan"
          else name
        ) (typing arguments)
        arguments);

    construct = name: arguments:
      located {
        ast.call = {
          function = name;
          arguments = map ast arguments;
        };
        type = name;
        __functor = _: argument: construct name (arguments ++ [argument]);
      };

    typeValue = name: {
      glslType = {
        inherit name;
        qualifiers = [];
        size = null;
      };
      __functor = _: argument: construct name [argument];
    };

    typeName = type:
      if type.size == null
      then type.name
      else "${type.name}[${toString type.size}]";

    element = type: let
      sized = builtins.match "(.*)\\[[0-9]+]" type;
    in
      if type == null
      then null
      else if sized != null
      then builtins.head sized
      else if shape type != null
      then vectorOf "float" (shape type).rows
      else scalarOf type;

    reference = type: name: extra:
      located {
        ast.ref = {inherit name;} // extra;
        inherit type;
      };

    qualified = qualifier: type:
      type
      // {
        glslType = type.glslType // {qualifiers = [qualifier] ++ type.glslType.qualifiers;};
      };

    unary = operator: accepts: operand: let
      inherit (term operand) type;
    in {
      ast.unary = {
        inherit operator;
        operand = ast operand;
      };
      type =
        if type == null || accepts type
        then type
        else throw "glsl: ${operator} of a ${type} is not a GLSL operation";
    };

    postfix = operator: operand: let
      inherit (term operand) type;
    in {
      ast.postfix = {
        inherit operator;
        operand = ast operand;
      };
      type =
        if type == null || within numbers type
        then type
        else throw "glsl: ${operator} of a ${type} is not a GLSL operation";
    };

    condition = value: let
      inherit (term value) type;
    in
      if type == null || type == "bool"
      then ast value
      else throw "glsl: a ${type} condition";

    declaredIn = lib.concatMap (one: lib.optional (builtins.isAttrs one && one ? var) one.var);

    scoped = given: function: listed: let
      names = builtins.attrNames (builtins.functionArgs function);
      placeholder = name: {
        ast.ref = {inherit name;};
        type = null;
      };
      declaredHere = declaredIn (listed (function (lib.genAttrs names (name: given.${name} or (placeholder name)))));
      declared = name: typeName (lib.findFirst (one: one.name == name) (throw "glsl: `${name}` is bound but not declared in its block") declaredHere).type;
    in
      function (lib.genAttrs names (name: given.${name} or (reference (declared name) name {})));

    block = body:
      if builtins.isFunction body
      then scoped {} body lib.id
      else body;

    statementOf = one:
      if one ? ast
      then {expression = one.ast;}
      else one;

    statements = body: map statementOf (block body);

    checked = target: value: let
      wanted = (term target).type;
      given = (term value).type;
    in
      if wanted != null && given != null && wanted != given
      then throw "glsl: a ${given} value for a ${wanted}"
      else ast value;

    atom = text: {
      precedence = 15;
      doc = text;
    };

    signed = value: text: {
      precedence =
        if value < 0
        then 13
        else 15;
      doc = text;
    };

    paren = least: printed:
      if printed.precedence < least
      then ["(" printed.doc ")"]
      else printed.doc;

    kind = value:
      if builtins.isAttrs value
      then builtins.head (builtins.attrNames value)
      else builtins.typeOf value;

    argumentList = values: ["(" (lib.intersperse ", " (map (value: paren 1 (expression value)) values)) ")"];

    rows = values: ["(" (nest 4 (lib.intersperse "," (map (value: [hardline (paren 1 (expression value))]) values))) hardline ")"];

    negative = value: (builtins.isInt value || builtins.isFloat value) && value < 0 || kind value == "unary";

    forms = {
      int = value: signed value (toString value);
      float = value: signed value (float value);
      bool = value:
        atom (
          if value
          then "true"
          else "false"
        );
      ref = value:
        atom (
          if value.ref.macro or false
          then "(${identifier value.ref.name})"
          else identifier value.ref.name
        );
      binary = value: let
        precedence = binary.${value.binary.operator};
      in {
        inherit precedence;
        doc = group [(paren precedence (expression value.binary.left)) (nest 4 [line value.binary.operator " " (paren (precedence + 1) (expression value.binary.right))])];
      };
      unary = value: {
        precedence = 13;
        doc = [
          value.unary.operator
          (paren (
            if negative value.unary.operand
            then 14
            else 13
          ) (expression value.unary.operand))
        ];
      };
      postfix = value: {
        precedence = 14;
        doc = [(paren 14 (expression value.postfix.operand)) value.postfix.operator];
      };
      call = value: {
        precedence = 14;
        doc = [(identifier value.call.function) (argumentList value.call.arguments)];
      };
      array = value: {
        precedence = 14;
        doc = [(identifier value.array.type) "[${toString (builtins.length value.array.elements)}]" (rows value.array.elements)];
      };
      index = value: {
        precedence = 14;
        doc = [(paren 14 (expression value.index.base)) "[" (paren 0 (expression value.index.at)) "]"];
      };
      field = value: {
        precedence = 14;
        doc = [(paren 14 (expression value.field.base)) "." value.field.name];
      };
      cond = value: {
        precedence = 1;
        doc = [(paren 2 (expression value.cond.test)) " ? " (paren 1 (expression value.cond.yes)) " : " (paren 1 (expression value.cond.no))];
      };
      unsigned = value: atom "${toString value.unsigned}u";
    };

    expression = value: (forms.${kind value} or (throw "glsl: no expression ${kind value}")) value;

    written = value: (expression value).doc;

    typeDoc = type: lib.concatStringsSep " " (type.qualifiers ++ [(identifier type.name)]);

    declarator = type: target: [(typeDoc type) " " (identifier target) (lib.optionalString (type.size != null) "[${toString type.size}]")];

    simple = value: !builtins.elem (kind value) ["if" "for" "while" "comment"];

    braced = body: ["{" (nest 4 (map (one: [hardline (statement one)]) body)) hardline "}"];

    single = body: builtins.length body == 1 && simple (builtins.head body);

    clause = body:
      if single body
      then [" " (statement (builtins.head body))]
      else [" " (braced body)];

    alternative = body: otherwise: [
      (
        if single body
        then hardline
        else " "
      )
      "else"
      (
        if builtins.isAttrs otherwise
        then [" " (statement otherwise)]
        else clause otherwise
      )
    ];

    header = value:
      if value == null
      then ""
      else if kind value == "var"
      then [(declarator value.var.type value.var.name) (lib.optionals (value.var.value != null) [" = " (written value.var.value)])]
      else statementBody value;

    statementBody = value:
      {
        assign = [(written value.assign.target) " = " (written value.assign.value)];
        update = [(written value.update.target) " ${value.update.operator} " (written value.update.value)];
        expression = written value.expression;
      }
      .${
        kind value
      };

    statementForms = {
      comment = value: "// ${value.comment}";
      var = value: [(header value) ";"];
      return = value:
        if value.return == null
        then "return;"
        else ["return " (written value.return) ";"];
      jump = value: "${value.jump};";
      "if" = value: ["if (" (written value."if".test) ")" (clause value."if".body) (lib.optional (value."if" ? otherwise) (alternative value."if".body value."if".otherwise))];
      for = value: ["for (" (lib.intersperse "; " [(header value.for.init) (written value.for.test) (statementBody value.for.step)]) ") " (braced value.for.body)];
      while = value: ["while (" (written value.while.test) ") " (braced value.while.body)];
    };

    statement = value: (statementForms.${kind value} or (one: [(statementBody one) ";"])) value;

    noted = value: print:
      if value ? commented
      then ["// ${value.commented.text}" hardline (print value.commented.declaration)]
      else print value;

    bare = value:
      if value ? commented
      then bare value.commented.declaration
      else value;

    globalForms = {
      list = lib.concatMap globals;
      set = value: lib.optional (value ? ref) value.ref.name ++ lib.optional (value ? call) value.call.function ++ lib.concatMap globals (builtins.attrValues value);
    };

    globals = value: (globalForms.${builtins.typeOf value} or (_: [])) value;

    ordered = dependencies: let
      go = done: let
        left = builtins.filter (one: !builtins.elem one done) (builtins.attrNames dependencies);
        next = lib.findFirst (one: lib.all (other: builtins.elem other done) dependencies.${one}) null left;
      in
        if left == []
        then done
        else if next == null
        then throw "glsl: ${lib.concatStringsSep ", " left} depend on each other"
        else go (done ++ [next]);
    in
      go [];

    among = set: key: found: builtins.filter (one: one != key && set ? ${one}) (lib.unique found);

    constantValue = value: let
      inner = bare value;
    in
      inner.typed.value or inner;

    parsedType = text: let
      sized = builtins.match "(.*)\\[([0-9]+)]" text;
    in {
      name = builtins.head (lib.defaultTo [text] sized);
      qualifiers = [];
      size = lib.mapNullable (parts: lib.toInt (builtins.elemAt parts 1)) sized;
    };

    constantType = value: let
      inner = bare value;
      inferred = (term (constantValue value)).type;
      declaredType = inner.typed.type.glslType;
    in
      if inner ? typed
      then lib.throwIf (inferred != null && inferred != typeName declaredType) "glsl: a ${inferred} constant typed ${typeName declaredType}" declaredType
      else parsedType (lib.throwIf (inferred == null) "glsl: a constant's type is its value's; write `typed type value` for this one" inferred);

    constant = key: value: ["const " (declarator (constantType value) key) " = " (written (ast (constantValue value))) ";"];

    macroBody = value:
      if value ? macro
      then value.macro.function (lib.genAttrs value.macro.params (name: reference null name {macro = true;}))
      else value;

    number = value: builtins.isInt value || builtins.isFloat value;

    define = key: value: let
      body = macroBody value;
    in [
      "#define ${identifier key}"
      (lib.optionalString (value ? macro) "(${lib.concatMapStringsSep ", " identifier value.macro.params})")
      " "
      (paren (
        if number body
        then 0
        else 14
      ) (expression (ast body)))
    ];

    definition = key: value: [
      (typeDoc value.fn.returns)
      " ${identifier key}("
      (lib.intersperse ", " (map (param: [(typeDoc param.type) " " (identifier param.name)]) value.fn.params))
      ") "
      (braced value.fn.body)
    ];

    globalValue = key: value: let
      inner = bare value;
    in
      if inner ? fn
      then curried (builtins.length inner.fn.params) (invoke key (typeName inner.fn.returns))
      else if inner ? macro
      then curried (builtins.length inner.macro.params) (invoke key null)
      else reference (typeName (constantType value)) key {};

    defineValue = key: value:
      if value ? macro
      then curried (builtins.length value.macro.params) (invoke key null)
      else reference (term value).type key {};

    rgbPart = text: index: (lib.fromHexString (builtins.substring (1 + 2 * index) 2 text) * 2000 + 255) / 510 / 1000.0;

    typeValues = lib.genAttrs types typeValue;
  in
    typeValues
    // lib.concatMapAttrs (typing:
      lib.concatMapAttrs (count: names:
        lib.genAttrs names (name:
          builtin name (lib.toInt count) (
            if typing == "same"
            then arguments: (term (builtins.head arguments)).type
            else _: "float"
          ))))
    builtinFunctions
    // {
      step = builtin "step" 2 (arguments: (term (builtins.elemAt arguments 1)).type);
      smoothstep = builtin "smoothstep" 3 (arguments: (term (builtins.elemAt arguments 2)).type);
      cross = builtin "cross" 2 (_: "vec3");
      texture = builtin "texture" 2 (_: "vec4");

      iResolution = reference "vec3" "iResolution" {};
      iTime = reference "float" "iTime" {};
      iFrame = reference "int" "iFrame" {};
      iMouse = reference "vec4" "iMouse" {};
      iChannel0 = reference "sampler2D" "iChannel0" {};

      math = {
        pi = 3.141592653589793;

        round = digits: value: let
          scale = lib.foldl' (built: _: built * 10.0) 1.0 (lib.range 1 digits);
        in
          builtins.floor (value * scale + 0.5) / scale;

        sqrt = value:
          if value == 0
          then 0.0
          else lib.foldl' (guess: _: (guess + value / guess) / 2.0) (value * 1.0) (lib.range 1 64);
      };

      rgb = text: let
        parts = map (rgbPart text) [0 1 2];
      in
        if lib.all (part: part == builtins.head parts) parts
        then typeValues.vec3 (builtins.head parts)
        else lib.foldl' lib.id typeValues.vec3 parts;

      array = type: elements: {
        ast.array = {
          type = type.glslType.name;
          elements = map ast elements;
        };
        type = "${type.glslType.name}[${toString (builtins.length elements)}]";
      };

      index = base: at:
        located {
          ast.index = {
            base = ast base;
            at = ast at;
          };
          type = element (term base).type;
        };

      neg = operand: located (unary "-" (within numbers) operand);

      not = operand: located (unary "!" (type: type == "bool") operand);

      bitNot = operand: located (unary "~" (vectorial integers) operand);

      preInc = unary "++" (within numbers);

      preDec = unary "--" (within numbers);

      inc = postfix "++";

      dec = postfix "--";

      cond = test: yes: no: let
        arms = map (value: (term value).type) [yes no];
        known = builtins.filter (type: type != null) arms;
      in
        located {
          ast.cond = {
            test = condition test;
            yes = ast yes;
            no = ast no;
          };
          type =
            if builtins.length (lib.unique known) < 2
            then lib.findFirst (_: true) null known
            else throw "glsl: a choice between a ${builtins.head arms} and a ${lib.last arms}";
        };

      unsigned = value: {
        ast.unsigned = value;
        type = "uint";
      };

      sized = type: size: type // {glslType = type.glslType // {inherit size;};};

      out = qualified "out";

      in' = qualified "in";

      inout = qualified "inout";

      typed = type: value: {typed = {inherit type value;};};

      macro = function: {
        macro = {
          inherit function;
          params = builtins.attrNames (builtins.functionArgs function);
        };
      };

      local = target: type: value: {
        var = {
          inherit (target.ast.ref) name;
          type = type.glslType;
          value =
            if value == null
            then null
            else if (term value).type != null && (term value).type != typeName type.glslType
            then throw "glsl: ${target.ast.ref.name} is a ${typeName type.glslType} but its value is a ${(term value).type}"
            else ast value;
        };
      };

      set = target: value: {
        assign = {
          target = ast target;
          value = checked target value;
        };
      };

      update = target: operator: value: let
        wanted = (term target).type;
        result = combine (lib.removeSuffix "=" operator) wanted (term value).type;
      in {
        update = {
          inherit operator;
          target = ast target;
          value =
            if result != null && result != wanted
            then throw "glsl: ${wanted} ${operator} a ${(term value).type} is not a GLSL operation"
            else ast value;
        };
      };

      return = value: {
        return =
          if value == null
          then null
          else ast value;
      };

      break = {jump = "break";};

      continue = {jump = "continue";};

      if' = test: body: {
        "if" = {
          test = condition test;
          body = statements body;
        };
      };

      ifElse = test: body: otherwise: {
        "if" = {
          test = condition test;
          body = statements body;
          otherwise =
            if builtins.isAttrs otherwise
            then otherwise
            else statements otherwise;
        };
      };

      for = loop: let
        spec =
          if builtins.isFunction loop
          then scoped {} loop (result: lib.optional (result.init != null) result.init ++ block result.body)
          else loop;
      in {
        for = {
          inherit (spec) init;
          test = condition spec.test;
          step = statementOf spec.step;
          body = statements spec.body;
        };
      };

      while = test: body: {
        while = {
          test = condition test;
          body = statements body;
        };
      };

      comment = text: {comment = text;};

      commented = text: subject: {
        commented = {
          inherit text;
          declaration = subject;
        };
      };

      fn = returns: params: body: let
        named =
          map (param: let
            name = builtins.head (builtins.attrNames param);
          in {
            inherit name;
            type = param.${name}.glslType;
          })
          params;
        given = builtins.listToAttrs (map (param: {
            inherit (param) name;
            value = reference (typeName param.type) param.name {};
          })
          named);
      in {
        fn = {
          returns = returns.glslType;
          params = named;
          body = map statementOf (
            if builtins.isFunction body
            then scoped given body lib.id
            else body
          );
        };
      };

      render = shader: let
        result = shader self;
        inherit (lib.genAttrs ["defines" "consts" "fns"] (name: result.${name} or {})) defines consts fns;
        self = builtins.mapAttrs defineValue defines // builtins.mapAttrs globalValue consts // builtins.mapAttrs globalValue fns;
        constOrder = ordered (builtins.mapAttrs (key: value: among consts key (globals (ast (constantValue value)))) consts);
        fnOrder = ordered (builtins.mapAttrs (key: value: among fns key (globals (bare value).fn.body)) fns);
        sections = builtins.filter (section: section != []) [
          (lib.optional (defines != {}) (lib.intersperse hardline (lib.mapAttrsToList (key: value: noted value (define key)) defines)))
          (lib.optional (consts != {}) (lib.intersperse hardline (map (key: noted consts.${key} (constant key)) constOrder)))
          (map (key: noted fns.${key} (definition key)) fnOrder)
        ];
      in
        doc.render 100 [(lib.intersperse [hardline hardline] (lib.concatLists sections)) hardline];
    };
  lua = let
    inherit (doc) hardline nest;

    precedence = {
      "or" = 1;
      "and" = 2;
      "<" = 3;
      ">" = 3;
      "<=" = 3;
      ">=" = 3;
      "~=" = 3;
      "==" = 3;
      "|" = 4;
      "~" = 5;
      "&" = 6;
      "<<" = 7;
      ">>" = 7;
      ".." = 8;
      "+" = 9;
      "-" = 9;
      "*" = 10;
      "/" = 10;
      "//" = 10;
      "%" = 10;
      "^" = 12;
    };

    unaryPrecedence = 11;

    parse = infixTree {
      language = "lua";
      inherit precedence;
      right = [".." "^"];
      operand = lib.id;
    };

    nixKeywords = ["assert" "else" "false" "if" "in" "inherit" "let" "null" "or" "rec" "then" "true" "with"];

    globals = ["CS" "Vector3" "_G" "_VERSION" "assert" "bit" "collectgarbage" "coroutine" "debug" "dofile" "error" "getfenv" "getmetatable" "hl" "io" "ipairs" "jit" "load" "loadfile" "loadstring" "math" "next" "os" "package" "pairs" "pcall" "pg" "print" "rawequal" "rawget" "rawlen" "rawset" "require" "select" "setfenv" "setmetatable" "string" "table" "tonumber" "tostring" "type" "unpack" "utf8" "xpcall"];

    identifier = name:
      if builtins.match "[A-Za-z_][A-Za-z0-9_]*" name != null
      then name
      else throw "lua: `${name}` is not a name";

    fieldPath = text:
      if builtins.match "[A-Za-z_][A-Za-z0-9_]*([.][A-Za-z_][A-Za-z0-9_]*)*" text != null
      then text
      else throw "lua: `${text}` is not a field path";

    binder = value:
      if kindOf value == "name"
      then identifier value.name
      else if kindOf value == "vararg"
      then "..."
      else throw "lua: a declared name is a bound variable, not a ${kindOf value}";

    nameList = values: lib.concatStringsSep ", " (map binder (lib.toList values));

    infix = value: builtins.isList value && builtins.length value >= 3 && builtins.isString (builtins.elemAt value 1);

    unaries = {
      not = "not ";
      neg = "-";
      len = "#";
      bnot = "~";
    };

    operatorRank = items: precedence.${(parse items).binary.operator};

    signedRank = value:
      if value < 0
      then unaryPrecedence
      else 13;

    ranks =
      {
        list = value:
          if infix value
          then operatorRank value
          else 13;
        rows = value: operatorRank (lib.concatLists value.rows);
        int = signedRank;
        float = signedRank;
      }
      // lib.mapAttrs (_: _: _: unaryPrecedence) unaries;

    rank = value: (ranks.${kindOf value} or (_: 13)) value;

    powerBase = items: index: index + 1 < builtins.length items && builtins.elemAt items (index + 1) == "^" && rank (builtins.elemAt items index) == unaryPrecedence;

    term = items: index: let
      item = builtins.elemAt items index;
    in
      if lib.mod index 2 == 1
      then item
      else if infix item
      then ["(" (infixDoc item) ")"]
      else if powerBase items index
      then ["(" (expression item) ")"]
      else expression item;

    infixDoc = items: builtins.seq (parse items).binary.operator (lib.intersperse " " (builtins.genList (term items) (builtins.length items)));

    rowsDoc = rowList: let
      items = lib.concatLists rowList;
      offsets = builtins.foldl' (sums: one: sums ++ [(lib.last sums + builtins.length one)]) [0] rowList;
      row = index: lib.intersperse " " (builtins.genList (at: term items (builtins.elemAt offsets index + at)) (builtins.length (builtins.elemAt rowList index)));
      broken = lib.all (one: builtins.isString (builtins.head one)) (builtins.tail rowList);
    in
      if !broken
      then throw "lua: each row of an infix expression after the first starts with its operator"
      else builtins.seq (parse items).binary.operator [(row 0) (builtins.genList (index: nest 2 [hardline (row (index + 1))]) (builtins.length rowList - 1))];

    commas = values: lib.intersperse ", " (map expression values);

    prefixKinds = ["name" "dot" "index" "paren" "apply"];

    prefix = value:
      if builtins.elem (kindOf value) prefixKinds || builtins.isList value && !infix value
      then expression value
      else ["(" (expression value) ")"];

    callee = head:
      if kindOf head == "method"
      then [(prefix head.method.table) ":" (identifier head.method.name)]
      else prefix head;

    call = value: [(callee (builtins.head value)) "(" (commas (builtins.tail value)) ")"];

    fieldForms = {
      field = value: [(identifier value.field.name) " = " (expression value.field.value)];
      key = value: ["[" (expression value.key.key) "] = " (expression value.key.value)];
    };

    tableField = value: (fieldForms.${kindOf value} or expression) value;

    fieldRow = row: [hardline (lib.intersperse ", " (map tableField (lib.toList row))) ","];

    braces = fields:
      if fields ? rows
      then ["{" (nest 2 (map fieldRow fields.rows)) hardline "}"]
      else if fields == []
      then "{}"
      else ["{ " (lib.intersperse ", " (map tableField fields)) " }"];

    declarers = {
      local = one: lib.catAttrs "name" (lib.toList one.local.names);
      localFunction = one: [one.localFunction.name.name];
    };

    declaredIn = one: (declarers.${kindOf one} or (_: [])) one;

    statementsOf = value: value.inline or value;

    binders = header: builtins.filter builtins.isString (map (one: one.name or null) (header.params or header.names or [header.name]));

    declared = result:
      if result ? body
      then binders result ++ lib.concatMap declaredIn (statementsOf result.body)
      else lib.concatMap declaredIn (statementsOf result);

    bound = boundBlock {
      language = "lua";
      name = spelled: {name = spelled;};
      inherit declared;
    };

    block = value:
      if value ? inline
      then {inline = bound value.inline;}
      else bound value;

    functionSpec = value: let
      result = block value;
    in
      if result ? body
      then result
      else {
        params = [];
        body = result;
      };

    sequence = values: lib.imap0 (index: value: [(lib.optionalString (index > 0 && kindOf value != "semicolon") " ") (statement value)]) values;

    layout = value:
      if value ? inline
      then [" " (sequence value.inline) (lib.optionalString (value.inline != []) " ")]
      else if builtins.isList value
      then [(nest 2 (map (one: [hardline (entry one)]) value)) hardline]
      else throw "lua: a block is a list of statements, `inline [ ... ]` or a lambda of the names it declares";

    function = head: value: let
      resolved = functionSpec value;
    in [head "(" (nameList resolved.params) ")" (layout resolved.body) "end"];

    expressions =
      {
        name = value: identifier value.name;
        vararg = _: "...";
        nil = _: "nil";
        int = toString;
        float = builtins.toJSON;
        bool = lib.boolToString;
        string = value: throw "lua: the bare string \"${value}\" is not an expression (a literal is `str`, a name a bound variable)";
        list = value:
          if infix value
          then infixDoc value
          else call value;
        rows = value: rowsDoc value.rows;
        str = value: "\"${value.str}\"";
        sq = value: "'${value.sq}'";
        table = value: braces value.table;
        fn = value: function "function" value.fn;
        apply = value: [(callee value.apply.head) (expression value.apply.arg)];
        paren = value:
          if builtins.elem (kindOf value.paren) ["vararg" "apply"] || builtins.isList value.paren && !infix value.paren
          then ["(" (expression value.paren) ")"]
          else throw "lua: `paren` keeps the first value of a call or `...`; a grouping is a nested infix list";
        dot = value: [(prefix value.dot.table) "." (fieldPath value.dot.path)];
        index = value: [(prefix value.index.table) "[" (expression value.index.key) "]"];
      }
      // lib.mapAttrs (kind: mark: value: [
        mark
        (
          if rank value.${kind} < unaryPrecedence
          then ["(" (expression value.${kind}) ")"]
          else expression value.${kind}
        )
      ])
      unaries;

    expression = value: (expressions.${kindOf value} or (throw "lua: a ${kindOf value} is not an expression")) value;

    assigned = values: lib.optionals (values != []) [" = " (commas values)];

    clause = value: [
      (expression value.test)
      " then"
      (layout (block value.body))
      (
        if !(value ? otherwise)
        then "end"
        else if value.otherwise ? "if"
        then ["elseif " (clause value.otherwise."if")]
        else ["else" (layout (block value.otherwise)) "end"]
      )
    ];

    loop = head: body: [head " do" (layout body) "end"];

    statements = {
      list = value:
        if infix value
        then throw "lua: an infix expression is not a statement"
        else call value;
      comment = value: value.comment;
      break = _: "break";
      semicolon = _: ";";
      local = value: ["local " (nameList value.local.names) (assigned value.local.values)];
      assign = value: [(commas (lib.toList value.assign.targets)) (assigned value.assign.values)];
      localFunction = value: function "local function ${binder value.localFunction.name}" value.localFunction.spec;
      return = value: ["return" (lib.optionals (value.return != []) [" " (commas value.return)])];
      "if" = value: ["if " (clause value."if")];
      forIn = value: let
        resolved = block value.forIn;
      in
        loop ["for " (nameList resolved.names) " in " (commas resolved.values)] resolved.body;
      forNum = value: let
        resolved = block value.forNum;
      in
        loop ["for " (binder resolved.name) " = " (commas resolved.range)] resolved.body;
      while = value: loop ["while " (expression value.while.test)] (block value.while.body);
      do = value: ["do" (layout (block value.do)) "end"];
    };

    statement = value: (statements.${kindOf value} or (throw "lua: a ${kindOf value} is not a statement")) value;

    entry = value:
      if value ? blank
      then []
      else statement value;

    constructors = {
      inherit (lib.genAttrs ["blank" "break" "semicolon" "nil" "vararg"] (name: {${name} = true;})) blank break semicolon nil vararg;

      inherit (lib.genAttrs ["comment" "str" "sq"] (name: text: {${name} = text;})) comment str sq;

      inherit (lib.genAttrs ["inline" "rows" "return"] (name: values: {${name} = values;})) inline rows return;

      table = fields: {table = fields;};

      field = name: value: {field = {inherit name value;};};

      key = key: value: {key = {inherit key value;};};

      inherit (lib.genAttrs ["fn" "forIn" "forNum"] (name: spec: {${name} = spec;})) fn forIn forNum;

      apply = head: arg: {apply = {inherit head arg;};};

      inherit (lib.genAttrs ["paren" "not" "neg" "len" "bnot"] (name: value: {${name} = value;})) paren not neg len bnot;

      dot = table: path: {dot = {inherit table path;};};

      method = table: name: {method = {inherit table name;};};

      index = table: key: {index = {inherit table key;};};

      local = names: values: {local = {inherit names values;};};

      assign = targets: values: {assign = {inherit targets values;};};

      localFunction = name: spec: {localFunction = {inherit name spec;};};

      if' = test: body: {"if" = {inherit test body;};};

      ifElse = test: body: otherwise: {"if" = {inherit test body otherwise;};};

      while = test: body: {while = {inherit test body;};};

      do = body: {do = body;};

      render = top: doc.render 80 (map (value: [(entry value) hardline]) (block top));
    };
  in
    constructors
    // builtins.listToAttrs (map (global: {
        name =
          if constructors ? ${global} || builtins.elem global nixKeywords
          then "${global}'"
          else global;
        value = {name = global;};
      })
      globals);

  python = let
    inherit (doc) hardline nest align;

    nameNode = spelled: {name = spelled;};

    keyword = word: {keyword = word;};

    nameOf = value:
      if kindOf value == "name"
      then value.name
      else throw "python: a ${kindOf value} where a name is expected";

    levels = {
      "or" = 4;
      "and" = 5;
      "in" = 7;
      "not in" = 7;
      "is" = 7;
      "is not" = 7;
      "<" = 7;
      "<=" = 7;
      ">" = 7;
      ">=" = 7;
      "!=" = 7;
      "==" = 7;
      "|" = 8;
      "^" = 9;
      "&" = 10;
      "<<" = 11;
      ">>" = 11;
      "+" = 12;
      "-" = 12;
      "*" = 13;
      "/" = 13;
      "//" = 13;
      "%" = 13;
      "@" = 13;
      "**" = 15;
    };

    rowItems = row:
      if builtins.isList row
      then row
      else [row];

    contentItems = content:
      if builtins.isList content
      then content
      else lib.concatMap rowItems (content.rows or content.aligned or (throw "python: no bracket content ${kindOf content}"));

    marked = rows: let
      ends = builtins.foldl' (sums: row: sums ++ [(lib.last sums + builtins.length (rowItems row))]) [0] rows;
      breaks = lib.init (builtins.tail ends);
    in
      lib.imap0 (index: piece:
        if lib.mod index 2 == 0
        then piece
        else if builtins.isString piece
        then {
          symbol = piece;
          before = builtins.elem index breaks;
          after = builtins.elem (index + 1) breaks;
        }
        else throw "python: an infix list alternates operands and operator strings")
      (lib.concatMap rowItems rows);

    tree = pieces: let
      count = builtins.length pieces;
      operators = lib.genList (index: builtins.elemAt pieces (2 * index + 1)) (count / 2);
      ranks = map (operator: levels.${operator.symbol} or (throw "python: `${operator.symbol}` is not a binary operator")) operators;
      lowest = lib.foldl' lib.min 99 ranks;
      indices = builtins.filter (index: builtins.elemAt ranks index == lowest) (lib.range 0 (builtins.length operators - 1));
      root =
        if lowest == levels."**"
        then builtins.head indices
        else lib.last indices;
    in
      if count == 1
      then operand (builtins.head pieces)
      else if lib.mod count 2 == 0
      then throw "python: an infix list alternates operands and operators and ends in an operand"
      else {
        binary = {
          operator = builtins.elemAt operators root;
          left = tree (lib.take (2 * root + 1) pieces);
          right = tree (lib.drop (2 * root + 2) pieces);
        };
      };

    operand = value:
      if builtins.isList value
      then {group = tree (marked [value]);}
      else value;

    form = level: print: {
      inherit level;
      doc = print;
    };

    atom = text: form 17 (_: text);

    printed = value: (forms.${kindOf value} or (throw "python: ${kindOf value} is not a Python expression")) value;

    at = need: mode: value: let
      it = printed value;
    in
      if it.level < need
      then ["(" (align (it.doc inside)) ")"]
      else it.doc mode;

    top = mode: value:
      if kindOf value == "commas"
      then (printed value).doc mode
      else at 2 mode value;

    inside = 0;

    bare = 1;

    continued = 2;

    continuing = mode:
      if mode == bare
      then continued
      else mode;

    broken = mode: rest:
      if mode == inside
      then [hardline rest]
      else if mode == bare
      then [" \\" (nest 4 [hardline rest])]
      else [" \\" hardline rest];

    item = at 2 inside;

    separated = values: lib.intersperse ", " (map item values);

    contents = {
      list = open: close: tuple: values: [open (align (separated values)) (lib.optional (tuple && builtins.length values == 1) ",") close];
      rows = open: close: _: value: let
        comma = lib.optionalString (builtins.length value.rows > 1) ",";
      in [open (nest 4 (map (row: [hardline (separated (rowItems row)) comma]) value.rows)) hardline close];
      aligned = open: close: _: value: [open (align (lib.intersperse ["," hardline] (map (row: separated (rowItems row)) value.aligned))) close];
    };

    bracket = open: close: tuple: content: (contents.${kindOf content} or (throw "python: no bracket content ${kindOf content}")) open close tuple content;

    argumentsOf = values:
      if builtins.isList values && builtins.length values == 1 && kindOf (builtins.head values) == "gen"
      then ["(" (align (comprehension (builtins.head values).gen)) ")"]
      else bracket "(" ")" false values;

    fstringParts = {
      string = lib.id;
      field = value: ["{" (item value.field) "}"];
      spec = value: ["{" (item value.spec.value) ":" value.spec.format "}"];
    };

    literal = quote: content:
      atom (
        if builtins.isList content
        then ["f" quote (map (part: (fstringParts.${kindOf part} or (throw "python: no f-string part ${kindOf part}")) part) content) quote]
        else [quote content quote]
      );

    simple = value: value == null || builtins.isInt value || kindOf value == "name";

    bound = side: value: lib.optional (value != null) (side (item value));

    subscript = key:
      if kindOf key == "slice"
      then let
        inherit (key.slice) lower upper;
        pad = lib.optionalString (!(simple lower && simple upper)) " ";
      in [(bound (side: [side pad]) lower) ":" (bound (side: [pad side]) upper)]
      else top inside key;

    lines = mode: docs: [(builtins.head docs) (map (broken mode) (builtins.tail docs))];

    infix = value: printed (tree value);

    forms = {
      string = value: throw "python: the string \"${value}\" where an expression is expected (a name is a bound variable, a literal is `str`)";
      name = value: atom value.name;
      int = value:
        form (
          if value < 0
          then 14
          else 17
        ) (_: toString value);
      bool = value:
        atom (
          if value
          then "True"
          else "False"
        );
      octal = value: atom "0o${lib.concatMapStrings toString (lib.toBaseDigits 8 value.octal)}";
      placeholder = value: atom "@${value.placeholder}@";
      str = value: literal "\"" value.str;
      sq = value: literal "'" value.sq;
      triple = value: literal "\"\"\"" value.triple;
      raw = value: atom ["r" (item value.raw)];
      bytes = value: atom ["b" (item value.bytes)];
      strings = value:
        form 17 (mode:
          if builtins.isList value.strings
          then lib.intersperse " " (map item value.strings)
          else lines mode (map item value.strings.rows));
      list = value: infix (marked [value]);
      rows = value: infix (marked value.rows);
      aligned = value: atom ["(" (align ((infix (marked value.aligned)).doc inside)) ")"];
      hang = value: atom ["(" (nest 4 [hardline ((printed value.hang).doc inside)]) hardline ")"];
      binary = value: let
        inherit (value.binary) operator left right;
        level = levels.${operator.symbol};
        power = operator.symbol == "**";
        after = mode: rest:
          if operator.after
          then broken mode rest
          else [" " rest];
        right' = mode:
          at (
            if power
            then 14
            else level + 1
          ) (
            if operator.before || operator.after
            then continuing mode
            else mode
          )
          right;
      in
        form level (mode: [
          (at (
              if power
              then 16
              else level
            )
            mode
            left)
          (
            if operator.before
            then broken mode [operator.symbol (after (continuing mode) (right' mode))]
            else [" " operator.symbol (after mode (right' mode))]
          )
        ]);
      group = value: let
        inner = printed value.group;
      in {
        level =
          if kindOf value.group == "binary" && levels.${value.group.binary.operator.symbol} == levels."=="
          then 6.5
          else inner.level;
        inherit (inner) doc;
      };
      call = value: form 17 (mode: [(at 17 mode value.call.function) (argumentsOf value.call.arguments)]);
      dot = value: form 17 (mode: [(at 17 mode value.dot.value) "." value.dot.name]);
      index = value: form 17 (mode: [(at 17 mode value.index.value) "[" (align (subscript value.index.key)) "]"]);
      "list'" = value: atom (bracket "[" "]" false value."list'");
      tuple = value: atom (bracket "(" ")" true value.tuple);
      set = value: atom (bracket "{" "}" false value.set);
      dict = value: atom (bracket "{" "}" false value.dict);
      listComp = value: atom ["[" (align (comprehension value.listComp)) "]"];
      setComp = value: atom ["{" (align (comprehension value.setComp)) "}"];
      dictComp = value: atom ["{" (align (comprehension value.dictComp)) "}"];
      gen = value: atom ["(" (align (comprehension value.gen)) ")"];
      commas = value: form 0 (mode: lib.intersperse ", " (map (at 2 mode) value.commas));
      pair = value: atom [(item value.pair.key) ": " (item value.pair.value)];
      kw = value: atom [value.kw.name "=" (item value.kw.value)];
      default = value: atom [(nameOf value.default.name) "=" (item value.default.value)];
      star = value: atom ["*" (at 8 inside value.star)];
      starstar = value: atom ["**" (at 8 inside value.starstar)];
      neg = value: form 14 (mode: ["-" (at 14 mode value.neg)]);
      invert = value: form 14 (mode: ["~" (at 14 mode value.invert)]);
      not = value: form 6 (mode: ["not " (at 6 mode value.not)]);
      await = value: form 16 (mode: ["await " (at 17 mode value.await)]);
      lambda = value: let
        scope = lambdaScope value.lambda;
      in
        form 2 (mode: ["lambda" (lib.optionals (scope.params != []) [" " (separated scope.params)]) ": " (at 2 mode scope.body)]);
      ternary = value: let
        inherit (value.ternary) body test orelse;
      in
        form 3 (mode: [(at 4 mode body) " if " (at 4 mode test) " else " (at 3 mode orelse)]);
      walrus = value: form 1 (mode: [(nameOf value.walrus.target) " := " (at 2 mode value.walrus.value)]);
      as = value: atom [(item value.as.value) " as " (item value.as.target)];
    };

    comprehensionClauses = {
      compFor = value: ["for " (top inside value.compFor.target) " in " (at 4 inside value.compFor.iter)];
      compIf = value: ["if " (at 4 inside value.compIf)];
    };

    comprehensionPart = value: (comprehensionClauses.${kindOf value} or item) value;

    comprehension = body: let
      parts = comprehensionScope body;
    in
      if builtins.isList parts
      then lib.intersperse " " (map comprehensionPart parts)
      else lines inside (map (row: lib.intersperse " " (map comprehensionPart (rowItems row))) parts.rows);

    boundForms = {
      name = value: [value.name];
      commas = value: lib.concatMap boundNames value.commas;
      tuple = value: lib.concatMap boundNames (contentItems value.tuple);
      "list'" = value: lib.concatMap boundNames (contentItems value."list'");
      star = value: boundNames value.star;
    };

    boundNames = value: (boundForms.${kindOf value} or (_: [])) value;

    parameterForms = {
      default = value: [(nameOf value.default.name)];
      star = value: boundNames value.star;
      starstar = value: boundNames value.starstar;
    };

    parameter = value: (parameterForms.${kindOf value} or boundNames) value;

    walrusTarget = value: lib.optional (kindOf value == "walrus") (nameOf value.walrus.target);

    binders = {
      "import" = value: [(nameOf value."import")];
      from = value: map nameOf value.from.names;
      assign = value: boundNames value.assign.target;
      augAssign = value: boundNames value.augAssign.target;
      del = value: lib.concatMap boundNames value.del;
      def = value: [(nameOf value.def.name)];
      class = value: [(nameOf value.class.name)];
      async = value: declares value.async;
      "if" = value:
        walrusTarget value."if".test
        ++ declared value."if".body
        ++ lib.optionals (value."if" ? otherwise) (
          if builtins.isList value."if".otherwise
          then declared value."if".otherwise
          else declares value."if".otherwise
        );
      for = value: boundNames value.for.target ++ declared value.for.body;
      while = value: walrusTarget value.while.test ++ declared value.while.body;
      "with" = value: lib.concatMap (one: lib.optionals (kindOf one == "as") (boundNames one.as.target)) value."with".items ++ declared value."with".body;
      try = value: declared value.try.body ++ declared value.try.clauses;
      except = value: lib.optionals (kindOf value.except.caught == "as") (boundNames value.except.caught.as.target) ++ declared value.except.body;
      finally = value: declared value.finally;
    };

    declares = value: (binders.${kindOf value} or (_: [])) value;

    declared = lib.concatMap declares;

    scoped = declaring:
      boundBlock {
        language = "python";
        name = nameNode;
        declared = declaring;
      };

    blockScope = scoped declared;

    functionScope = scoped (result: lib.concatMap parameter (contentItems result.params) ++ declared result.body);

    lambdaScope = scoped (result: lib.concatMap parameter result.params);

    comprehensionScope = scoped (result: lib.concatMap (one: lib.optionals (kindOf one == "compFor") (boundNames one.compFor.target)) (contentItems result));

    block = body: nest 4 (map (value: [hardline (entry value)]) body);

    alternative = value:
      if builtins.isList value
      then [hardline "else:" (block value)]
      else if kindOf value == "if"
      then [hardline "el" (statement value)]
      else throw "python: an else branch is a body or an if";

    handlers = {
      except = value: ["except" (lib.optionals (value.except.caught != null) [" " (at 2 bare value.except.caught)]) ":" (block value.except.body)];
      finally = value: ["finally:" (block value.finally)];
    };

    statements = {
      keyword = value: value.keyword;
      comment = value: value.comment;
      "import" = value: ["import " (nameOf value."import")];
      from = value: ["from " value.from.module " import " (lib.concatMapStringsSep ", " nameOf value.from.names)];
      assign = value: [(top bare value.assign.target) " = " (top bare value.assign.value)];
      augAssign = value: [(top bare value.augAssign.target) " ${value.augAssign.operator} " (top bare value.augAssign.value)];
      return = value: ["return" (lib.optionals (value.return != null) [" " (top bare value.return)])];
      raise = value: ["raise" (lib.optionals (value.raise != null) [" " (at 2 bare value.raise)])];
      del = value: ["del " (lib.intersperse ", " (map (at 2 bare) value.del))];
      global = value: ["global " (lib.concatMapStringsSep ", " nameOf value.global)];
      nonlocal = value: ["nonlocal " (lib.concatMapStringsSep ", " nameOf value.nonlocal)];
      "assert" = value: ["assert " (at 2 bare value."assert")];
      def = value: let
        scope = functionScope value.def.function;
      in ["def " (nameOf value.def.name) (bracket "(" ")" false scope.params) ":" (block scope.body)];
      async = value: ["async " (statement value.async)];
      class = value: ["class " (nameOf value.class.name) (lib.optional (value.class.bases != null) (bracket "(" ")" false value.class.bases)) ":" (block (blockScope value.class.body))];
      "if" = value: ["if " (at 1 bare value."if".test) ":" (block value."if".body) (lib.optional (value."if" ? otherwise) (alternative value."if".otherwise))];
      for = value: ["for " (top bare value.for.target) " in " (top bare value.for.iter) ":" (block value.for.body)];
      while = value: ["while " (at 1 bare value.while.test) ":" (block value.while.body)];
      "with" = value: ["with " (lib.intersperse ", " (map (at 2 bare) value."with".items)) ":" (block value."with".body)];
      try = value: ["try:" (block value.try.body) (map (clause: [hardline ((handlers.${kindOf clause} or (throw "python: ${kindOf clause} is not a try clause")) clause)]) value.try.clauses)];
    };

    statement = value: (statements.${kindOf value} or (top bare)) value;

    entry = value:
      if value ? blank
      then []
      else statement value;

    builtinNames = [
      "None"
      "NotImplemented"
      "Ellipsis"
      "__name__"
      "__doc__"
      "__file__"
      "abs"
      "aiter"
      "all"
      "anext"
      "any"
      "ascii"
      "bin"
      "bool"
      "breakpoint"
      "bytearray"
      "bytes"
      "callable"
      "chr"
      "classmethod"
      "compile"
      "complex"
      "delattr"
      "dict"
      "dir"
      "divmod"
      "enumerate"
      "eval"
      "exec"
      "filter"
      "float"
      "format"
      "frozenset"
      "getattr"
      "globals"
      "hasattr"
      "hash"
      "help"
      "hex"
      "id"
      "input"
      "int"
      "isinstance"
      "issubclass"
      "iter"
      "len"
      "list"
      "locals"
      "map"
      "max"
      "memoryview"
      "min"
      "next"
      "object"
      "oct"
      "open"
      "ord"
      "pow"
      "print"
      "property"
      "range"
      "repr"
      "reversed"
      "round"
      "set"
      "setattr"
      "slice"
      "sorted"
      "staticmethod"
      "str"
      "sum"
      "super"
      "tuple"
      "type"
      "vars"
      "zip"
      "BaseException"
      "Exception"
      "ArithmeticError"
      "AssertionError"
      "AttributeError"
      "BlockingIOError"
      "BrokenPipeError"
      "BufferError"
      "ChildProcessError"
      "ConnectionAbortedError"
      "ConnectionError"
      "ConnectionRefusedError"
      "ConnectionResetError"
      "EOFError"
      "FileExistsError"
      "FileNotFoundError"
      "GeneratorExit"
      "ImportError"
      "IndexError"
      "InterruptedError"
      "IsADirectoryError"
      "KeyError"
      "KeyboardInterrupt"
      "LookupError"
      "MemoryError"
      "ModuleNotFoundError"
      "NameError"
      "NotADirectoryError"
      "NotImplementedError"
      "OSError"
      "OverflowError"
      "PermissionError"
      "ProcessLookupError"
      "RecursionError"
      "RuntimeError"
      "StopAsyncIteration"
      "StopIteration"
      "SyntaxError"
      "SystemExit"
      "TimeoutError"
      "TypeError"
      "UnicodeDecodeError"
      "UnicodeEncodeError"
      "UnicodeError"
      "ValueError"
      "ZeroDivisionError"
    ];

    constructors = {
      blank = {blank = true;};

      comment = text: {comment = text;};

      inherit (lib.genAttrs ["str" "sq" "triple"] (name: content: {${name} = content;})) str sq triple;

      inherit (lib.genAttrs ["raw" "bytes" "field" "octal" "hang" "star" "starstar" "neg" "invert" "not" "await" "return" "raise" "async"] (name: value: {${name} = value;})) raw bytes field octal hang star starstar neg invert not await return raise async;

      spec = value: format: {spec = {inherit value format;};};

      inherit (lib.genAttrs ["strings" "rows" "aligned" "tuple" "set" "dict" "commas"] (name: values: {${name} = values;})) strings rows aligned tuple set dict commas;

      placeholder = text: {placeholder = text;};

      call = function: arguments: {call = {inherit function arguments;};};

      dot = value: name: {dot = {inherit value name;};};

      index = value: key: {index = {inherit value key;};};

      slice = lower: upper: {slice = {inherit lower upper;};};

      list = values: {"list'" = values;};

      pair = key: value: {pair = {inherit key value;};};

      kw = name: value: {kw = {inherit name value;};};

      default = name: value: {default = {inherit name value;};};

      lambda = function: {lambda = function;};

      ternary = body: test: orelse: {ternary = {inherit body test orelse;};};

      walrus = target: value: {walrus = {inherit target value;};};

      inherit (lib.genAttrs ["listComp" "setComp" "dictComp" "gen" "finally"] (name: body: {${name} = body;})) listComp setComp dictComp gen finally;

      compFor = target: iter: {compFor = {inherit target iter;};};

      compIf = test: {compIf = test;};

      as = value: target: {as = {inherit value target;};};

      assign = target: value: {assign = {inherit target value;};};

      augAssign = target: operator: value: {augAssign = {inherit target operator value;};};

      inherit (lib.genAttrs ["pass" "break" "continue"] keyword) pass break continue;

      del = targets: {del = targets;};

      global = names: {global = names;};

      nonlocal = names: {nonlocal = names;};

      assert' = test: {"assert" = test;};

      import' = module: {"import" = module;};

      from = module: names: {from = {inherit module names;};};

      def = name: function: {def = {inherit name function;};};

      class = name: bases: body: {class = {inherit name bases body;};};

      if' = test: body: {"if" = {inherit test body;};};

      ifElse = test: body: otherwise: {"if" = {inherit test body otherwise;};};

      for = target: iter: body: {for = {inherit target iter body;};};

      while = test: body: {while = {inherit test body;};};

      with' = items: body: {"with" = {inherit items body;};};

      try = body: clauses: {try = {inherit body clauses;};};

      except = caught: body: {except = {inherit caught body;};};

      render = module: doc.render 80 (map (value: [(entry value) hardline]) (blockScope module));
    };
  in
    constructors
    // builtins.listToAttrs (map (spelled:
      lib.nameValuePair (
        if constructors ? ${spelled}
        then "${spelled}'"
        else spelled
      ) (nameNode spelled))
    builtinNames);

  javascript = let
    inherit (doc) hardline nest;

    language = "javascript";

    levels = [
      ["=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" ">>>=" "&=" "|=" "^=" "&&=" "||=" "??="]
      ["??"]
      ["||"]
      ["&&"]
      ["|"]
      ["^"]
      ["&"]
      ["==" "!=" "===" "!=="]
      ["<" ">" "<=" ">=" "in" "instanceof"]
      ["<<" ">>" ">>>"]
      ["+" "-"]
      ["*" "/" "%"]
      ["**"]
    ];

    binary = builtins.listToAttrs (lib.concatLists (lib.imap1 (level: map (operator: lib.nameValuePair operator level)) levels));

    right = ["**"] ++ builtins.head levels;

    logical = ["??" "||" "&&"];

    bitwise = ["|" "^" "&" "<<" ">>" ">>>"];

    flattens = parent: child: let
      both = group: builtins.elem parent group && builtins.elem child group;
    in
      parent != "**" && !(both ["==" "!=" "===" "!=="]) && !(both ["*" "/" "%"] && parent != child) && !(both ["<<" ">>" ">>>"]);

    mixed = parent: operator: builtins.elem parent logical && builtins.elem operator logical && parent != operator;

    clarified = parent: operator: operator == "%" && builtins.elem parent ["+" "-"] || builtins.elem parent bitwise;

    unaryBase = parent: side: child: parent == "**" && side == "left" && child.precedence < 15;

    grouped = parent: side: child: let
      level = binary.${parent};
      operator = child.operator or null;
      rules = [
        {
          when = child.precedence < level;
          value = true;
        }
        {
          when = operator == null;
          value = unaryBase parent side child;
        }
        {
          when = mixed parent operator;
          value = true;
        }
        {
          when = child.precedence > level;
          value = clarified parent operator;
        }
        {
          when = level == 1;
          value = side == "left";
        }
      ];
    in
      (lib.findFirst (rule: rule.when) {value = side == "right" || !flattens parent operator;} rules).value;

    parse = infixTree {
      inherit language right;
      precedence = binary;
      operand = item: {operand = item;};
    };

    infix = split: items: let
      walk = tree:
        if tree ? binary
        then let
          inherit (tree.binary) operator;
          side = which: let
            child = walk tree.binary.${which};
          in
            if grouped operator which child
            then ["(" child.doc ")"]
            else child.doc;
        in {
          precedence = binary.${operator};
          inherit operator;
          doc =
            if split
            then [(side "left") (nest 2 [" " operator hardline (side "right")])]
            else [(side "left") " ${operator} " (side "right")];
        }
        else expression tree.operand;
    in
      walk (parse items);

    printed = precedence: shown: {
      inherit precedence;
      doc = shown;
    };

    wrapped = least: value: let
      out = expression value;
    in
      if out.precedence < least
      then ["(" out.doc ")"]
      else out.doc;

    negative = value: (builtins.isInt value || builtins.isFloat value) && value < 0 || value ? neg;

    number = value:
      printed (
        if value < 0
        then 14
        else 15
      ) (builtins.toJSON value);

    identifier = "[A-Za-z_$][A-Za-z0-9_$]*";

    property = path:
      if builtins.match "${identifier}([.]${identifier})*" path != null
      then path
      else throw "javascript: `${path}` is not a property path";

    commas = lib.intersperse ", ";

    entries = values: values.rows or values;

    broken = value: builtins.isAttrs value && (value.obj or value.array or null) ? rows;

    layout = open: close: item: values:
      if values ? rows
      then [open (nest 2 (lib.imap1 (index: value: [hardline (item value) (lib.optional (index < builtins.length values.rows) ",")]) values.rows)) hardline close]
      else [open (commas (map item values)) close];

    key = value:
      if builtins.isString value
      then property value
      else if value ? str
      then "\"${value.str}\""
      else throw "javascript: an object key is a property name or a str";

    entry = value:
      if builtins.isList value && builtins.length value == 2
      then [(key (builtins.head value)) ": " (wrapped 1 (lib.last value))]
      else value.name or (throw "javascript: an object entry is a name or a [key value] pair");

    hugged = values: builtins.isList (lib.last values) && broken (lib.last (lib.last values));

    braced = values:
      if values ? rows || values == []
      then layout "{" "}" entry values
      else layout "{ " (lib.optionalString (!hugged values) " " + "}") entry values;

    argumentList = values: ["(" (commas (map (wrapped 1) values)) ")"];

    patterns = values: commas (map (value: (expression value).doc) values);

    expressionForms = {
      int = number;
      float = number;
      bool = value: printed 20 (lib.boolToString value);
      name = value: printed 20 value.name;
      keyword = value: printed 20 value.keyword;
      string = value: throw "javascript: the string `${value}` where an expression is expected; a name is a Nix variable, a literal a str";
      str = value: printed 20 "\"${value.str}\"";
      number = value: printed 15 value.number;
      list = infix false;
      rows = value: infix true value.rows;
      obj = value: printed 20 (braced value.obj);
      array = value: printed 20 (layout "[" "]" (wrapped 1) value.array);
      dot = value: printed 18 [(wrapped 18 value.dot.object) "." (property value.dot.path)];
      optional = value: printed 18 [(wrapped 18 value.optional.object) "?." (property value.optional.path)];
      call = value: printed 18 [(wrapped 18 value.call.callee) (argumentList value.call.arguments)];
      new = value: printed 18 ["new " (expression value.new.callee).doc (argumentList value.new.arguments)];
      not = value: printed 14 ["!" (wrapped 14 value.not)];
      neg = value:
        printed 14 [
          "-"
          (
            if negative value.neg
            then ["(" (expression value.neg).doc ")"]
            else wrapped 14 value.neg
          )
        ];
      typeof = value: printed 14 ["typeof " (wrapped 14 value.typeof)];
      await = value: printed 14 ["await " (wrapped 14 value.await)];
      async = value: let
        inner = expression value.async;
      in
        inner // {doc = ["async " inner.doc];};
      arrow = value: printed 1 ["(" (patterns value.arrow.params) ") => " (concise value.arrow.body)];
    };

    expression = value: (expressionForms.${kindOf value} or (throw "javascript: no expression ${kindOf value}")) value;

    statements = value:
      if builtins.isList value
      then value
      else value.inline or [];

    blockLike = value: builtins.isList value || value ? inline;

    braces = value:
      if value ? inline
      then
        if value.inline == []
        then "{}"
        else ["{ " (lib.intersperse " " (map statement value.inline)) " }"]
      else ["{" (nest 2 (map (one: [hardline (statement one)]) value)) hardline "}"];

    clause = value:
      if blockLike value
      then braces value
      else statement value;

    concise = value:
      if blockLike value
      then braces value
      else if value ? obj
      then ["(" (expression value).doc ")"]
      else wrapped 1 value;

    function = value: ["function " value.function.name.name "(" (patterns value.function.params) ") " (braces value.function.body)];

    statementForms = {
      blank = _: "";
      comment = value: value.comment;
      const = value: ["const " (expression value.const.target).doc " = " (wrapped 1 value.const.value) ";"];
      "let" = value: ["let " (expression value."let".target).doc (lib.optionals (value."let".value != null) [" = " (wrapped 1 value."let".value)]) ";"];
      import = value: ["import { " (commas (map (one: one.name) value.import.names)) " } from \"" value.import.source "\";"];
      inherit function;
      async = value:
        if value.async ? function
        then ["async " (function value.async)]
        else [(expression value).doc ";"];
      return = value:
        if value.return == null
        then "return;"
        else ["return " (wrapped 1 value.return) ";"];
      throw = value: ["throw " (wrapped 1 value.throw) ";"];
      continue = _: "continue;";
      "if" = value: ["if (" (wrapped 1 value."if".test) ") " (clause value."if".body) (lib.optionals (value."if" ? otherwise) [" else " (clause value."if".otherwise)])];
      while = value: ["while (" (wrapped 1 value.while.test) ") " (clause value.while.body)];
      try = value: ["try " (braces value.try.body) " catch " (lib.optionals (value.try ? param) ["(" (expression value.try.param).doc ") "]) (braces value.try.handler)];
    };

    statement = value: (statementForms.${kindOf value} or (_: [(expression value).doc ";"])) value;

    name = spelled:
      if builtins.match "[A-Za-z_][A-Za-z0-9_]*" spelled != null
      then {name = spelled;}
      else throw "javascript: `${spelled}` is not a name Nix can spell";

    patternForms = {
      name = value: [value.name];
      obj = value:
        lib.concatMap (one:
          patternNames (
            if builtins.isList one
            then lib.last one
            else one
          )) (entries value.obj);
      array = value: lib.concatMap patternNames (entries value.array);
    };

    patternNames = value: (patternForms.${kindOf value} or (throw "javascript: ${kindOf value} is not a binding pattern; a name is a Nix variable")) value;

    binding = target: lib.throwIf (patternNames target == []) "javascript: a binding pattern that declares no name" target;

    declaring = {
      const = value: patternNames value.const.target;
      "let" = value: patternNames value."let".target;
      import = value: map (one: one.name) value.import.names;
      function = value: [value.function.name.name];
      async = value: declares value.async;
    };

    declares = value: (declaring.${kindOf value} or (_: [])) value;

    declaredIn = value: lib.concatMap declares (statements value);

    scope = declared: boundBlock {inherit language name declared;};

    block = scope declaredIn;

    callable = definition: let
      result = scope (scoped: lib.concatMap patternNames scoped.params ++ declaredIn scoped.body) definition;
    in {
      params = map binding result.params;
      inherit (result) body;
    };

    handler = scope (result:
      if result ? param
      then patternNames result.param ++ declaredIn result.body
      else declaredIn result);

    clauseOf = value:
      if builtins.isFunction value || blockLike value
      then block value
      else value;

    globals = ["AbortController" "Array" "Boolean" "Buffer" "Date" "Error" "JSON" "Map" "Math" "Number" "Object" "Promise" "RegExp" "Set" "String" "Symbol" "TypeError" "URL" "URLSearchParams" "clearInterval" "clearTimeout" "console" "fetch" "globalThis" "parseFloat" "parseInt" "process" "queueMicrotask" "setInterval" "setTimeout" "structuredClone" "undefined"];
  in
    lib.genAttrs globals name
    // {
      null' = {keyword = "null";};

      blank = {blank = true;};

      continue = {continue = true;};

      comment = string: {comment = string;};

      inherit (lib.genAttrs ["inline" "rows" "obj" "array"] (tag: values: {${tag} = values;})) inline rows obj array;

      str = string: {str = string;};

      number = written: {number = written;};

      dot = object: path: {dot = {inherit object path;};};

      optional = object: path: {optional = {inherit object path;};};

      call = callee: arguments: {call = {inherit callee arguments;};};

      new = callee: arguments:
        if callee ? name || callee ? dot
        then {new = {inherit callee arguments;};}
        else throw "javascript: new takes a name or a dot path";

      inherit (lib.genAttrs ["not" "neg" "typeof" "await" "async" "return"] (tag: value: {${tag} = value;})) not neg typeof await async return;

      arrow = definition: {arrow = callable definition;};

      function = named: definition: {function = {name = named;} // callable definition;};

      const = target: value: {
        const = {
          target = binding target;
          inherit value;
        };
      };

      let' = target: value: {
        "let" = {
          target = binding target;
          inherit value;
        };
      };

      import' = names: source: {
        import = {
          names = map (one: lib.throwIfNot (one ? name) "javascript: an import names Nix variables" one) names;
          inherit source;
        };
      };

      throw' = value: {throw = value;};

      if' = test: body: {
        "if" = {
          inherit test;
          body = clauseOf body;
        };
      };

      ifElse = test: body: otherwise: {
        "if" = {
          inherit test;
          body = clauseOf body;
          otherwise = clauseOf otherwise;
        };
      };

      while = test: body: {
        while = {
          inherit test;
          body = clauseOf body;
        };
      };

      try' = body: caught: let
        catching = handler caught;
      in {
        try =
          {body = block body;}
          // (
            if catching ? param
            then {
              param = binding catching.param;
              handler = catching.body;
            }
            else {handler = catching;}
          );
      };

      render = top: doc.render 80 (map (value: [(statement value) hardline]) (block top));
    };

  ts = let
    inherit (doc) hardline nest;

    nameNode = spelled: {name = spelled;};

    see = value:
      if builtins.isFunction value
      then
        boundBlock {
          language = "ts";
          name = nameNode;
          declared = binds;
        }
        value
      else if value ? infix
      then infixNode value.infix
      else value;

    never = _: false;

    items = value: value.rows or value;

    patternForms = {
      name = value: [value.name];
      obj = value: lib.concatMap patternNames (items value.obj);
      array = value: lib.concatMap patternNames (items value.array);
      prop = value: patternNames value.prop.value;
      inherit (lib.genAttrs ["spread" "opt" "readonly"] (name: value: patternNames value.${name})) spread opt readonly;
      typed = value: patternNames value.typed.value;
      binary = value: patternNames value.binary.left;
    };

    patternNames = value: let
      seen = see value;
    in
      (patternForms.${kindOf seen} or (_: [])) seen;

    parameterForms = {
      list = lib.concatMap patternNames;
      rows = value: parameterNames value.rows;
      typed = value: parameterNames value.typed.value;
    };

    parameterNames = value: (parameterForms.${kindOf value} or patternNames) value;

    headName = head: let
      seen = see head;
    in
      if seen ? extends
      then seen.extends.name.name
      else seen.name;

    importForms = {
      list = lib.concatMap imported;
      inherit (lib.genAttrs ["rows" "typeOnly" "namespace"] (name: value: imported value.${name})) rows typeOnly namespace;
      name = value: [value.name];
    };

    imported = value: let
      seen = see value;
    in
      importForms.${kindOf seen} seen;

    declarationForms = {
      const = value: patternNames value.const.target;
      "let" = value: patternNames value."let".target;
      fn = value: lib.optional (value.fn.name != null) value.fn.name.name;
      inherit (lib.genAttrs ["async" "export" "default"] (name: value: declares value.${name})) async export default;
      class = value: [(headName value.class.head)];
      interface = value: [(headName value.interface.head)];
      type = value: [value.type.name.name];
      import = value: imported value.import.clause;
    };

    declares = value: let
      seen = see value;
    in
      (declarationForms.${kindOf seen} or (_: [])) seen;

    statementsOf = body:
      if builtins.isList body
      then lib.concatMap declares body
      else [];

    loopBinds = loop: patternNames loop.target ++ statementsOf loop.body;

    bindForms = {
      list = lib.concatMap declares;
      inherit (lib.genAttrs ["fn" "arrow" "method"] (name: value: parameterNames value.${name}.parameters ++ statementsOf value.${name}.body)) fn arrow method;
      for = value: lib.optionals (value.for.init != null) (declares value.for.init) ++ statementsOf value.for.body;
      inherit (lib.genAttrs ["forOf" "forIn" "forAwait"] (name: value: loopBinds value.${name})) forOf forIn forAwait;
      catch = value: lib.optionals (value.catch.parameter != null) (patternNames value.catch.parameter) ++ statementsOf value.catch.body;
      switch = value: lib.concatMap (arm: lib.optionals (arm ? case) (statementsOf arm.case.body)) value.switch.cases;
    };

    binds = value: (bindForms.${kindOf value} or (_: throw "ts: a lambda around a ${kindOf value}, which binds no names")) value;

    assignments = ["=" "+=" "-=" "*=" "/=" "%=" "**=" "<<=" ">>=" ">>>=" "&=" "|=" "^=" "&&=" "||=" "??="];

    precedence =
      lib.genAttrs assignments (_: 1)
      // {
        "??" = 2;
        "||" = 3;
        "&&" = 4;
        "|" = 5;
        "^" = 6;
        "&" = 7;
        "==" = 8;
        "!=" = 8;
        "===" = 8;
        "!==" = 8;
        "<" = 9;
        ">" = 9;
        "<=" = 9;
        ">=" = 9;
        "in" = 9;
        "instanceof" = 9;
        "as" = 9;
        "satisfies" = 9;
        "<<" = 10;
        ">>" = 10;
        ">>>" = 10;
        "+" = 11;
        "-" = 11;
        "*" = 12;
        "/" = 12;
        "%" = 12;
        "**" = 13;
      };

    parse = infixTree {
      language = "ts";
      inherit precedence;
      right = assignments ++ ["**"];
      operand = item:
        if builtins.isList item
        then parse item
        else if builtins.isString item
        then throw "ts: the bare string \"${item}\" is not an operand; a name is a Nix variable, text is `str`"
        else if item ? infix
        then throw "ts: an infix inside an infix list is a nested list"
        else item;
    };

    infixNode = value: let
      tree = parse (items value);
    in
      if value ? rows
      then {binary = tree.binary // {broken = true;};}
      else tree;

    assignment = operator: precedence.${operator} == 1;

    cast = operator: operator == "as" || operator == "satisfies";

    multiplicative = operator: builtins.elem operator ["*" "/" "%"];

    bitshift = operator: builtins.elem operator ["<<" ">>" ">>>"];

    bitwise = operator: bitshift operator || builtins.elem operator ["|" "^" "&"];

    equality = operator: builtins.elem operator ["==" "!=" "===" "!=="];

    logical = operator: builtins.elem operator ["??" "||" "&&"];

    both = test: outer: inner: test outer && test inner;

    unflattened = [
      (outer: _: outer == "**")
      (both equality)
      (both bitshift)
      (outer: inner: both multiplicative outer inner && (outer != inner || outer == "%"))
    ];

    flattens = outer: inner: precedence.${outer} == precedence.${inner} && !(builtins.any (rule: rule outer inner) unflattened);

    mixed = outer: inner: both logical outer inner && (outer == "??") != (inner == "??") || outer == "||" && inner == "&&";

    nestedRules = [
      {
        when = _: inner: _: cast inner;
        wants = outer: _: side: !(cast outer) || side == "right";
      }
      {
        when = outer: inner: _: assignment inner || cast outer || mixed outer inner;
        wants = _: _: _: true;
      }
      {
        when = outer: inner: _: precedence.${outer} > precedence.${inner};
        wants = _: _: _: true;
      }
      {
        when = outer: inner: _: precedence.${outer} == precedence.${inner};
        wants = outer: inner: side: side == "right" || !(flattens outer inner);
      }
      {
        when = _: inner: _: inner == "%";
        wants = outer: _: _: outer == "+" || outer == "-";
      }
    ];

    nested = outer: inner: side:
      (lib.findFirst (rule: rule.when outer inner side) {wants = parent: _: _: bitwise parent;} nestedRules).wants outer inner side;

    classForms = {
      inherit (lib.genAttrs ["binary" "ternary" "arrow" "await" "union" "intersection"] (name: _: name)) binary ternary arrow await union intersection;
      fn = _: "function";
      async = value: classOf (see value.async);
      not = _: "unary";
      neg = _: "unary";
      typeof = _: "unary";
      delete = _: "unary";
      int = signed;
      float = signed;
      obj = _: "object";
    };

    signed = value:
      if value < 0
      then "negative"
      else "atom";

    classOf = value: (classForms.${kindOf value} or (_: "atom")) value;

    listed = classes: value: builtins.elem (classOf value) classes;

    binaryWith = test: value: classOf value == "binary" && test value.binary.operator;

    slotWants = {
      unary = listed ["binary" "ternary" "arrow" "await"];
      spread = listed ["binary" "ternary" "await"];
      member = listed ["binary" "ternary" "arrow" "function" "await" "unary" "negative"];
      test = value: listed ["ternary" "arrow" "await"] value || binaryWith (operator: cast operator || assignment operator) value;
      branch = binaryWith (operator: cast operator || operator == "??");
      arrowBody = listed ["ternary" "object"];
      statement = listed ["object" "function"];
      arrayOf = listed ["union" "intersection" "arrow" "unary"];
      union = listed ["arrow"];
      intersection = listed ["union" "arrow"];
    };

    always = _: _: true;

    powerLeft = slot: _: slot.operator == "**" && slot.side == "left";

    operandWants = {
      binary = slot: value: nested slot.operator value.binary.operator slot.side;
      ternary = always;
      arrow = always;
      await = always;
      unary = powerLeft;
      negative = powerLeft;
    };

    binaryWants = slot: value:
      if assignment slot.operator
      then slot.side == "left" && binaryWith cast value
      else (operandWants.${classOf value} or (_: never)) slot value;

    wants = slot: value:
      if builtins.isAttrs slot
      then binaryWants slot value
      else (slotWants.${slot} or never) value;

    operand = slot: value: let
      seen = see value;
      printed = expr seen;
    in
      if wants slot seen
      then ["(" printed ")"]
      else printed;

    hangForms = {
      arrow = value: see value.arrow.body ? below;
      async = value: hangs value.async;
    };

    hangs = value: let
      seen = see value;
    in
      (hangForms.${kindOf seen} or never) seen;

    entry = printer: mark: last: value:
      if value ? blank
      then hardline
      else if value ? comment
      then [hardline value.comment]
      else [hardline (printer value) (lib.optionalString (!last) mark)];

    broken = printer: open: close: mark: final: rows: let
      count = builtins.length rows;
    in [open (nest 1 (lib.imap1 (index: entry printer mark (index == count && final (lib.last rows))) rows)) hardline close];

    flat = printer: open: close: mark: padded: values: let
      pad = lib.optionalString padded " ";
      hug = !padded && mark == "," && hangs (lib.last values);
    in [
      open
      pad
      (lib.intersperse "${mark} " (map printer values))
      (
        if hug
        then [mark hardline]
        else pad
      )
      close
    ];

    delimitedWith = printer: open: close: mark: padded: final: values:
      if values ? rows
      then broken printer open close mark final values.rows
      else if values == []
      then open + close
      else flat printer open close mark padded values;

    delimited = delimitedWith expr;

    terminatedForms = {
      async = value: terminated value.async;
      readonly = value: terminated value.readonly;
      method = value: value.method.body == null;
      comment = never;
    };

    terminated = value: let
      seen = see value;
    in
      (terminatedForms.${kindOf seen} or (_: true)) seen;

    rest = value: let
      seen = see value;
    in
      seen ? spread || seen ? comment;

    array = delimited "[" "]" "," false never;

    argumentList = delimited "(" ")" "," false never;

    parameterListForms = {
      list = delimited "(" ")" "," false rest;
      rows = delimited "(" ")" "," false rest;
      typed = value: [(parameterList value.typed.value) ": " (expr value.typed.type)];
    };

    parameterList = value: (parameterListForms.${kindOf value} or expr) value;

    hang = slot: value: let
      seen = see value;
    in
      if seen ? below
      then nest 1 [hardline (operand slot seen.below)]
      else [" " (operand slot seen)];

    branch = mark: value: let
      seen = see value;
    in
      if seen ? below
      then nest 1 [hardline mark " " (nest 1 (operand "branch" seen.below))]
      else [" " mark " " (operand "branch" seen)];

    keyDoc = value:
      if builtins.isString value
      then value
      else expr value;

    block = value: let
      values = see value;
    in
      if values == []
      then "{}"
      else ["{" (nest 1 (map (one: [hardline (line one)]) values)) hardline "}"];

    line = value:
      if value ? blank
      then []
      else statement value;

    memberForms = {
      string = lib.id;
      typed = value: [(memberDoc value.typed.value) ": " (expr value.typed.type)];
      opt = value: [(memberDoc value.opt) "?"];
      readonly = value: ["readonly " (memberDoc value.readonly)];
      async = value: ["async " (memberDoc value.async)];
      field = value: [(memberDoc value.field.key) " =" (hang "value" value.field.value)];
    };

    memberDoc = value: let
      seen = see value;
    in
      (memberForms.${kindOf seen} or expr) seen;

    member = value: let
      seen = see value;
    in
      if seen ? blank
      then []
      else if terminated seen
      then [(memberDoc seen) ";"]
      else memberDoc seen;

    memberBlock = values:
      if values == []
      then "{}"
      else ["{" (nest 1 (map (value: [hardline (member value)]) values)) hardline "}"];

    consequent = value:
      if builtins.isList (see value)
      then [" " (block value)]
      else [" " (statement value)];

    parenthesized = value: let
      seen = see value;
    in
      if seen ? below
      then ["(" (nest 1 [hardline (expr seen.below)]) hardline ")"]
      else ["(" (expr seen) ")"];

    binding = kind: value: [
      kind
      " "
      (expr value.target)
      (lib.optionals (value.value != null) [" =" (hang "value" value.value)])
    ];

    clauseDoc = value: let
      seen = see value;
    in
      if builtins.isList seen || seen ? rows
      then delimited "{" "}" "," true never seen
      else expr seen;

    alternative = value:
      if builtins.isList (see value)
      then ["else " (block value)]
      else ["else " (statement value)];

    loopDoc = word: op: value: ["for" word " (const " (expr value.target) " ${op} " (expr value.subject) ")" (consequent value.body)];

    label = test:
      if test == null
      then "default:"
      else ["case " (expr test) ":"];

    clauseBody = body:
      if body ? braces
      then [" " (block body.braces)]
      else nest 1 (map (one: [hardline (line one)]) body);

    armForms = {
      blank = _: [];
      comment = value: value.comment;
      case = value: [(label value.case.test) (clauseBody value.case.body)];
    };

    armDoc = value: armForms.${kindOf value} value;

    initForms = {
      const = value: binding "const" value.const;
      "let" = value: binding "let" value."let";
    };

    initializer = value: let
      seen = see value;
    in
      (initForms.${kindOf seen} or expr) seen;

    forClause = value: [";" (lib.optionals (value != null) [" " (expr value)])];

    handler = value: let
      seen = see value;
    in
      if seen ? finally
      then [" finally " (block seen.finally)]
      else [" catch" (lib.optionals (seen.catch.parameter != null) [" (" (expr seen.catch.parameter) ")"]) " " (block seen.catch.body)];

    statementForms = {
      comment = value: value.comment;
      const = value: [(binding "const" value.const) ";"];
      "let" = value: [(binding "let" value."let") ";"];
      return = value:
        if value.return == null
        then "return;"
        else if see value.return ? below
        then ["return " (parenthesized value.return) ";"]
        else ["return " (expr value.return) ";"];
      throw = value: ["throw " (expr value.throw) ";"];
      break = _: "break;";
      continue = _: "continue;";
      "if" = value: let
        test = value."if";
      in [
        "if "
        (parenthesized test.test)
        (consequent test.body)
        (lib.optionals (test ? otherwise) [
          (
            if builtins.isList (see test.body)
            then " "
            else hardline
          )
          (alternative test.otherwise)
        ])
      ];
      for = value: [
        "for ("
        (lib.optional (value.for.init != null) (initializer value.for.init))
        (forClause value.for.condition)
        (forClause value.for.update)
        ")"
        (consequent value.for.body)
      ];
      forOf = value: loopDoc "" "of" value.forOf;
      forIn = value: loopDoc "" "in" value.forIn;
      forAwait = value: loopDoc " await" "of" value.forAwait;
      while = value: ["while " (parenthesized value.while.test) (consequent value.while.body)];
      switch = value: ["switch " (parenthesized value.switch.subject) " {" (nest 1 (map (one: [hardline (armDoc one)]) value.switch.cases)) hardline "}"];
      try = value: ["try " (block value.try.body) (map handler value.try.handlers)];
      fn = expr;
      class = expr;
      interface = expr;
      type = value: ["type " (expr value.type.name) " = " (expr value.type.value) ";"];
      import = value: ["import " (clauseDoc value.import.clause) " from " (expr value.import.source) ";"];
      async = value: ["async " (statement value.async)];
      export = value: ["export " (statement value.export)];
      default = value: ["default " (statement value.default)];
    };

    statement = value: let
      seen = see value;
    in
      (statementForms.${kindOf seen} or (one: [(operand "statement" one) ";"])) seen;

    continues = split: slot: left: split && left ? binary && precedence.${left.binary.operator} == precedence.${slot.operator} && !(wants slot left);

    stacked = slot: value: [hardline (operand slot value)];

    binaryDoc = value: let
      operation = value.binary;
      split = operation.broken or false;
      left = see operation.left;
      leftSlot = {
        inherit (operation) operator;
        side = "left";
      };
      rightSlot = {
        inherit (operation) operator;
        side = "right";
      };
    in [
      (
        if continues split leftSlot left
        then binaryDoc {binary = left.binary // {broken = true;};}
        else operand leftSlot left
      )
      " "
      operation.operator
      (
        (
          if split
          then stacked
          else hang
        )
        rightSlot
        operation.right
      )
    ];

    dotted = path:
      if lib.hasPrefix "?." path
      then path
      else ".${path}";

    pathLinks = path: let
      parts = builtins.split "(\\?\\.|\\.)" (dotted path);
    in
      lib.zipListsWith (mark: segment: {
        kind = "member";
        doc = "${mark}${segment}";
      }) (map builtins.head (builtins.filter builtins.isList parts)) (builtins.tail (builtins.filter builtins.isString parts));

    literalKinds = ["int" "str" "sq"];

    called = callee: mark: arguments: let
      seen = see callee;
      generic = seen ? generic;
    in {
      base = seen.generic.callee or seen;
      links = [
        {
          kind = "call";
          doc = [
            (lib.optionals generic (delimited "<" ">" "," false never seen.generic.types))
            mark
            (argumentList arguments)
          ];
        }
      ];
    };

    indexed = value: open: index: {
      base = value;
      links = [
        {
          kind =
            if builtins.elem (kindOf (see index)) literalKinds
            then "literalIndex"
            else "index";
          doc = [open (expr index) "]"];
        }
      ];
    };

    linkForms = {
      dot = value: {
        base = value.dot.value;
        links = pathLinks value.dot.path;
      };
      call = value: called value.call.callee "" value.call.arguments;
      optCall = value: called value.optCall.callee "?." value.optCall.arguments;
      idx = value: indexed value.idx.value "[" value.idx.index;
      optIdx = value: indexed value.optIdx.value "?.[" value.optIdx.index;
      nonNull = value: {
        base = value.nonNull;
        links = [
          {
            kind = "nonNull";
            doc = "!";
          }
        ];
      };
    };

    spine = value: let
      seen = see value;
      step = (linkForms.${kindOf seen} or (_: null)) seen;
      inner = spine step.base;
    in
      if step == null
      then {
        base = seen;
        links = [];
      }
      else {
        inherit (inner) base;
        links = inner.links ++ step.links;
      };

    prefix = test: values:
      if values != [] && test (builtins.head values)
      then 1 + prefix test (builtins.tail values)
      else 0;

    kinds = names: link: builtins.elem link.kind names;

    memberish = kinds ["member" "index" "literalIndex"];

    opens = state: link: state.groups == [] || state.called && kinds ["member" "literalIndex"] link;

    grouping = state: link: let
      starts = opens state link;
    in {
      groups =
        if starts
        then state.groups ++ [[link]]
        else lib.init state.groups ++ [(lib.last state.groups ++ [link])];
      called = link.kind == "call" || !starts && state.called;
    };

    chainDoc = value: let
      whole = spine value;
      inherit (whole) links;
      calls = prefix (kinds ["call" "nonNull" "literalIndex"]) links;
      after = lib.drop calls links;
      merged = prefix lib.id (lib.zipListsWith (one: next: memberish one && memberish next) after (lib.drop 1 after));
      lead =
        if links != [] && (builtins.head links).kind != "call"
        then calls + merged
        else calls;
      inherit
        (lib.foldl' grouping {
          groups = [];
          called = false;
        } (lib.drop lead links))
        groups
        ;
    in [
      (operand "member" whole.base)
      (map (link: link.doc) (lib.take lead links))
      (map (group: nest 1 [hardline (map (link: link.doc) group)]) groups)
    ];

    forms = {
      name = value: value.name;
      int = toString;
      float = builtins.toJSON;
      bool = lib.boolToString;
      num = value: value.num;
      string = value: throw "ts: the bare string \"${value}\" where an expression is expected; a name is a Nix variable, text is `str`";
      list = _: throw "ts: a list where an expression is expected; an array is `array [...]`, operators are `infix [...]`";
      array = value: array value.array;
      str = value: "\"${value.str}\"";
      sq = value: "'${value.sq}'";
      tpl = value: let
        tick = "`";
      in [
        tick
        (map (part:
          if builtins.isString part
          then part
          else ["\${" (expr part.sub) "}"])
        value.tpl)
        tick
      ];
      regex = value: "/${value.regex.pattern}/${value.regex.flags}";
      obj = value: delimited "{" "}" "," true never value.obj;
      shape = value: delimitedWith memberDoc "{" "}" ";" true never value.shape;
      prop = value: [(keyDoc value.prop.key) ":" (hang "value" value.prop.value)];
      typed = value: [(expr value.typed.value) ": " (expr value.typed.type)];
      opt = value: [(expr value.opt) "?"];
      computed = value: ["[" (expr value.computed) "]"];
      call = value: [(operand "member" value.call.callee) (argumentList value.call.arguments)];
      optCall = value: [(operand "member" value.optCall.callee) "?." (argumentList value.optCall.arguments)];
      new = value: ["new " (operand "member" value.new.callee) (argumentList value.new.arguments)];
      generic = value: [(operand "member" value.generic.callee) (delimited "<" ">" "," false never value.generic.types)];
      dot = value: [(operand "member" value.dot.value) (dotted value.dot.path)];
      idx = value: [(operand "member" value.idx.value) "[" (expr value.idx.index) "]"];
      optIdx = value: [(operand "member" value.optIdx.value) "?.[" (expr value.optIdx.index) "]"];
      nonNull = value: [(operand "member" value.nonNull) "!"];
      inc = value: [(operand "member" value.inc) "++"];
      dec = value: [(operand "member" value.dec) "--"];
      not = value: ["!" (operand "unary" value.not)];
      neg = value: ["-" (operand "unary" value.neg)];
      typeof = value: ["typeof " (operand "unary" value.typeof)];
      delete = value: ["delete " (operand "unary" value.delete)];
      await = value: ["await " (operand "unary" value.await)];
      readonly = value: ["readonly " (expr value.readonly)];
      spread = value: ["..." (operand "spread" value.spread)];
      async = value: ["async " (expr value.async)];
      typeOnly = value: ["type " (clauseDoc value.typeOnly)];
      namespace = value: ["* as " (expr value.namespace)];
      arrayOf = value: [(operand "arrayOf" value.arrayOf) "[]"];
      union = value: lib.intersperse " | " (map (operand "union") value.union);
      intersection = value: lib.intersperse " & " (map (operand "intersection") value.intersection);
      is = value: [(expr value.is.name) " is " (expr value.is.type)];
      extends = value: [(expr value.extends.name) " extends " (expr value.extends.base)];
      binary = binaryDoc;
      ternary = value: [(operand "test" value.ternary.test) (branch "?" value.ternary.yes) (branch ":" value.ternary.no)];
      arrow = value: [
        (parameterList value.arrow.parameters)
        " =>"
        (
          if builtins.isList (see value.arrow.body)
          then [" " (block value.arrow.body)]
          else if see value.arrow.body ? below
          then hang "value" value.arrow.body
          else hang "arrowBody" value.arrow.body
        )
      ];
      fn = value: ["function" (lib.optionals (value.fn.name != null) [" " (expr value.fn.name)]) (lib.optionalString (value.fn.name == null) " ") (parameterList value.fn.parameters) " " (block value.fn.body)];
      method = value: [(memberDoc value.method.key) (parameterList value.method.parameters) (lib.optionals (value.method.body != null) [" " (block value.method.body)])];
      chain = value: chainDoc value.chain;
      class = value: ["class " (expr value.class.head) " " (memberBlock value.class.members)];
      interface = value: ["interface " (expr value.interface.head) " " (memberBlock value.interface.members)];
    };

    expr = value: let
      seen = see value;
    in
      (forms.${kindOf seen} or (throw "ts: no expression ${kindOf seen}")) seen;

    top = value:
      if value ? blank
      then hardline
      else [(statement value) hardline];

    globals =
      lib.genAttrs [
        "Array"
        "Awaited"
        "Boolean"
        "Buffer"
        "Bun"
        "Date"
        "Error"
        "Extract"
        "Intl"
        "JSON"
        "Map"
        "Math"
        "NodeJS"
        "Number"
        "Object"
        "Parameters"
        "Partial"
        "Promise"
        "Readonly"
        "ReadonlySet"
        "Record"
        "Response"
        "ReturnType"
        "Set"
        "String"
        "Symbol"
        "WeakMap"
        "WeakSet"
        "any"
        "boolean"
        "console"
        "globalThis"
        "never"
        "number"
        "object"
        "performance"
        "process"
        "string"
        "super"
        "this"
        "undefined"
        "unknown"
        "void"
      ]
      nameNode;
  in
    globals
    // {
      null' = nameNode "null";

      const' = nameNode "const";

      inherit (lib.genAttrs ["blank" "break" "continue"] (name: {${name} = true;})) blank break continue;

      inherit (lib.genAttrs ["comment" "str" "sq"] (name: string: {${name} = string;})) comment str sq;

      inherit (lib.genAttrs ["rows" "array" "union" "intersection" "infix" "braces" "finally"] (name: values: {${name} = values;})) rows array union intersection infix braces finally;

      inherit (lib.genAttrs ["below" "sub" "opt" "computed" "nonNull" "inc" "dec" "not" "neg" "typeof" "delete" "await" "readonly" "spread" "async" "export" "default" "typeOnly" "namespace" "arrayOf" "chain" "return"] (name: value: {${name} = value;})) below sub opt computed nonNull inc dec not neg typeof delete await readonly spread async export default typeOnly namespace arrayOf chain return;

      spaces = by: values: {spaces = {inherit by values;};};

      num = text: {num = text;};

      tpl = parts: {tpl = parts;};

      regex = pattern: flags: {regex = {inherit pattern flags;};};

      obj = members: {obj = members;};

      shape = members: {shape = members;};

      prop = key: value: {prop = {inherit key value;};};

      field = key: value: {field = {inherit key value;};};

      typed = value: type: {typed = {inherit value type;};};

      inherit (lib.genAttrs ["call" "optCall" "new"] (name: callee: arguments: {${name} = {inherit callee arguments;};})) call optCall new;

      generic = callee: types: {generic = {inherit callee types;};};

      dot = value: path: {dot = {inherit value path;};};

      idx = value: index: {idx = {inherit value index;};};

      optIdx = value: index: {optIdx = {inherit value index;};};

      is = name: type: {is = {inherit name type;};};

      extends = name: base: {extends = {inherit name base;};};

      ternary = test: yes: no: {ternary = {inherit test yes no;};};

      arrow = parameters: body: {arrow = {inherit parameters body;};};

      fn = name: parameters: body: {fn = {inherit name parameters body;};};

      method = key: parameters: body: {method = {inherit key parameters body;};};

      class = head: members: {class = {inherit head members;};};

      interface = head: members: {interface = {inherit head members;};};

      type = name: value: {type = {inherit name value;};};

      const = target: value: {const = {inherit target value;};};

      let' = target: value: {"let" = {inherit target value;};};

      return' = {return = null;};

      throw' = value: {throw = value;};

      if' = test: body: {"if" = {inherit test body;};};

      ifElse = test: body: otherwise: {"if" = {inherit test body otherwise;};};

      for = init: condition: update: body: {for = {inherit init condition update body;};};

      inherit (lib.genAttrs ["forOf" "forIn" "forAwait"] (name: target: subject: body: {${name} = {inherit target subject body;};})) forOf forIn forAwait;

      while = test: body: {while = {inherit test body;};};

      switch = subject: cases: {switch = {inherit subject cases;};};

      case = test: body: {case = {inherit test body;};};

      try = body: handlers: {try = {inherit body handlers;};};

      catch = parameter: body: {catch = {inherit parameter body;};};

      import' = clause: source: {import = {inherit clause source;};};

      render = file: let
        spaced = (see file) ? spaces;
        values =
          if spaced
          then file.spaces.values
          else file;
      in
        doc.renderWith (
          if spaced
          then lib.strings.replicate file.spaces.by " "
          else "\t"
        )
        null (map top (see values));
    };

  sourceFile = name: text:
    builtins.path {
      path = builtins.toFile name text;
      inherit name;
    };
in {
  cssSource = list: "${nodes list}\n";

  inherit doc glsl javascript lisp lua luau markdown patch python shell ts vbs;

  inherit sourceFile;

  sourceDir = pkgs: name: files: pkgs.runCommand name {} "mkdir $out\n${lib.concatStrings (lib.mapAttrsToList (file: text: "cp ${sourceFile file text} $out/${file}\n") files)}";

  jsonSource = value: jsonValue "" value + "\n";

  tomlSource = groups: lib.concatMapStringsSep "\n" (lib.concatMapStrings (item: "${tomlLine item}\n")) groups;

  iniSource = lib.concatMapStringsSep "\n" (section: "[${section.name}]\n${keyValueSource "" section.groups}");

  keyValueSource = keyValueSource "";

  cmusThemeSource = keyValueSource "set ";

  xmlSource = root: "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n${xmlElement "" root}";

  sshPublicKey = key: "${key.type} ${key.key} ${key.comment}";

  sshKeySource = key: "${key.type} ${key.key} ${key.comment}\n";

  pemSource = pem: lib.concatMapStrings (block: "-----BEGIN ${pem.label}-----\n${wrap pem.width block}-----END ${pem.label}-----\n") pem.blocks;

  gitignoreSource = lib.concatMapStrings (pattern: "${pattern}\n");

  pathSource = path: "${path}\n";
}
