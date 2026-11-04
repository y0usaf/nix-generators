nix-generators renders structured Nix data into source text (CSS, GLSL, JavaScript, Lisp, Lua, Luau, Markdown, patches, Python, shell, TypeScript, VBScript, JSON, TOML, INI, XML, PEM and more) and writes it to the store.

The library is `lib`, a plain attribute set that only needs nixpkgs `lib`:

```nix
inputs.nix-generators = {
  url = "git+file:///home/y0usaf/dev/developing/nix-generators";
  inputs.nixpkgs.follows = "nixpkgs";
};

generators = inputs.nix-generators.lib;
generators.sourceFile "hello.ts" (generators.ts.render (data generators.ts))
```

`sourceFile name text` is `builtins.toFile name text`: each file is stored once. Re-adding that store path with `builtins.path` works in Nix but iets refuses it ("copying ... to the store again is not supported"), so `checks.x86_64-linux.source-file` evaluates `sourceFile` under both Nix and iets and requires the same path and text.

It is consumed by finix and by strictix, whose importers emit Nix that calls `generators.<lang>`.
