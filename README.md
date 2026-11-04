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

It is consumed by finix and by strictix, whose importers emit Nix that calls `generators.<lang>`.
