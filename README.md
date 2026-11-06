nix-generators renders structured Nix data into source text (CSS, GLSL, JavaScript, Lisp, Lua, Luau, Markdown, patches, Python, shell, TypeScript, VBScript, JSON, TOML, INI, XML, PEM and more) and writes it to the store.

The library is `lib`, a plain attribute set that only needs nixpkgs `lib`:

```nix
inputs.nix-generators = {
  url = "github:y0usaf/nix-generators";
  inputs.nixpkgs.follows = "nixpkgs";
};

generators = inputs.nix-generators.lib;
generators.sourceFile "hello.ts" (generators.ts.render (data generators.ts))
```

`sourceFile name text` is `builtins.toFile name text`: each file is stored once. Re-adding that store path with `builtins.path` works in Nix but iets (a Rust reimplementation of Nix) refuses it, so `nix flake check` evaluates `sourceFile` under both and requires the same path and text.

It is consumed by finix and by strictix, whose importers emit Nix that calls `generators.<lang>`.
