{
  lib,
  generators,
}: let
  file = generators.sourceFile "source-file.txt" "stored once\n";
in {
  path = file;
  text = builtins.readFile file;
}
