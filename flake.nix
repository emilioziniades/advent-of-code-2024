{
  description = "A flake to set up basic development shells";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    forAllSystems = fn:
      nixpkgs.lib.genAttrs
      ["x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin"]
      (system: fn system nixpkgs.legacyPackages.${system});
  in {
    devShells = forAllSystems (system: pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          # put your packages here!
          curl
        ];
      };
    });
  };
}
