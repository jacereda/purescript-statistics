{
  description = "purescript-statistics package";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-parts.url = "github:hercules-ci/flake-parts";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  };

  outputs = inputs@{ flake-parts, pre-commit-hooks, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-darwin" ];
      perSystem = { config, pkgs, system, ... }: {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = with pkgs; [
            nodePackages.purescript-language-server
            nodePackages.purs-tidy
            nodejs
            purescript
            spago
          ];
#          shellHook = ''
#            ${config.checks.pre-commit-check.shellHook}
#          '';
        };
#        checks = {
#          pre-commit-check = pre-commit-hooks.lib.${system}.run {
#            src = ./.;
#            hooks = {
#              deadnix.enable = true;
#              nixpkgs-fmt.enable = true;
#              purs-tidy.enable = true;
#              statix.enable = true;
#            };
#          };
#        };
      };
    };
}
