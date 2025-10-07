{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs =
    inputs@{
      self,
      nixpkgs,
      flake-parts,
      ...
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [ inputs.haskell-flake.flakeModule ];

      perSystem =
        { self', pkgs, ... }:
        {

          haskellProjects.default = {
            # The base package set representing a specific GHC version.
            basePackages = pkgs.haskellPackages;

            # Extra package information goes here.
            # See https://zero-to-flakes.com/haskell-flake/dependency
            #
            # Note that local packages are automatically included in `packages`
            # (defined by `defaults.packages` option).
            #
            # packages = {
            # };

            devShell = {
              # Enabled by default
              enable = true;

              # Programs that should be available in the shell.
              # Default programs can be disabled by setting to 'null'
              # tools = hp: { fourmolu = hp.fourmolu; ghcid = null; };

              hlsCheck.enable = true;
            };
          };

          # haskell-flake doesn't set the default package, so we do it here.
          packages.default = self'.packages.excalibur;
        };
    };
}
