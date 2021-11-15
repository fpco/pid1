# Run using:
#
#     $(nix-build --no-link -A fullBuildScript)
{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  cabalPackageName = "pid1";
  compiler = "ghc844"; # matching stack.yaml

  # Pin static-haskell-nix version.
  static-haskell-nix =
    if builtins.pathExists ../.in-static-haskell-nix
      then toString ../. # for the case that we're in static-haskell-nix itself, so that CI always builds the latest version.
      # Update this hash to use a different `static-haskell-nix` version:
      else fetchTarball https://github.com/nh2/static-haskell-nix/archive/95fa110091dff2bf6dace3921c18a26c264d776e.tar.gz;

  # Pin nixpkgs version
  # By default to the one `static-haskell-nix` provides, but you may also give
  # your own as long as it has the necessary patches, using e.g.
  pkgs = import (fetchTarball https://github.com/nixos/nixpkgs/archive/ca3531850844e185d483fb878fcd00c6b44069e5.tar.gz) {};
  # pkgs = import "${static-haskell-nix}/nixpkgs.nix";
  # pkgs = import (import ./nix/sources.nix).nixpkgs {};


  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = "2020-02-24T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeShellScript "stack2nix-and-build-script.sh" ''
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

 # So it turns out we have to do this because otherwise the build fails with
 # hpack complaining that pid1.cabal has been manually modified and that I should use --force
 # to get it to override it. Deleting the cabal file from the repo doesn't help as something
 # obvously regerates it when it gets to this stage.
 # I tried deleting the cabal file in this function but then it complains there is no cabal file and fails.
 # Since the cabal is obviously already generated somehow it's safe to delete the pacakge.yaml
 # file instead
 # NOTE: this will only be useful if we switch pid1 to package.yaml (as it currently only uses the cabal file).
 # But if not it won't interfere with anything. I'm also not sure if this is strictly necessary anymore, that is
 # if static-haskell-nix has fixed it.
 static_package = with pkgs.haskell.lib;
    overrideCabal
      static-stack2nix-builder.static_package
      (old: {
        preConfigure = ''
          rm -f package.yaml
        '';
      });

in
  {
    inherit static_package;
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
