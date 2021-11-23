{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.05;
    lean = {
      url = github:yatima-inc/lean4/acs/add-nix-ability-for-native-libs;
    };
    lean-blake3.url = github:yatima-inc/lean-blake3;
    lean-neptune.url = github:yatima-inc/lean-neptune;

    flake-utils = {
      url = github:numtide/flake-utils;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , utils
    , nixpkgs
    , flake-utils
    , lean
    , lean-blake3
    , lean-neptune
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      lib = utils.lib.${system};
      pkgs = nixpkgs.legacyPackages.${system};
      leanPkgs = lean.packages.${system};
      Blake3 = lean-blake3.project.${system};
      Neptune = lean-neptune.project.${system};
      Ipld = leanPkgs.buildLeanPackage {
        src = ./.;
        name = "Ipld";
        deps = [ Blake3 Neptune ];
      };
    in
    {
      project = Ipld;
      packages = Ipld // {
        "Ipld" = Ipld.executable;
      };

      defaultPackage = self.packages.${system}.Ipld;

      # `nix develop`
      devShell = pkgs.mkShell {
        inputsFrom = [ Ipld.executable ];
        buildInputs = with pkgs; [
          leanPkgs.lean
        ];
        LEAN_PATH = lib.concatStringsSep ":" (map (d: "${d.modRoot}") (builtins.attrValues Ipld.allExternalDeps));
        LEAN_SRC_PATH = lib.concatStringsSep ":" (map (d: "${d.src}") (builtins.attrValues Ipld.allExternalDeps));
      };
    });
}
