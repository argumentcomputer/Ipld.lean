{
  inputs = {
    utils.url = "github:yatima-inc/nix-utils";
    nixpkgs.url = github:nixos/nixpkgs/nixos-21.05;
    lean = {
      url = github:leanprover/lean4;
    };
    lean-blake3 = {
      url = github:yatima-inc/lean-blake3;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.lean.follows = "lean";
      inputs.flake-utils.follows = "flake-utils";
    };
    lean-neptune = {
      url = github:yatima-inc/lean-neptune;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.lean.follows = "lean";
      inputs.flake-utils.follows = "flake-utils";
    };

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
        deps = [ Blake3 ];
      };
      joinDepsDerivationns = getSubDrv: lib.concatStringsSep ":" (map (d: "${getSubDrv d}") ([Ipld] ++ Ipld.allExternalDeps));
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
        LEAN_PATH = joinDepsDerivationns (d: d.modRoot);
        LEAN_SRC_PATH = joinDepsDerivationns (d: d.src);
      };
    });
}
