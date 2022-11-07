{
  # Use `nix run` to invoke (and if necessary, build) the binaries in this package, like so:
  # `$ nix run github:bitnomial/aws-ec2-knownhosts`  # equivalent to the command on the next line
  # `$ nix run github:bitnomial/aws-ec2-knownhosts#aws-ec2-knownhosts`
  # `$ nix run github:bitnomial/aws-ec2-knownhosts#aws-ec2-pubkeys`
  # `$ nix run github:bitnomial/aws-ec2-knownhosts#aws-ec2-keysync`
  #
  # The `nix-command` and `flakes` experimental features need to be enabled either on the CLI
  # or in nix.conf

  description = "aws-ec2-knownhosts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , ...
    }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        aws-ec2-knownhosts-drv = pkgs.haskell.packages.ghc924.callCabal2nix "aws-ec2-knownhosts" self { };
      in
      {
        packages.aws-ec2-knownhosts =
            pkgs.haskell.lib.justStaticExecutables aws-ec2-knownhosts-drv;
        
        apps.default = {
          type = "app";
          program = "${self.packages.${system}.aws-ec2-knownhosts}/bin/aws-ec2-knownhosts";
        };
        apps.aws-ec2-pubkeys = {
          type = "app";
          program = "${self.packages.${system}.aws-ec2-knownhosts}/bin/aws-ec2-pubkeys";
        };
        apps.aws-ec2-keysync = {
          type = "app";
          program = "${self.packages.${system}.aws-ec2-knownhosts}/bin/aws-ec2-keysync";
        };
      });
}
