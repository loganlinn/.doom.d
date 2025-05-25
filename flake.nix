{
  description = "doomemacs private config";

  inputs = {
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    devshell.url = "github:numtide/devshell";
  };

  outputs =
    inputs@{ self, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        flake-parts.flakeModules.easyOverlay
        inputs.devshell.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          system,
          config,
          self',
          inputs',
          pkgs,
          lib ? pkgs.lib,
          ...
        }:
        let
          # TODO integrate with init.el
          modulePackages = with pkgs; {
            default = [
              binutils # for native-comp
              emacs-all-the-icons-fonts
              fd
              ripgrep
            ];

            ":app irc" = [ gnutls ];

            # ":checkers spell +aspell" = [
            #   (aspellWithDicts (
            #     ds: with ds; [
            #       en
            #       en-computers
            #       en-science
            #     ]
            #   ))
            # ];

            ":checkers spell +hunspell" = [ hunspell ];

            ":editor format" = [ nodePackages.prettier ];

            ":emacs dired" = [
              fd
              ffmpegthumbnailer
              gnutar
              imagemagick
              mediainfo
              poppler_utils
              unzip
            ];

            ":emacs undo" = [ zstd ];

            # ":lang clojure" = [ cljfmt clojure-lsp ];

            ":lang docker" = [
              dockfmt
              dockerfile-language-server-nodejs
            ];

            # ":lang elixir +lsp" = [ elixir-ls ];

            ":lang go" = [
              gomodifytags
              gopls
              gore
              gotests
            ];

            ":lang javascript" = [ nodePackages.prettier ];

            ":lang latex" = [ texlive.combined.scheme-medium ];

            ":lang markdown" = [ python3Packages.grip ];

            ":lang org +gnuplot" = [ gnuplot ];

            ":lang org +pandoc" = [ pandoc ];

            ":lang org +roam" = [ sqlite ];

            ":lang sh +lsp" = [ bash-language-server ];

            ":lang sh" = [
              shellcheck
              shfmt
            ];

            ":tools direnv" = [ direnv ];

            ":tools editorconfig" = [ editorconfig-core-c ];

            ":tools just" = [ just ];

            ":tools lookup" = [
              ripgrep
              sqlite
              wordnet
            ];

            ":tools make" = [ gnumake ];

            ":tools pass" = [
              pass
              gnupg
            ];

            # ":tools pdf" = [
            #   # for building epdfinfo (i.e. M-x pdf-tools-install)
            #   pkgconfig
            #   autoconf
            #   automake
            #   libpng
            #   zlib
            #   poppler
            #   poppler_gi
            # ];

            # ":lang hugo" = [hugo];
          };
        in
        {
          overlayAttrs = inputs'.emacs.packages;
          checks = { };
          packages = { };
          formatter = pkgs.alejandra;
          devshells.default = {
            name = "DOOM";
            env = [
              {
                name = "DOOMDIR";
                eval = "$(pwd)/"; # trailing slash expected
              }
              {
                name = "EMACSDIR";
                eval = ''$HOME/.config/emacs/''; # trailing slash expected
              }
              {
                name = "PATH";
                eval = lib.concatStringsSep ":" (
                  [
                    "$PWD/bin"
                    "$(printenv EMACSDIR)bin" # $${EMACSDIR}bin does not get escaped properly
                  ]
                  ++ (lib.optional pkgs.stdenv.isDarwin "/opt/homebrew/bin")
                  ++ lib.singleton "$PATH"
                );
              }
            ];
            commands = [
              # {
              #   category = "example";
              #   help = "print hello";
              #   name = "hello";
              #   command = "echo hello";
              # }
            ];
            packages = lib.concatLists (lib.attrValues modulePackages);
          };
        };

      flake = {
        homeModules.default = {
          imports = [
            self.homeModules.emacs
            self.homeModules.doom
          ];
        };

        homeModules.emacs =
          {
            config,
            lib,
            pkgs,
            ...
          }:
          with lib;
          let
            cfg = config.programs.emacs;
          in
          {
            config = mkIf cfg.enable {
              programs.emacs = {
                package = mkDefault pkgs.emacs-unstable; # most recent git tag
                extraPackages = epkgs: [ epkgs.vterm ];
              };

              services.emacs = {
                package = mkDefault config.programs.emacs.package;
                client = {
                  enable = mkDefault true;
                  arguments = [ "-c" ];
                };
              };

              home.packages = [
                (pkgs.writeShellScriptBin "magit" (readFile ../../../bin/magit))
              ];

              programs.zsh.initExtra = ''
                vterm_printf() {
                    if [ -n "$TMUX" ] \
                        && { [ "$${TERM%%-*}" = "tmux" ] \
                            || [ "$${TERM%%-*}" = "screen" ]; }; then
                        # Tell tmux to pass the escape sequences through
                        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
                    elif [ "$${TERM%%-*}" = "screen" ]; then
                        # GNU screen (screen, screen-256color, screen-256color-bce)
                        printf "\eP\e]%s\007\e\\" "$1"
                    else
                        printf "\e]%s\e\\" "$1"
                    fi
                }

                function e() {
                  hash emacs || return 1
                  command emacs "$@" &
                  disown %+;
                }

                function ec() {
                  hash emacsclient || return 1
                  command emacsclient --alternate-editor="" --create-frame "$@" &
                  disown %+;
                }

                function et() {
                  emacs -nw "$@"
                }
              '';
            };
          };

        homeModules.doom =
          {
            config,
            pkgs,
            lib,
            ...
          }:
          with lib;
          let
            emacsdir = "${config.xdg.configHome}/emacs"; # i.e. github.com/doomemacs/doomemacs
            doomdir = "${config.xdg.configHome}/doom"; # i.e. github.com/loganlinn/.doom.d
          in
          {
            home.packages = concatLists (attrValues (import ./packages.nix pkgs));
            home.sessionPath = [ "${emacsdir}/bin" ]; # doom cli
            home.sessionVariables.EMACSDIR = "${removeSuffix "/" emacsdir}/"; # trailing sep expected
            home.sessionVariables.DOOMDIR = "${removeSuffix "/" doomdir}/"; # trailing sep expected
            home.activation = {
              doomEmacs = hm.dag.entryAfter [ "writeBoundary" ] ''
                if ! [ -d "${emacsdir}" ]; then
                  run ${pkgs.git}/bin/git clone $VERBOSE_ARG --depth=1 --single-branch "https://github.com/doomemacs/doomemacs.git" "${emacsdir}"
                  run "${emacsdir}"/bin/doom install
                fi
              '';
              doomConfig = hm.dag.entryBefore [ "doomEmacs" ] ''
                if ! [ -d "${doomdir}" ]; then
                  run ${pkgs.git}/bin/git clone $VERBOSE_ARG https://github.com/loganlinn/.doom.d.git "${doomdir}"
                fi
              '';
            };
          };
      };
    };
}
