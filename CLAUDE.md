# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Personal [Doom Emacs](https://github.com/doomemacs/doomemacs) configuration (`$DOOMDIR`). Doom is installed separately at `~/.config/emacs/`; this repo is the private config layer.

## Commands

```sh
doom sync          # after changing init.el or packages.el
doom sync -u       # sync + update packages
doom doctor        # diagnose issues
doom env           # regenerate envvar cache (cli.el filters env vars)
doom build         # recompile packages
```

Doom CLI lives at `~/.config/emacs/bin/doom`. The `flake.nix` devshell puts it on `$PATH`.

## Architecture

### Core Files (load order matters)

1. **`init.el`** — `doom!` block declaring enabled modules and flags. Conditional module loading via `:cond`/`:if` for OS-specific modules.
2. **`packages.el`** — Additional packages beyond what modules provide. Pinned recipes from GitHub.
3. **`config.el`** — Main config. Sets theme, font, keybindings, then conditionally loads `+*.el` feature files at the bottom.
4. **`cli.el`** — Customizes `doom env` CLI command (filters transient env vars like `KITTY_*`, `STARSHIP_*`, `DIRENV_WATCHES`).

## Conventions

- All `.el` files use `lexical-binding: t`
- Feature files prefixed with `+` (Doom convention)
- Autoload functions prefixed with `+module-name/` (e.g. `+nix/switch-to-repl-buffer`)
- Keybindings use `map!` macro; localleader bindings in language files
- `after!` for deferred config, `use-package!` for additional packages
- Lisp modes share structural editing setup via `my/turn-on-lisp-modes` (evil-cleverparens + smartparens strict)
- Projects clone to `~/src/{host}/{owner}/{repo}` (projectile search path: `("~/src" . 3)`)
- Nix formatter: alejandra
- Format-on-save enabled globally, disabled for select modes (`+format-on-save-disabled-modes`)

### Modules

- The Doom Emacs repository should be treated as authoratative source for the design patterns and best practices.
- The local installation of Doom Emacs (`~/.config/emacs`) should be used for obtaining additional context, rather than fetching from the Doom Emacs main branch.
- Built-in modules are documented `~/.config/emacs/docs/modules.org`
- Adopt the same organizational patterns (namespaces). Have justification for _using_ or _not using_ Doom Emacs namespaces.
