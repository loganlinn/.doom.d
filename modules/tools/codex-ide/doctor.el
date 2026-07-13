;;; tools/codex-ide/doctor.el -*- lexical-binding: t; -*-

(unless (executable-find "codex")
  (warn! "Couldn't find codex executable"))
