;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;  This file is part of Frama-C.                                         ;
;                                                                        ;
;  Copyright (C) 2007-2017                                               ;
;    CEA (Commissariat à l'énergie atomique et aux énergies              ;
;         alternatives)                                                  ;
;                                                                        ;
;  you can redistribute it and/or modify it under the terms of the GNU   ;
;  Lesser General Public License as published by the Free Software       ;
;  Foundation, version 2.1.                                              ;
;                                                                        ;
;  It is distributed in the hope that it will be useful,                 ;
;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;
;  GNU Lesser General Public License for more details.                   ;
;                                                                        ;
;  See the GNU Lesser General Public License version 2.1                 ;
;  for more details (enclosed in the file licenses/LGPLv2.1).            ;
;                                                                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *** Recommended Emacs settings for Frama-C developers ***
;;
;; This file should be loaded in the beginning of your .emacs file.
;;
;; 1. Include the directory containing this file in your Emacs load path;
;; 2. Load this file as a library.
;;
;; You can do so by adding these lines in the beginning of your ~/.emacs file:
;;
;; (add-to-list 'load-path "path/to/this/file/")
;; (load-library "frama-c-recommended")
;;
;; Note: 'frama-c-init.el' and 'frama-c-dev.el' must also be in the load path.

;; ***** Initialization and configuration settings *****
(load-library "frama-c-init")
(load-library "frama-c-dev")

;; MELPA helps with the installation of several Emacs packages
(safe-require package
  (add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/"))
)

;; ***** OCaml settings *****
;;
;; Load Tuareg (OCaml mode for Emacs) if installed
(safe-load-library "tuareg-site-file")

;; ***** End of OCaml settings *****


;; ***** Miscellaneous settings *****
;;
;; fill-column-indicator adds a vertical line to help enforce the
;; 80-characters-per-line rule
;; Note: in some specific configurations (e.g. KDE, maximized window),
;; this library may interfere with window redrawing when using e.g. some
;; merlin functions. Try deactivating this extension if you have problems.
(safe-require fill-column-indicator
  (setq-default fill-column 80)
  (setq fci-rule-color "#8f8f8f")
  (add-hook 'tuareg-mode-hook 'fci-mode t)
)

;; Disable insertion of tabs for indentation everywhere but in Makefiles
;; (Emacs always inserts tabs in Makefiles, regardless of this option)
(setq-default indent-tabs-mode nil)

;; Disable overwrite mode, which is rarely used nowadays and can be annoying
;; when enabled accidentally
(global-unset-key [insert])

;; redo+ (needs to be installed via ELPA) enables a somewhat "standard"
;; undo/redo mechanism, similar to most modern applications;
;; these key settings bind Ctrl+Z and Ctrl+Shift+Z to undo/redo, respectively
(safe-require redo+
  (global-set-key (kbd "C-z") 'undo-only)
  (global-set-key (kbd "C-S-z") 'redo)
  ; Avoid accidental minimization when pressing pressing C-z just after a 'C-x'
  (global-unset-key (kbd "C-x C-z"))
)

;; This hook removes trailing whitespace, which should always be avoided
;; Note that, if applied to an existing file with trailing whitespace, this
;; will remove it in *every* line. This can generate large diffs and is not
;; always ideal. For new files, however, this should be on by default.
;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Require a final newline in a file in order to avoid confusing some tools
;; (also helps 'git diff')
(setq require-final-newline t)

;; whitespace-mode, when activated, displays several kinds of possibly
;; undesirable whitespace.
;; The settings below enable display of tabs, empty lines at the end of
;; the file, and the most important one: trailing spaces
;; It is also possible to use whitespace-mode to highlight long lines
;; (over 80 characters, or the width that you prefer), but they are
;; not activated in these settings.
(global-whitespace-mode 1)
(setq whitespace-style '(face empty tabs trailing))

;; Terminal Settings
;; (related to colors in compilation mode; see MR frama-c/frama-c!300)
(safe-require ansi-color
  (defun my-colorize-compilation-buffer ()
    (ansi-color-apply-on-region (point-min) (point-max))
    (goto-char (point-min))
    (while (search-forward "\x0d" nil t)
      (delete-region (line-beginning-position) (point)))
    (goto-char (point-min))
    (while (search-forward "\x1b[K" nil t)
      (replace-match ""))
    (goto-char (point-min))
    )
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)
)
