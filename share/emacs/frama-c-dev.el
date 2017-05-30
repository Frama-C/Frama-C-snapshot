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

;; *** Mandatory Emacs settings for Frama-C developers ***
;;
;; If you are using 'frama-c-recommended.el', ignore these instructions
;; (this file is already included by frama-c-recommended.el).
;;
;; Otherwise, do the following:
;; 1. Include the directory containing this file in your Emacs load path;
;; 2. Load this file as a library.
;;
;; You can do so by adding these lines in the beginning of your ~/.emacs file:
;;
;; (add-to-list 'load-path "path/to/this/file/")
;; (load-library "frama-c-dev")
;;
;; tuareg and ocp-indent must be installed (e.g. via their OPAM packages).
;;
;; Note: 'frama-c-init.el' must also be in the load path.

(load-library "frama-c-init")

;; ocp-indent should always be used
(safe-require ocp-indent
  (add-hook 'tuareg-mode-hook (lambda ()
    (setq indent-line-function 'ocp-indent-line)))
)

;; Never indent Caml files using tabs
(defun no-tabs-hook ()
  (setq indent-tabs-mode nil))
(add-hook 'tuareg-mode-hook 'no-tabs-hook)
(add-hook 'caml-mode-hook 'no-tabs-hook)

;; Load Merlin (IDE features for OCaml, such as type info and code navigation)
(safe-require merlin
  ;; Start merlin on ocaml files
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  ;; Enable auto-completion: definitions for company-mode
  (set (make-local-variable 'company-backends) '(company-predictive))
  (global-set-key (kbd "<C-tab>") 'company-complete-common)
  (add-to-list 'company-backends 'merlin-company-backend)
  ;; Enable company on merlin managed buffers
  (add-hook 'merlin-mode-hook 'company-mode)
  ;; Or enable it globally:
  ;(add-hook 'after-init-hook 'global-company-mode)
  ;; If you do not want to use company-mode, you can use auto-complete mode
  ;; (setq merlin-use-auto-complete-mode 'easy)

  ;; This hook avoids accidents with merlin's "C-c C-x", which often quits
  ;; Emacs without confirmation when the user presses "C-x C-c" by accident.
  ;; This hook is local to merlin-mode: Emacs will quit normally in non-ML
  ;; files. It is also only displayed when the keyboard shortcut is used,
  ;; but not when the user clicks the Close button on Emacs' window.
  (defun confirm-before-save-kill-emacs ()
    (interactive)
    (let ((confirm-kill-emacs 'y-or-n-p))
      (save-buffers-kill-emacs)))
  (add-hook 'merlin-mode-hook
    (lambda ()
      (local-set-key (kbd "C-x C-c") 'confirm-before-save-kill-emacs)))
)
