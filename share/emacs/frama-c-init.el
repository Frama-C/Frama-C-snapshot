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

;; *** Auxiliary definitions for Frama-C Emacs files ***
;;
;; This file is automatically included by both 'frama-c-dev.el' and
;; 'frama-c-recommended.el'. You do not need to do anything here.
;;

;; Macro to avoid crashing if the required file does not exist
(defmacro safe-require (symbol &rest body)
  `(condition-case nil
       (progn
         (require ',symbol)
         ,@body)
     (error (message (format "Require NOT available: %s" ',symbol)) nil)))

;; Macro to avoid crashing if the library to load does not exist
(defun safe-load-library (path)
  (condition-case nil
      (load-library path)
    (error (message (format "Library NOT available: %s" path)) nil)))

;; Add opam emacs directory to the load-path
(condition-case nil
    (progn nil (setq opam-share (substring
                                 (shell-command-to-string
                                  "opam config var share 2> /dev/null") 0 -1))
           (add-to-list 'load-path (concat opam-share "/emacs/site-lisp")))
  (error (message "Error configuring OPAM, check your installation.") nil))

;; Setup environment variables using OPAM
;; This helps when compiling OCaml installed via OPAM directly from Emacs
(condition-case nil
    (dolist (var (car (read-from-string (shell-command-to-string
                                         "opam config env --sexp"))))
      (setenv (car var) (cadr var)))
  (error (message "Error configuring OPAM sexp, check your installation.") nil))

;; One of the `opam config env` variables is PATH. Update `exec-path` to that.
(setq exec-path (split-string (getenv "PATH") path-separator))
