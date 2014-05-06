;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                        ;
;  This file is part of Frama-C.                                         ;
;                                                                        ;
;  Copyright (C) 2008-2011                                               ;
;    Pierre Roux                                                         ;
;                                                                        ;
;  Copyright (C) 2009-2014                                               ;
;    CEA LIST                                                            ;
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

;; How to install:
;; copy the following in your .emacs file
;;
;; (setq load-path (cons "/directory/in/which/you/put/the/file/acsl.el" load-path))
;; (autoload 'acsl-mode "acsl" "Major mode for editing ACSL code" t)
;; ;; uncomment this if you want to automatically load ACSL mode with
;; ;; each C file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq auto-mode-alist
      (append
       '(("\\.[chi]" . acsl-mode))
       auto-mode-alist))

;; you can then load the mode in emacs with M-x acsl-mode

;; TODO:
;; - font-lock for ghost code

;; code

(defun acsl-keymap-init ()
  "Init keymap"
  (define-key acsl-mode-map "\C-c\C-j" 'acsl-jessie-gui))

;; font-lock

(defconst acsl-keywords
  '("assert"
    "assigns"
    "assumes"
    "allocates"
    "axiom"
    "axiomatic"
    "behavior"
    "behaviors"
    "breaks"
    "case"
    "char"
    "complete"
    "continues"
    "decreases"
    "disjoint"
    "double"
    "else"
    "ensures"
    "enum"
    "exits"
    "float"
    "for"
    "frees"
    "if"
    "inductive"
    "int"
    "integer"
    "invariant"
    "global"
    "label"
    "lemma"
    "logic"
    "long"
    "loop"
    "pragma"
    "predicate"
    "reads"
    "real"
    "requires"
    "returns"
    "short"
    "signed"
    "sizeof"
    "slice"
    "impact"
    "struct"
    "terminates"
    "type"
    "union"
    "unsigned"
    "variant"
    "void"
)
  "List of ACSL keywords to highlight.")

(defun acsl-in-acsl-annot ()
  "If we are in a C comment beginning with @."
  (and (nth 4 (syntax-ppss))
       (eq (char-after (+ (nth 8 (syntax-ppss)) 2)) ?@)))

(defvar acsl-font-lock-keywords
  (let ((pre-match-form (lambda ()
			  (goto-char (match-beginning 0))
			  (match-end 0)))
	(find-annot (lambda (limit)
			;; skip comments
			(if (and (looking-at "//") (acsl-in-acsl-annot))
			    (re-search-forward "\n" limit 'e))
			(while (and (not (acsl-in-acsl-annot)) (< (point) limit))
			  (re-search-forward "/[*/]" limit 'e))
			(if (>= (point) limit)
			    nil
			  (let ((b (save-excursion
				     (re-search-backward "/[*/]" (- (point) 2) t)
				     (point))))
			    (re-search-forward "[*/]/\\|\n" limit 'e)
			    (re-search-backward "//" (- (point) 2) t)  ; don't recolor comments
			    (set-match-data (list b (point) (nth 2 (match-data t))))
			    t)))))
    (list
     `(,find-annot
       (0 font-lock-type-face t)
       (,(concat (regexp-opt acsl-keywords 'words) "\\|<?=?=>?\\|&&\\|||\\|!=?\\|\\^\\^") (,pre-match-form) nil
	(0 font-lock-keyword-face t))
       ("\\(\\?\\)[^:]*\\(:\\)" (,pre-match-form) nil
	(1 font-lock-keyword-face t)
	(2 font-lock-keyword-face t))
       ("\\(axiom\\|behavior\\|case\\|inductive\\|predicate\\|l\\(ogic\\|emma\\)\\)\\>[ \t\n@]*\\([a-zA-Z_][a-zA-Z_0-9]*\\)" (,pre-match-form) nil
	(3 font-lock-function-name-face t))
       ("\\\\\\(at\\|e\\(mpty\\|xists\\)\\|f\\(alse\\|orall\\)\\|old\\|result\\|true\\|valid\\(_range\\|_index\\)?\\)" (,pre-match-form) nil
	(0 font-lock-constant-face t)))))
  "Default highlighting for ACSL mode")

(defun acsl-font-lock-init ()
  "Initialize font-lock for ACSL."
  (add-hook 'c-mode-hook
	    (lambda ()
	      (font-lock-add-keywords nil acsl-font-lock-keywords))))

;; custom variables

(require 'custom)

(defcustom acsl-jessie-gui-prog-name "frama-c -jessie"
  "Frama-C/Jessie executable name."
  :group 'acsl
  :type 'string)

(defcustom acsl-jessie-int-model "exact"
  "Jessie int model."
  :group 'acsl
  :type '(choice (const :tag "Exact" "exact")
		 (const :tag "Bounded" "bounded")
		 (const :tag "Modulo" "modulo")))

(defun acsl-jessie-gui ()
  "Generate VCs and show them in a GUI"
  (interactive)
  (compile (concat acsl-jessie-gui-prog-name
		   " -jessie-int-model "
		   acsl-jessie-int-model
		   " "
		   (buffer-file-name))))

;; menu

(require 'easymenu)

(defun acsl-menu-init ()
  (easy-menu-define
   acsl-menu (list acsl-mode-map)
   "ACSL Mode Menu."
   '("ACSL"
     ["Customize ACSL mode" (customize-group 'acsl) t]
     "---"
     ["Jessie GUI" acsl-jessie-gui t]
     ))
  (easy-menu-add acsl-menu))

;; indent

(defun acsl-indent-command (&optional arg)
  "Indent ACSL code (quite basic yet)."
  (interactive "*")
  (c-indent-line)
  (when (and (acsl-in-acsl-annot)
	     (< (nth 8 (syntax-ppss)) (line-beginning-position)))  ; not the first line of an annot (which don't need to be indented)
    (save-excursion
      (back-to-indentation)
      (if (not (eq (char-after) ?@))
	  (insert "@")
	(goto-char (+ (point) 1)))
      (if (not (looking-at "*/"))  ; to avoid indenting last lines of annotation of the form "@*/" (thanks Yannick)
	  (let ((current (save-excursion
			   (skip-chars-forward " \t@")))
		(expected (save-excursion
			    (let ((cc (current-column)))
			      (forward-line -1)
			      (move-to-column cc))
			    (skip-chars-forward " \t@"))))
	    (if (save-excursion
		  (skip-chars-backward " \t\n@")
		  (memq (char-before) '(?: ?=)))
		(setq expected (+ expected 2)))
	    (if (save-excursion
		  (skip-chars-forward " \t@")
		  (looking-at "\\<\\(axiom\\|behavior\\|predicate\\|l\\(ogic\\|emma\\)\\|inductive\\)\\>"))
		(setq expected (save-excursion
				 (goto-char (+ (nth 8 (syntax-ppss)) 3))
				 (skip-chars-forward " \t@"))))
	    (if (< current expected)
		(insert-char ?  (- expected current)))
	    (if (> current expected)
		(kill-forward-chars (- current expected))))))
    (if (eq (char-after) ?@)
	(skip-chars-forward " \t@"))))

(defun acsl-indent-init ()
  (setq indent-line-function 'acsl-indent-command)
  ;; maybe not the best solution for C code but still works
  (setq indent-region-function nil))

;; main function for the mode

(define-derived-mode acsl-mode c-mode
  "ACSL"
  "Major mode for C annoted with ACSL."
  (acsl-font-lock-init)
  (acsl-keymap-init)
  (acsl-indent-init)
  (acsl-menu-init))

(provide 'acsl-mode)
