;;; gershwin-mode.el --- Major mode for Gershwin code

;; Copyright (C) 2013 Daniel Gregoire
;;
;; Authors: Daniel Gregoire <daniel.l.gregoire@gmail.com>
;; URL: https://github.com/semperos
;; Version: 0.1
;; Keywords: languages, lisp, stack-based, concatenative

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This mode is derived from Clojure mode, with extra support
;; added for the Gershwin programming language.

;; Gershwin is implemented in Clojure and provides Clojure interop,
;; and since Gershwin is a concatenative language with a minimal
;; syntax of its own, Clojure mode serves the majority of its needs.

;;; Installation:

;; This mode requires:
;;
;;   * clojure-mode
;;
;; Make sure they're installed first.

;; Then just add the following to your Emacs initialization:
;;
;; (require 'gershwin-mode)

(defvar gershwin-builtins
  '("swap" "dup" "dup2" "dup3" "drop" "drop2" "drop3" "nip" "nip2" "rot"
    "over" "over2" "pick" "dip" "dip2" "dip3" "dip4" "dupd" "keep" "keep2" "keep3"
    "bi" "bi2" "bi3" "tri" "tri2" "tri3" "bi*" "bi2*" "tri*" "tri2*" "boolean"
    "bi&" "bi2&" "tri&" "tri2&" "both?" "either?" "+" "-" "*" "div" "lt"
    "gt" "lt=" "gt=" "odd?" "even?" "=" "nth" "assoc" "conj" "cons" "dissoc" "print-doc"
    "empty?" "count"
    "meta" "invoke" "apply" "apply2" "apply3" "apply-swap"
    "load" "require" "reload" "compile" "clear" "pr" "prn" "print" "println"
    "type" "class" "symbol" "symbol?"
    "function?" "var" "gershwin-var" "if" "if-not" "when" "when-not" "?" "or" "and"
    "re-pattern" "str" "str2" "str3" "take" "map" "reduce" "reduce-with" "filter" "remove"
    "in-ns" "first" "second" "last" "peek")
  "Words built into the core of Gershwin")

(defun gershwin-open-quotation () (interactive) (insert "\u00ab"))
(defun gershwin-close-quotation () (interactive) (insert "\u00bb"))

(defvar gershwin-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
    (define-key map (kbd "C-M-,") 'gershwin-open-quotation)
    (define-key map (kbd "C-M-.") 'gershwin-close-quotation)
    map)
  "Keymap for Gershwin mode. Inherits from `clojure-mode-map'.")

(defun gershwin-enable-nrepl ()
  "Turn on nrepl interaction mode (see command `nrepl-interaction-mode').
Useful in hooks."
  (nrepl-interaction-mode 1)
  (setq next-error-function 'nrepl-jump-to-compilation-error))

(defun gershwin-disable-nrepl ()
  "Turn off nrepl interaction mode (see command `nrepl-interaction-mode').
Useful in hooks."
  (nrepl-interaction-mode -1))

;;;###autoload
(defun nrepl-enable-on-existing-gershwin-buffers ()
  "Enable interaction mode on existing Gershwin buffers.
See command `nrepl-interaction-mode'."
  (interactive)
  (add-hook 'gershwin-mode-hook 'gershwin-enable-nrepl)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'gershwin-mode)
          (gershwin-enable-nrepl))))))

;;;###autoload
(defun nrepl-disable-on-existing-gershwin-buffers ()
  "Disable interaction mode on existing Gershwin buffers.
See command `nrepl-interaction-mode'."
  (interactive)
  (save-window-excursion
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (eq major-mode 'gershwin-mode)
          (setq nrepl-buffer-ns "user")
          (gershwin-disable-nrepl))))))

(defun nrepl-possibly-disable-on-existing-gershwin-buffers ()
  "If not connected, disable nrepl interaction mode on existing clojure buffers."
  (when (not (nrepl-current-connection-buffer))
    (nrepl-disable-on-existing-gershwin-buffers)))

;;;###autoload
(define-derived-mode gershwin-mode
  clojure-mode "Gershwin"
  "Major mode for Gershwin code"

  ;; Key bindings (Mode map)
  (use-local-map gershwin-mode-map)

  ;; Syntax highlighting for built-in language forms
  (cl-flet ((escape-chars
             (word)
             (replace-regexp-in-string
              (regexp-opt '("*" "+" "?"))
              (lambda (match)
                (concat "\\\\" match))
              word)))
    (font-lock-add-keywords
     'gershwin-mode
     (append '(": ")
             (mapcar (lambda (item)
                       (concat "\\b" item "\\b"))
                     (mapcar (lambda (x)
                               (escape-chars x))
                             gershwin-builtins)))))

  ;; Treat left and right guillemets as proper delimiters
  (add-hook 'gershwin-mode-hook (lambda ()
                                  (modify-syntax-entry ?« "(>" )
                                  (modify-syntax-entry ?» ")<" )))

  ;; nREPL integration
  (add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-gershwin-buffers)
  (add-hook 'nrepl-disconnected-hook
            'nrepl-possibly-disable-on-existing-gershwin-buffers)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gwn$" . gershwin-mode))

(provide 'gershwin-mode)
