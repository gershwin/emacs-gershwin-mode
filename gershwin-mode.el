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
  '("!" "*'" "*" "*gershwin-eval*" "*gershwin-stack*" "*gershwin-version*" "*print-stack*" "+'" "+" "-'" "-" "<=" "<" "==" "=" ">=" ">" "?" "aclone" "add-meta!" "aget" "alength" "alias" "all-ns" "ancestors" "and-greedy" "and" "ap" "apply-swap" "apply2" "apply3" "apply" "aset-boolean" "aset-byte" "aset-char" "aset-double" "aset-float" "aset-int" "aset-long" "aset-short" "aset" "assoc" "bi&" "bi*" "bi2&" "bi2*" "bi2" "bi3" "bi" "bigdec" "bigint" "bit-and-not" "bit-and" "bit-clear" "bit-flip" "bit-not" "bit-or" "bit-set" "bit-shift-left" "bit-shift-right" "bit-test" "bit-xor" "boolean" "both?" "butlast" "byte" "call-swap" "call2" "call3" "call" "cast" "char?" "char" "class" "clear-stack" "clear" "coll?" "compare" "complement" "concat*" "concat0" "concat1" "concat3" "concat4" "concat" "cond" "conj-it" "conj" "cons" "contains?" "count" "create-ns" "create-struct*" "create-struct2" "create-struct3" "create-struct4" "create-struct" "cycle" "dec'" "dec" "decimal?" "declare-word" "delay?" "delay" "denominator" "deref" "dip2" "dip3" "dip4" "dip" "disj-keys" "disj" "dissoc-keys" "dissoc" "distinct" "div" "doseq" "double" "drop2" "drop3" "drop" "dup2" "dup3" "dup4" "dup" "dupd" "each" "either?" "empty?" "ends-with?" "even?" "every?" "false?" "ffirst" "filter" "find-keyword-with-ns" "find-keyword" "find-ns" "find" "first" "float?" "float" "flush" "fnext" "force" "format*" "format2" "format3" "format" "function?" "gensym-with-prefix" "gensym" "gershwin-eval" "gershwin-symbol" "gershwin-version" "get-in" "get-method" "get" "has-any?" "hash-map*" "hash-map2" "hash-map" "hash-set*" "hash-set2" "hash-set3" "hash-set4" "hash-set" "identical?" "identity" "if*" "if-not" "if" "in-ns" "inc'" "inc" "index-of" "instance?" "integer?" "interleave" "interpose" "invoke" "iterate" "keep2" "keep3" "keep4" "keep" "key" "keys" "keyword-with-ns" "keyword?" "keyword" "last-index-of" "last" "line-seq" "list*2" "list*3" "list*4" "list*" "load-file" "load-gershwin-file" "load-reader" "load-string" "locking" "long" "macroexpand-1" "macroexpand" "make-array" "map?" "map" "mapcat" "max" "member?" "meta" "methods" "min" "mod" "name" "namespace" "neg?" "newline" "next" "nfirst" "nil?" "nip2" "nip" "nnext" "not=" "not" "ns-aliases" "ns-imports" "ns-interns" "ns-map" "ns-name" "ns-publics" "ns-refers" "ns-unalias" "ns-unmap" "nth" "num" "number?" "numerator" "odd?" "or" "over2" "over" "peek*" "peek-safe" "peek" "perst-peek" "perst-pop" "pick" "pmap" "pop-it" "pop-n" "pop-n-swap" "pop-n-swap2" "pop-n-swap3" "pos?" "pr" "prefer-method" "prefers" "print-stack" "print-stack" "print" "println" "prn" "ps" "quad2" "quad3" "quad" "quotient" "range-to" "range" "ratio?" "rational?" "rationalize" "re-find" "re-pattern" "re-seq" "read-line" "read-string" "read" "reduce-with" "reduce" "rem" "remove-all-methods" "remove-method" "remove-ns" "remove" "repeat" "repeatedly" "rest" "reverse" "rot" "rseq" "second" "select-keys" "seq?" "seq" "set" "short" "sleep" "slurp" "some" "sort" "sorted-map*" "sorted-map-by*" "sorted-map-by2" "sorted-map-by" "sorted-map2" "sorted-map" "sorted-set*" "sorted-set-by*" "sorted-set-by2" "sorted-set-by3" "sorted-set-by4" "sorted-set-by" "sorted-set2" "sorted-set3" "sorted-set4" "sorted-set" "spit" "st-pop" "stack-clear" "stack-print" "stack-void" "starts-with?" "str*" "str2" "str3" "str" "string?" "substring-from" "substring" "subvec-from" "subvec" "swap" "symbol-with-ns" "symbol?" "symbol" "take" "the-ns" "time-took" "to-array-2d" "to-array" "to-lower-case" "to-upper-case" "tri&" "tri*" "tri2&" "tri2*" "tri2" "tri3" "tri" "true?" "type" "unchecked-add-int" "unchecked-add" "unchecked-byte" "unchecked-char" "unchecked-dec-int" "unchecked-dec" "unchecked-divide-int" "unchecked-double" "unchecked-float" "unchecked-inc-int" "unchecked-inc" "unchecked-int" "unchecked-long" "unchecked-multiply-int" "unchecked-multiply" "unchecked-negate-int" "unchecked-negate" "unchecked-remainder-int" "unchecked-short" "unchecked-subtract-int" "unchecked-subtract" "update-in" "val" "vals" "values" "vec" "vector*" "vector2" "vector3" "vector4" "vector?" "vector" "when-not" "when" "with-gershwin" "with-meta" "with-open" "with-out-str" "word-var" "wrap-vec" "wrap" "zero?")
  "Words built into the core of Gershwin")

(defvar gershwin-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map clojure-mode-map)
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

  ;; nREPL integration
  (add-hook 'nrepl-connected-hook 'nrepl-enable-on-existing-gershwin-buffers)
  (add-hook 'nrepl-disconnected-hook
            'nrepl-possibly-disable-on-existing-gershwin-buffers)
)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.gwn$" . gershwin-mode))

(provide 'gershwin-mode)
