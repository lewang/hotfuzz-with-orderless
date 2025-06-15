;;; hotfuzz-with-orderless.el --- Fuzzy + orderless -*- lexical-binding: t; -*-

;; Copyright (C) Le Wang

;; Author: Le Wang
;; Version: 0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: matching
;; URL: https://
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; See readme.org for philosophy.

;; To use this style, prepend `hotfuzz+orderless' to `completion-styles'.

;;; Code:


(require 'hotfuzz)
(require 'orderless)


(defun hotfuzz+orderless--split (str)
  "Split string into two (hotfuzz-query . orderless-query).

`hotfuzz-query' is the first word.
`orderless-query' is the remainder, or the entire word if str starts with a space"
  (pcase str
    ((rx string-start
	 (let hotfuzz-query
	   (seq (* (not ? ))
		(or (* ? ) string-end)))
	  (let orderless-query
	    (* anychar)))
     (cons (string-trim hotfuzz-query) (string-trim orderless-query)))))

;;;###autoload
(defun hotfuzz+orderless-all-completions (str table pred pt)
  (message "hotfuzz+orderless-all-completions args %S" (list str table pred pt))
  (pcase-let* ((`(,hotfuzz-query . ,orderless-query) (hotfuzz+orderless--split str))
	       ;; nconc removex replaces cdr of list with nil
	       (orderless-completions (nconc (orderless-all-completions orderless-query table pred (length orderless-query))
					     nil)))
    (message "orderless-completions: %S" orderless-completions)
    (defvar le::mid orderless-completions)
    (defvar le::t table)
    (defvar le::pred pred)
    ;; (hotfuzz-all-completions hotfuzz-query
    ;; 			     orderless-completions
    ;; 			     pred
    ;; 			     (length hotfuzz-query))
    (hotfuzz-all-completions hotfuzz-query
			     table
			     pred
			     (length hotfuzz-query))))

;;;###autoload
(progn
  (add-to-list 'completion-styles-alist
               '(hotfuzz+orderless orderless-try-completion hotfuzz+orderless-all-completions
				   "Filter first component with Hotfuzz, rest with Orderless."))
  (put 'hotfuzz+orderless 'completion--adjust-metadata #'hotfuzz--adjust-metadata))

(provide 'hotfuzz+orderless)
