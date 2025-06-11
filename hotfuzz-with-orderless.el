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

;; To use this style, prepend `hotfuzz-with-orderless' to `completion-styles'.

;;; Code:


(require 'hotfuzz)
(require 'orderless)

(defun orderless-flex-if-twiddle (pattern _index _total)
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun oderless-first-flex (_pattern index _total)
  (if (= index 0) 'orderless-flex))

(defun order-less-not-if-bang (pattern _index _total)
  (cond
   ((equal "!" pattern)
    #'ignore)
   ((string-prefix-p "!" pattern)
    `(orderless-not . ,(substring pattern 1)))))

(setq orderless-matching-styles '(orderless-regexp)
      orderless-style-dispatchers '(oderless-first-flex
                                    orderless-flex-if-twiddle
                                    order-less-not-if-bang))

(defun hotfuzz-with-orderless--dispatch (n str)
 "Use hotfuzz Nth function from completion-styles-alist if there is no space in STR, else use orderless"
  (let ((hotfuzz-fn (nth n (assoc 'hotfuzz completion-styles-alist)))
	(orderless-fn (nth n (assoc 'orderless completion-styles-alist))))
    (if (string-match-p " " str)
	orderless-fn
      hotfuzz-fn)))

;;;###autoload
(defun hotfuzz-with-orderless-try-completion (str &rest args)
  "dispatch between hotfuzz and orderless"
  (apply (hotfuzz-with-orderless--dispatch 1 str) str args))

;;;###autoload
(defun hotfuzz-with-orderless-all-completions (str &rest args)
  "dispatch between hotfuzz and orderless"
  (apply (hotfuzz-with-orderless--dispatch 2 str) str args))


;;;###autoload
(progn
  (add-to-list 'completion-styles-alist
               '(hotfuzz-with-orderless hotfuzz-with-orderless-try-completion hotfuzz-with-orderless-all-completions
				   "use hotfuzz if no space, else dispatch to orderless."))
  ;; Why is the Emacs completion API so cursed?
  (put 'hotfuzz-with-orderless 'completion--adjust-metadata #'hotfuzz--adjust-metadata))

(provide 'hotfuzz-with-orderless)
