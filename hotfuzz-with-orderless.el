;;; hotfuzz-with-orderless.el --- Fuzzy + orderless -*- lexical-binding: t; -*-

;; Copyright (C) Le Wang

;; Author: Le Wang
;; Version: 0.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: matching
;; URL: https://
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; What is hotfuzz?
;;
;;`flex' style, but with a better scoring algorithm. Specifically, it is
;; non-greedy and ranks completions that match at word; path component; or
;; camelCase boundaries higher.
;;
;; What is orderless?
;;
;; Orderless completion style divides the pattern into space-separated
;; components, and matches candidates that match all of the components in any
;; order.
;;
;; Why together?
;;
;; Hotfuzz and earlier (and slower) flx algorithm offer excellent sorting. It
;; allows the user to always type forward and bubble up the best completion by
;; continuuing typing instead of adjusting completion strategies.
;;
;; Orderless has builtin `orderless-flex' style, which does matching without
;; sorting. Sorting really is mandatory to make flex search useful.
;;
;; Orderless also has builtin `orderless-initialism' style, which completes
;; strictly as initials, which is not flexible or powerful enough to be
;; generallh useful.
;;
;; How do hotfuzz and orderless work together?
;;
;; 1. Starting typing query, hotfuzz is used for superior sorting.
;; 2. Press space, and orderless takes over, but treats the first word before
;;    space as `orderless-flex'.
;; 3. Note this combination means completions can be filter by a list of space
;;    separated words (i.e. the default orderless usage). However, the first
;;    word has special powers.
;;
;;
;; What's the compromise?
;;
;; `orderless-flex' will redo the work hotfuzz did to filter the collection. In
;; practice this is not noticeable.
;;
;;

;; To use this style, prepend `hotfuzz-with-orderless' to `completion-styles'.
;;
;;

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
 "Use hotfuzz Nth function from completion-styles-alist if there is no space in STRING, else use orderless"
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
(defun hotfuzz-with-orderless-all-completions (&rest args)
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
