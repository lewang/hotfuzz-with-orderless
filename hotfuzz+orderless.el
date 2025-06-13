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

(defun hotfuzz+orderless [str]
  (let* ((orderless-start (progn
			    (string-match (rx string-start
					      (or (seq (+ word) (* ? ))
						  (+ ? )))
					  str)
			    (match-end 0)))
	 (orderless-str (substring str orderless-start))
	 (hotfuzz-str (string-trim (substring str 0 orderless-start))))
    (list)
    (hotfuzz-all-completions hotfuzz-str orderless-completions pred (length hotfuzz-str))))

;;;###autoload
(defun hotfuzz+orderless-all-completions (str table pred pt)
  (let* ((orderless-start (progn
			    (string-match (rx string-start
					      (or (seq (+ word) (* ? ))
						  (+ ? )))
					  str)
			    (match-end 0)))
	 (orderless-str (substring str orderless-start))
	 (hotfuzz-str (string-trim (substring str 0 orderless-start)))
	 (orderless-completions (orderless-all-completions orderless-str table pred (length orderless-str))))
    (hotfuzz-all-completions hotfuzz-str orderless-completions pred (length hotfuzz-str))))

;;;###autoload
(progn
  (add-to-list 'completion-styles-alist
               '( hotfuzz+orderless orderless-try-completion hotfuzz+orderless-all-completions
                  "Filter first component with Hotfuzz, rest with Orderless."))
  (put 'hotfuzz+orderless 'completion--adjust-metadata #'hotfuzz--adjust-metadata))

(provide 'hotfuzz+orderless)
