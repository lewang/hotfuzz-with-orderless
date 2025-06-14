;;; test-hotfuzz+orderless.el --- Tests for hotfuzz+orderless -*- lexical-binding: t; -*-

;; Copyright (C) Le Wang

;; Author: Le Wang
;; Keywords: testing

;;; Commentary:

;; Tests for hotfuzz+orderless package using ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(require 'hotfuzz+orderless)

(ert-deftest hotfuzz+orderless--split-single-word ()
  "Test splitting a single word returns it as hotfuzz query with empty orderless query."
  (should (equal (hotfuzz+orderless--split "hello")
                 '("hello" ""))))

(ert-deftest hotfuzz+orderless--split-word-with-trailing-space ()
  "Test splitting word with trailing space trims the space from hotfuzz query."
  (should (equal (hotfuzz+orderless--split "hello ")
                 '("hello" ""))))

(ert-deftest hotfuzz+orderless--split-word-with-trailing-spaces ()
  "Test splitting word with multiple trailing spaces trims them."
  (should (equal (hotfuzz+orderless--split "hello   ")
                 '("hello" ""))))

(ert-deftest hotfuzz+orderless--split-two-words ()
  "Test splitting two words separates first word as hotfuzz query, trimming spaces."
  (should (equal (hotfuzz+orderless--split "hello world")
                 '("hello" "world"))))

(ert-deftest hotfuzz+orderless--split-multiple-words ()
  "Test splitting multiple words keeps first word as hotfuzz query, trims spaces."
  (should (equal (hotfuzz+orderless--split "hello world foo bar")
                 '("hello" "world foo bar"))))

(ert-deftest hotfuzz+orderless--split-leading-space ()
  "Test string starting with space treats all as hotfuzz query, trimmed."
  (should (equal (hotfuzz+orderless--split " hello world")
                 '("" "hello world"))))

(ert-deftest hotfuzz+orderless--split-leading-spaces ()
  "Test string starting with multiple spaces, trimmed."
  (should (equal (hotfuzz+orderless--split "   hello world")
                 '("" "hello world"))))

(ert-deftest hotfuzz+orderless--split-only-spaces ()
  "Test string with only spaces returns empty after trimming."
  (should (equal (hotfuzz+orderless--split "   ")
                 '("" ""))))

(ert-deftest hotfuzz+orderless--split-empty-string ()
  "Test empty string returns empty queries."
  (should (equal (hotfuzz+orderless--split "")
                 '("" ""))))

(ert-deftest hotfuzz+orderless--split-word-space-word ()
  "Test word followed by space and another word, spaces trimmed."
  (should (equal (hotfuzz+orderless--split "foo bar")
                 '("foo" "bar"))))

(ert-deftest hotfuzz+orderless--split-complex-orderless-query ()
  "Test complex orderless query after first word, spaces trimmed."
  (should (equal (hotfuzz+orderless--split "test one two three")
                 '("test" "one two three"))))

(provide 'test-hotfuzz+orderless)

;;; test-hotfuzz+orderless.el ends here
