;;; flycheck-google-cpplint.el --- Help to comply with the Google C++ Style Guide

;; Copyright (C) 2016  Ryohei Ueda

;; Author: Ryohei Ueda <garaemona@gmail.com>
;; URL: https://github.com/garaemon/emacs-clang-format-diff
;; Version: 1.0
;; Keywords: clang-format, clang, ediff
;; Package-Requires: ()

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This visualizes difference between code of current buffer
;; and formatted code by clang-format in ediff.

;; You need to install clang-format beforehand.
;; http://clang.llvm.org/docs/ClangFormat.html

;;;; Setup

;; (require 'clang-format-diff)
;; (global-set-key "\M-[" 'clang-format-diff-view)
;; ;; if you want to show diff side-by-side style
;; ;; comment in codes below:
;; ;; (custom-set-variables '(ediff-split-window-function 'split-window-horizontally))
;; ;;

;;; Code:

(defgroup clang-format-diff nil
  ""
  :group 'ediff)

(defcustom clang-format-diff-clang-executable nil
  "path to clang-format executable"
  :type 'string
  :group 'clang-format-diff)

(defcustom clang-format-diff-clang-options nil
  "options to clang-format"
  :type 'string
  :group 'clang-format-diff)

(defcustom clang-format-diff-enable-buffer-mode t
  "Use c++-mode to colorize buffer which clang-format applied"
  :type 'boolean
  :group 'clang-format-diff)

(defun clang-format-diff-cleanup ()
  "Close buffer"
  (remove-hook 'ediff-quit-hook 'clang-format-diff-cleanup)
  (ediff-cleanup-mess)
  (kill-buffer "*clang-format-diff-buffer*")
  )

(defun clang-format-diff-view ()
  "Apply clang-format to current buffer and merge the result by ediff"
  (interactive)
  (let ((exe (or clang-format-diff-clang-executable
                 (executable-find "clang-format"))))
    (message exe)
    (let ((temp-buffer (generate-new-buffer "*clang-format-diff-buffer*")))
      (apply #'call-process-region (point-min) (point-max)
                           exe nil temp-buffer nil clang-format-diff-clang-options)
      ;; force to apply c++-mode to colorize buffer
      (if clang-format-diff-enable-buffer-mode
          (with-current-buffer temp-buffer
            (c++-mode)))
      (add-hook 'ediff-quit-hook 'clang-format-diff-cleanup)
      (ediff-buffers (current-buffer) temp-buffer))
    ))

(provide 'clang-format-diff)
