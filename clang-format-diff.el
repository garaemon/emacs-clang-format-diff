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
  "Path to clang-format executable."
  :type 'string
  :group 'clang-format-diff)

(defcustom clang-format-diff-clang-options nil
  "Options for clang-format."
  :type 'string
  :group 'clang-format-diff)

(defcustom clang-format-diff-enable-buffer-mode t
  "Use c++-mode to colorize buffer which clang-format applied."
  :type 'boolean
  :group 'clang-format-diff)

(defun clang-format-diff-view-region (char-start char-end)
  "Apply clang-format to selected region and merge the result by ediff."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point) (point))))

  (let ((start (1- (position-bytes char-start)))
        (end (1- (position-bytes char-end)))
        (cursor (1- (position-bytes (point))))
        (temp-buffer (get-buffer-create "*clang-format-diff-buffer*"))
        (exe (or clang-format-diff-clang-executable
                 (executable-find "clang-format"))))
    (with-current-buffer temp-buffer
      (erase-buffer))
    (let (status stderr operations)
      (setq status
            (apply #'call-process-region
                   (point-min) (point-max) exe
                   nil temp-buffer nil
                   ;;"-assume-filename" (or (buffer-file-name) "")
                   "-offset" (number-to-string start)
                   "-length" (number-to-string (- end start))
                   "-cursor" (number-to-string cursor)
                   clang-format-diff-clang-options))
      ;; temp-buffer has '{ "Cursor": 50, "IncompleteFormat": false }'
      ;; at the top.
      (with-current-buffer temp-buffer
        (delete-region (progn (goto-line 1) (beginning-of-line) (point))
                       (progn (goto-line 2) (beginning-of-line) (point))))
                       ;;(progn (end-of-line) (point))))
      (if clang-format-diff-enable-buffer-mode
          (with-current-buffer temp-buffer
            (c++-mode)))
      ;; check difference between two buffer first
      (if (clang-format-diff-have-difference (current-buffer) temp-buffer)
          (ediff-buffers (current-buffer) temp-buffer)
        (message "No need to fix! Have a good luck!"))
      )
    ))

(defun clang-format-diff-have-difference (buffer-a buffer-b)
  "Return true if two buffer has difference."
  (not (string= (with-current-buffer buffer-a
                  (buffer-string))
                (with-current-buffer buffer-b
                  (buffer-string)))))

(defun clang-format-diff-view ()
  "Apply clang-format to current buffer and merge the result by ediff."
  (interactive)
  (message "use-region-p: %s" (use-region-p))
  (if (not (use-region-p))
      (clang-format-diff-view-region (point-min) (point-max))
    (clang-format-diff-view-region (region-beginning) (region-end))))

(global-set-key "\M-[" 'clang-format-diff-view)

(defvar clang-format-diff-ediff-last-windows nil)

(defun clang-format-diff-store-pre-ediff-winconfig ()
  (setq clang-format-diff-ediff-last-windows (current-window-configuration)))

(defun clang-format-diff-restore-pre-ediff-winconfig ()
  (set-window-configuration clang-format-diff-ediff-last-windows))

(add-hook 'ediff-before-setup-hook #'clang-format-diff-store-pre-ediff-winconfig)
(add-hook 'ediff-quit-hook #'clang-format-diff-restore-pre-ediff-winconfig)

(provide 'clang-format-diff)
;;; clang-format-diff.el ends here
