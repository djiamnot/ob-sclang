;;; ob-sclang.el --- Org-babel interface for SuperCollider  -*- coding: utf-8;

;; Copyright (C) 2018 Michal Seta

;; Author: Michal Seta
;; Keywords: supercollider, literate programming, multimedia, languages, tools
;; URL: https://github.com/djiamnot/ob-sclang
;; Package-Requires: ((emacs "26.1") (sclang "1.0"))
;; Version: 0.3

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; ob-sclang enables SuperCollider source blocks to be used in org files


;;; Code:
(require 'ob)
(require 'sclang)

;; add file extensions for sclang
(add-to-list 'org-babel-tangle-lang-exts '("sclang" . "scd"))

;; declare default header arguments for sclang code blocks
(defvar org-babel-default-header-args:sclang
  '((:session . "*SCLang:Workspace*")
    (:result . "none")))

(defun org-babel-expand-body:sclang (body params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
             (value (cdr pair)))
         (setq body
               (replace-regexp-in-string
                (regexp-quote name)
                (org-babel-sclang-var-to-sclang value)
                body))))
     vars)
    body))

(defun org-babel-sclang-var-to-sclang (var)
  "Convert an elisp value VAR to a string of sclang code.
The value of the variable should be represented with correct type."
  (if (listp var)
      (concat "[" (mapconcat #'org-babel-sclang-var-to-sclang var ", ") "]")
    (cond ((stringp var) (format "%S" var))
          ((floatp var) (format "%f" var))
          ((integerp var) (format "%d" var))
          ((symbolp var) (concat (format "%S" (symbol-name var)) ".asSymbol")))))

(defun org-babel-execute:sclang (body params)
  "Execute a block of sclang code with org-babel.
This function is called by `org-babel-execute-src-block' with BODY and PARAMS"
  (sclang-eval-string (org-babel-expand-body:sclang body params)))

(defun org-babel-prep-session:sclang (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (message "No need for session preparation"))

(defun org-babel-sclang-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create one.
Return the initialized session."
  (unless (string= session "none")
    (let ((session (if sclang-library-initialized-p
                      sclang-post-buffer
                    (save-window-excursion
                      (sclang-start)
                      (current-buffer)))))
      session)))

(provide 'ob-sclang)
;;; ob-sclang.el ends here
