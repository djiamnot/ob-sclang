;;; ob-sclang.el --- org-babel functions for sclang evaluation

;; Copyright (C) 2018 Michal Seta

;; Author: Michal Seta
;; Keywords: supercollider, literate programming, reproducible research
;; Homepage: https://github.com/djiamnot/ob-sclang.git
;; Version: 0.01

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

;;; Requirements:
;;; sclang requires SuperCollider, of course. You need to have both the SuperCollider
;;; system installed and the sclang-mode
;;; SuperCollider can be found at https://github.com/supercollider/supercollider

;;; Code:
(require 'ob)
(require 'sclang)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("sclang" . "scd"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:sclang '())

(defun org-babel-expand-body:sclang (body params)
;;   "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params)))
    (mapc
     (lambda (pair)
       (let ((name (symbol-name (car pair)))
             (value (cdr pair)))
         (setq body
               (replace-regexp-in-string
                (regexp-quote name)
                (cond
                 ((stringp value) (format "%S" value))
                 ((floatp value) (format "%f" value))
                 ((integerp value) (format "%d" value))
                 ((symbolp value) (concat (format "%S" (symbol-name value)) ".asSymbol"))
                 )
                body))))
     vars)
    body))

(defun org-babel-execute:sclang (body params)
  "Execute a block of Sclang code with org-babel.
This function is called by `org-babel-execute-src-block'"

  (sclang-eval-string (org-babel-expand-body:sclang body params))
  )

(defun org-babel-prep-session:sclang (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "No need for session preparation")
  )

(defun org-babel-sclang-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    (let (session (if sclang-library-initialized-p
                      sclang-post-buffer
                    (save-window-excursion
                      (sclang-start)
                      (current-buffer) ))))))

(provide 'ob-sclang)
;;; ob-sclang.el ends here
