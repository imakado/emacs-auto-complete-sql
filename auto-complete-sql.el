;;; auto-complete-sql.el --- edbi's sql completion in any mode

;; Copyright (C) 2014 by IMAKADO


;; Prefix: acsql:
;; Author: Kenji Imakado <ken.imakado -at- gmail.com>
;; Maintainer: imakado
;; Created: :2014-10-29
;; Keywords: 
;; URL:
;; Version: 0.0.1
;; Package-Requires: ((imakado "0.12") (edbi "0.1.3") (auto-complete "1.4.0"))


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
;; I recommend install emacs-edbi-complete-columns.el: https://github.com/imakado/emacs-edbi-complete-columns

;; Note: This program is very experimental.

;; Installation:

;; Following lines are sample config:

;; (add-hook  'ruby-mode-hook
;;            'my-acsql-setup)
;; (add-hook  'perl-mode-hook
;;            'my-acsql-setup)

;; (defun my-acsql-setup ()
;;   (interactive)

;;   ;; complete sql only inside string
;;   ;; (setq acsql:complete-sql-determiner 'acsql:mode-inside-string)
;;   ;; complete sql every state
;;   (setq acsql:complete-sql-determiner t)

;;   (setq ac-sources `(acsql:ac-source-edbi:tables
;;                      acsql:ac-source-edbi:columns
;;                      acsql:ac-source-edbi:types
;;                      acsql:ac-source-edbi:keywords
;;                      ,@ac-sources
;;                      ))
;;   (define-key (current-local-map) (kbd "C-M-i") 'acsql:complete-sql)
;;   (define-key (current-local-map) (kbd ".") 'acsql:ac-dot-complete))


;;; Code:

(require 'imakado)
(eval-when-compile
  (require 'cl))
(require 'auto-complete)
(require 'edbi)

(defvar acsql:complete-sql-determiner
  'acsql:mode-inside-string)

(defmacro acsql:with-edbi-connection (&rest body)
  (declare (debug (body)))
  `(when (and edbi:dbview-buffer-name
              (buffer-live-p (get-buffer edbi:dbview-buffer-name)))
     (with-current-buffer edbi:dbview-buffer-name
       (when edbi:connection
         ,@body))))

(defun acsql:ac-source-edbi:tables-prefix ()
  (when (etypecase acsql:complete-sql-determiner
          (function (funcall acsql:complete-sql-determiner))
          (boolean acsql:complete-sql-determiner))
    (with-syntax-table sql-mode-syntax-table
      (let* ((from (point))
             (start-point (save-excursion 
                            (skip-syntax-backward "w_")
                            (point))))
        (imakado-acond
          (start-point it)
          (t (point)))))))


(defvar acsql:ac-source-edbi:tables
  (quote ((candidates . (lambda ()
                          (acsql:with-edbi-connection
                           (edbi:ac-editor-table-candidates))))
          (candidate-face . edbi:face-ac-table-candidate-face)
          (selection-face . edbi:face-ac-table-selection-face)
          (prefix . acsql:ac-source-edbi:tables-prefix)
          (symbol . "T"))))


(defvar acsql:ac-source-edbi:columns
  (quote ((candidates . (lambda ()
                          (acsql:with-edbi-connection
                           (edbi:ac-editor-column-candidates))))
          (candidate-face . edbi:face-ac-column-candidate-face)
          (selection-face . edbi:face-ac-column-selection-face)
          (prefix . acsql:ac-source-edbi:tables-prefix)
          (symbol . "C"))))

(defvar acsql:ac-source-edbi:types
  (quote ((candidates . (lambda ()
                          (acsql:with-edbi-connection
                           (edbi:ac-editor-type-candidates))))
          (candidate-face . edbi:face-ac-type-candidate-face)
          (selection-face . edbi:face-ac-type-selection-face)
          (prefix . acsql:ac-source-edbi:tables-prefix)
          (symbol . "+"))))

(defvar acsql:ac-source-edbi:keywords
  (quote ((candidates . (lambda ()
                          (acsql:with-edbi-connection
                           (edbi:ac-editor-keyword-candidates))))
          (prefix . acsql:ac-source-edbi:tables-prefix)
          (symbol . "K"))))




(defun acsql:mode-inside-string ()
  "Return non-nil if inside a string.
Actually returns the quote character that begins the string."
  (let ((parse-state (save-excursion
                       (syntax-ppss (point)))))
    (nth 3 parse-state)))

(defun* acsql:add-to-alist
    (alist k v &key (override t))
  (flet ((acsql:add-to-alist-aux
          (k v)
          (imakado-acond
            ((assoc k alist)
             (when override
               (setcdr it v))
             alist)
            (t
             (push `(,k . ,v) alist)
             alist))))
    (acsql:add-to-alist-aux k v)))

(defun acsql:ac-source-overide-require (source)
  (acsql:add-to-alist source 'requires 0))

(defun acsql:complete-sql ()
  (interactive)
  (let* (
         (ac-sources `(
                      ,(acsql:ac-source-overide-require
                        (copy-alist acsql:ac-source-edbi:tables))
                      ,(acsql:ac-source-overide-require
                        (copy-alist acsql:ac-source-edbi:columns))
                      ,(acsql:ac-source-overide-require
                        (copy-alist acsql:ac-source-edbi:keywords))
                      ,(acsql:ac-source-overide-require
                        (copy-alist acsql:ac-source-edbi:types))
                      )
                    ))
    (auto-complete)))

(defadvice edbi-cc:get-columns (around edbi-cc:get-columns-with-edbi-connection activate)
  (acsql:with-edbi-connection
   ad-do-it))

(defvar acsql:ac-source-columns-dot
  '((candidates . (lambda ()
                    (acsql:with-edbi-connection
                     (edbi-cc:ac-editor-column-candidates))))
    (candidate-face . edbi:face-ac-column-candidate-face)
    (selection-face . edbi:face-ac-column-selection-face)
    (symbol . "C")
    (prefix . edbi-cc:ac-completion-prefix)
    (requires . -1)
    ))

(defun acsql:ac-dot-complete ()
  (interactive)
  (with-no-warnings
    (if (require 'edbi-complete-columns nil t)
        (progn 
          (insert ".")
          (let ((ac-sources '(acsql:ac-source-columns-dot)))
            (ac-start)))
      (insert "."))))



(provide 'auto-complete-sql)

;;; auto-complete-sql.el ends here

