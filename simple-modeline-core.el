;;; simple-modeline-core.el --- The core libraries for simple-modeline

;; Copyright (C) 2019  Eder Elorriaga

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The core libraries for simple-modeline.

;;; Code:

(defgroup simple-modeline nil
  "A simple mode line."
  :prefix "simple-modeline-"
  :group 'mode-line)

(defvar simple-modeline--default-mode-line mode-line-format
  "The former value of `mode-line-format'.")

;;
;; Faces
;;

(defface simple-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements.")

(defface simple-modeline-status-modified
  '((t (:inherit (font-lock-variable-name-face))))
  "Face used for the 'modified' indicator symbol in the mode-line.")

(defface simple-modeline-status-info
  '((t (:inherit (font-lock-string-face))))
  "Face used for generic status indicators in the mode-line.")

(defface simple-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface simple-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface simple-modeline-status-error
  '((t (:inherit (error))))
  "Face for error stauts indicators in the mode-line.")

;;
;; Helpers
;;

(defmacro simple-modeline-create-segment (name doc &rest body)
  "Create a new segment function for `simple-modeline-mode'"
  (let ((segment (intern (format "simple-modeline-segment-%s" name)))
        (toggle (intern (format "simple-modeline-toggle-%s" name)))
        (show (intern (format "simple-modeline-show-%s" name))))
    `(progn
       (defcustom ,show t
         ,(format "Visibility of the %s segment of the mode-line." name)
         :group 'simple-modeline
         :type 'boolean)
       (defun ,toggle ()
         ,(format "Toggle visibility of %s segment of the mode-line." name)
         (interactive)
         (customize-save-variable (quote ,show) (not ,show)))
       (defalias
         (quote ,segment)
         (lambda ()
           (when ,show
             ,@body))
         ,doc))
    ))

(defun simple-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (simple-modeline--format-segments left-segments))
         (right (simple-modeline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " " 'display `((space :align-to (- right ,reserve))))
     right)))

(defun simple-modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(provide 'simple-modeline-core)
;;; simple-modeline-core.el ends here
