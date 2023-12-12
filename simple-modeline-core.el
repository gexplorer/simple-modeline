;;; simple-modeline-core.el --- The core libraries for simple-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  Eder Elorriaga

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
;; Options
;;

(defcustom simple-modeline-segments
  '((simple-modeline-segment-modified
     simple-modeline-segment-buffer-name
     simple-modeline-segment-position)
    (simple-modeline-segment-minor-modes
     simple-modeline-segment-input-method
     simple-modeline-segment-eol
     simple-modeline-segment-encoding
     simple-modeline-segment-vc
     simple-modeline-segment-misc-info
     simple-modeline-segment-process
     simple-modeline-segment-major-mode))
  "Simple modeline segments.

For a very clean look, you can decide to remove `simple-modeline-segment-minor-modes' from this list. In that case it might be a good idea to add `simple-modeline-segment-narrow' and `simple-modeline-segment-major-mode-with-recursion'."
  :type '(list (repeat :tag "Left aligned" function)
               (repeat :tag "Right aligned" function))
  :package-version '(simple-modeline . "1.2"))

;;
;; Faces
;;

(defface simple-modeline-space
  '((t))
  "Face for space used to alight the right segments in the mode-line.")

(defface simple-modeline-unimportant
  '((t (:inherit (shadow))))
  "Face for less important mode-line elements.")

(defface simple-modeline-status-modified
  '((t (:inherit (font-lock-variable-name-face))))
  "Face for the 'modified' indicator symbol in the mode-line.")

(defface simple-modeline-status-info
  '((t (:inherit (font-lock-string-face))))
  "Face for generic status indicators in the mode-line.")

(defface simple-modeline-status-success
  '((t (:inherit (success))))
  "Face used for success status indicators in the mode-line.")

(defface simple-modeline-status-warning
  '((t (:inherit (warning))))
  "Face for warning status indicators in the mode-line.")

(defface simple-modeline-status-error
  '((t (:inherit (error))))
  "Face for error status indicators in the mode-line.")

(defface simple-modeline-buffer-name-face
  '((t (:inherit mode-line-buffer-id)))
  "Face used for the buffer name segment."
  :group 'simple-modeline)

(defface simple-modeline-major-mode-face
  '((t (:inherit bold)))
  "Face used by the major-mode segment."
  :group 'simple-modeline)

(defface simple-modeline-position-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for showing the size of the region."
  :group 'simple-modeline)

(defface simple-modeline-narrow-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for showing when the buffer is narrowed."
  :group 'simple-modeline)

(defface simple-modeline-project-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for showing the size of the region."
  :group 'simple-modeline)

(defface simple-modeline-encoding-face
  '((t (:inherit mode-line-active)))
  "Face used for showing encoding style."
  :group 'simple-modeline)

(defface simple-modeline-eol-face
  '((t (:inherit mode-line-active)))
  "Face used for showing EOL style."
  :group 'simple-modeline)

(defface simple-modeline-input-method-face
  '((t (:inherit mode-line-active)))
  "Face used for showing EOL style."
  :group 'simple-modeline)
;;
;; Helpers
;;

(defun simple-modeline--format (left-segments right-segments)
  "Return a string of `window-width' length containing LEFT-SEGMENTS and RIGHT-SEGMENTS, aligned respectively."
  (let* ((left (simple-modeline--format-segments left-segments))
         (right (simple-modeline--format-segments right-segments))
         (reserve (length right)))
    (concat
     left
     (propertize " "
                 'display `((space :align-to (- right ,reserve)))
                 'face '(:inherit simple-modeline-space))
     right)))

(defun simple-modeline--format-segments (segments)
  "Return a string from a list of SEGMENTS."
  (format-mode-line (mapcar
                     (lambda (segment)
                       `(:eval (,segment)))
                     segments)))

(provide 'simple-modeline-core)
;;; simple-modeline-core.el ends here
