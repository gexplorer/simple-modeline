;;; simple-modeline.el --- A simple mode-line configuration for Emacs -*- lexical-binding: t; -*-

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; URL: https://github.com/gexplorer/simple-modeline
;; Keywords: mode-line faces
;; Version: 1.1
;; Package-Requires: ((emacs "26.1"))

;; Copyright (C) 2019  Eder Elorriaga

;; Author: Eder Elorriaga <gexplorer8@gmail.com>
;; URL: https://github.com/gexplorer/simple-modeline
;; Keywords: mode-line
;; Version: 1.0

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

;; A simple mode-line configuration for Emacs.
;; To enable, put this code in your init file:
;; (require 'simple-modeline)
;; (simple-modeline-mode 1)
;; or
;; (use-package simple-modeline
;;   :ensure t
;;   :hook (after-init . simple-modeline-mode))
;;

;;; Code:

(require 'simple-modeline-core)
(require 'simple-modeline-segments)

(defvar simple-modeline--mode-line
  '((:eval
     (simple-modeline--format
      '(simple-modeline-segment-modified
        simple-modeline-segment-buffer-name
        simple-modeline-segment-position)
      '(simple-modeline-segment-minions-mode
        simple-modeline-segment-minor-modes
        simple-modeline-segment-input-method
        simple-modeline-segment-eol
        simple-modeline-segment-encoding
        simple-modeline-segment-vc
        simple-modeline-segment-misc-info
        simple-modeline-segment-process
        simple-modeline-segment-major-mode)))))

;;;###autoload
(define-minor-mode simple-modeline-mode
  "Minor mode to get a simple mode line.

When called interactively, toggle
`simple-modeline-mode'.  With prefix ARG, enable
`simple-modeline--mode' if ARG is positive, otherwise
disable it.

When called from Lisp, enable `simple-modeline-mode' if ARG is omitted,
nil or positive.  If ARG is `toggle', toggle `simple-modeline-mode'.
Otherwise behave as if called interactively."
  :init-value nil
  :keymap nil
  :lighter ""
  :group 'simple-modeline
  :global t
  (if simple-modeline-mode
      (progn
        ;; Set the new mode-line-format
        (setq-default mode-line-format '(:eval simple-modeline--mode-line)))
    (progn
      ;; Restore the original mode-line format
      (setq-default mode-line-format simple-modeline--default-mode-line))))

(provide 'simple-modeline)
;;; simple-modeline.el ends here
