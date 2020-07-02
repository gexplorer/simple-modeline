;;; simple-modeline-segments.el --- The segments for simple-modeline -*- lexical-binding: t; -*-

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

;; The segments for simple-modeline

;;; Code:

(require 'simple-modeline-core)
(require 'subr-x)

(defvar minions-direct)
(defvar minions-mode-line-lighter)
(defvar minions-mode-line-minor-modes-map)

(defun simple-modeline-make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(simple-modeline-create-segment
 "modified"
 "Displays a color-coded buffer modification/read-only indicator in the mode-line."
 (if (not (string-match-p "\\*.*\\*" (buffer-name)))
     (let* ((read-only (and buffer-read-only (buffer-file-name)))
            (modified (buffer-modified-p)))
       (propertize
        (if read-only " " (if modified " ●" " ○"))
        'face `(:inherit
                ,(if modified 'simple-modeline-status-modified
                   (if read-only 'simple-modeline-status-error
                     'simple-modeline-unimportant)))
        'help-echo (format
                    "Buffer is %s and %smodified\nmouse-1: Toggle read-only status."
                    (if read-only "read-only" "writable")
                    (if modified "" "not "))
        'local-map (purecopy (simple-modeline-make-mouse-map
                              'mouse-1
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (read-only-mode 'toggle)))))
        'mouse-face 'mode-line-highlight))))

(simple-modeline-create-segment
 "buffer-name"
 "Displays the name of the current buffer in the mode-line."
 (propertize " %b" 'face 'mode-line-buffer-id))

(simple-modeline-create-segment
 "position"
 "Displays the current cursor position in the mode-line."
 `((line-number-mode
    ((column-number-mode
      (column-number-indicator-zero-based
       (8 " %l:%c")
       (8 " %l:%C"))
      (5 " L%l")))
    ((column-number-mode
      (column-number-indicator-zero-based
       (5 " C%c")
       (5 " C%C")))))
   ,(if (region-active-p)
        (propertize (format "+%s"
                            (apply #'+ (mapcar
                                       (lambda (pos)
                                         (- (cdr pos)
                                            (car pos)))
                                       (region-bounds))))
                    'font-lock-face 'font-lock-variable-name-face))))

(simple-modeline-create-segment
 "vc"
 "Displays color-coded version control information in the mode-line."
 '(vc-mode vc-mode))

(defvar simple-modeline-segment-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (when (and enable-multibyte-characters
		     buffer-file-coding-system)
	    (describe-coding-system buffer-file-coding-system)))))
    (define-key map [mode-line mouse-3]
      (lambda (e)
	(interactive "e")
	(with-selected-window (posn-window (event-start e))
	  (call-interactively #'set-buffer-file-coding-system))))
    (purecopy map))
  "Local keymap for the coding-system part of the simple-modeline.")

(simple-modeline-create-segment
 "encoding"
 "Displays the encoding style of the buffer in the mode-line."
 `(" "
   ,(propertize
     "%z"
     'help-echo
     (lambda (window)
       (with-current-buffer (window-buffer window)
         (if buffer-file-coding-system
             (format "Buffer coding system (%s): %s\nmouse-1: Describe coding system\nmouse-3: Set coding system"
                     (if enable-multibyte-characters "multi-byte" "unibyte")
                     (symbol-name buffer-file-coding-system))
           "Buffer coding system: none specified")))
     'mouse-face 'mode-line-highlight
     'local-map simple-modeline-segment-encoding-map)))

(simple-modeline-create-segment
 "eol"
 "Displays the EOL style of the current buffer in the mode-line."
 (let* ((eol (coding-system-eol-type buffer-file-coding-system))
        (mnemonic (pcase eol
                    ('0 " LF")
                    ('1 " CRLF")
                    ('2 " CR")
                    (_ "")))
        (desc (pcase eol
                ('0 "Unix-style")
                ('1 "DOS-style")
                ('2 "Mac-style")
                (_ "Undecided"))))
   (propertize
    mnemonic
    'help-echo (format "End-of-line style: %s\nmouse-1: Cycle" desc)
    'local-map (purecopy
                (simple-modeline-make-mouse-map
                 'mouse-1
                 (lambda (event)
                   (interactive "e")
                   (with-selected-window (posn-window (event-start event))
                     (let ((eol (coding-system-eol-type buffer-file-coding-system)))
                       (set-buffer-file-coding-system
                        (cond ((eq eol 0) 'dos) ((eq eol 1) 'mac) (t 'unix))))))))
    'mouse-face 'mode-line-highlight)))

(simple-modeline-create-segment
 "misc-info"
 "Displays the current value of `mode-line-misc-info' in the mode-line."
 (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'simple-modeline-unimportant))))
   (unless (string= misc-info "")
     (concat " " misc-info))))

(simple-modeline-create-segment
 "input-method"
 "Displays the input-method of the buffer in the mode-line."
 `(""
   (current-input-method
    (:propertize (" " current-input-method-title)
                 help-echo (format
                            "Current input method: %s\nmouse-1: Describe current input method"
                            current-input-method)
                 local-map ,(purecopy
                             (simple-modeline-make-mouse-map
                              'mouse-1
                              (lambda (e)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start e))
                                  (describe-current-input-method)))))
                 mouse-face 'mode-line-highlight))))

(simple-modeline-create-segment
 "minor-modes"
 "Displays the current minor modes in the mode-line."
 (let ((direct-alist))
   (dolist (cand-mode minor-mode-alist)
     (if (member (car cand-mode) minions-direct)
       (setq direct-alist (cons cand-mode direct-alist))))
   (replace-regexp-in-string
     "%" "%%%%"
     (format-mode-line direct-alist)
     t t)))

(simple-modeline-create-segment
 "minions-mode"
 "Display minions mode in the mode-line."
 (if (and (boundp 'minions-mode))
   (propertize (format "%s" minions-mode-line-lighter)
     'mouse-face 'mode-line-highlight
     'help-echo "Minions mouse-1: Display minions modes menu"
     'local-map minions-mode-line-minor-modes-map)))

(simple-modeline-create-segment
 "process"
 "Displays the current value of `mode-line-process' in the mode-line."
 (when mode-line-process
   (concat " " (string-trim (format-mode-line mode-line-process)))))

(simple-modeline-create-segment
 "major-mode"
 "Displays the current major mode in the mode-line."
 (propertize
  (concat " "
          (or (and (boundp 'delighted-modes)
                   (cadr (assq major-mode delighted-modes)))
              (format-mode-line mode-name)))
  'face 'bold))

(provide 'simple-modeline-segments)
;;; simple-modeline-segments.el ends here
