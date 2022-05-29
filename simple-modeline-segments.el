;;; simple-modeline-segments.el --- The segments for simple-modeline -*- lexical-binding: t; -*-

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

;; The segments for simple-modeline

;;; Code:

(require 'subr-x)

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

(defface simple-modeline-project-face
  '((t (:inherit font-lock-constant-face)))
  "Face used for showing the size of the region."
  :group 'simple-modeline)

(defface simple-modeline-encoding-face
  '((t (:inherit mode-line)))
  "Face used for showing encoding style."
  :group 'simple-modeline)

(defface simple-modeline-eol-face
  '((t (:inherit mode-line)))
  "Face used for showing EOL style."
  :group 'simple-modeline)

(defface simple-modeline-input-method-face
  '((t (:inherit mode-line)))
  "Face used for showing EOL style."
  :group 'simple-modeline)

(defun simple-modeline-make-mouse-map (mouse function)
  "Return a keymap with single entry for mouse key MOUSE on the mode line.
MOUSE is defined to run function FUNCTION with no args in the buffer
corresponding to the mode line clicked."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun simple-modeline-segment-modified ()
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

(defun simple-modeline-segment-buffer-name ()
  "Displays the name of the current buffer in the mode-line."
  (propertize " %b"
              'face 'simple-modeline-buffer-name-face
              'mouse-face 'mode-line-highlight))

(defun simple-modeline-segment-position ()
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
                     'font-lock-face 'simple-modeline-position-face))))

(defun simple-modeline--get-version-string (file)
  "Get a version string to display for FILE.

This is the normal vc mode-line text with the backend name stripped.
We keep the text properties."
  (when-let*
      ((backend (ignore-errors (vc-responsible-backend file nil)))
       (vc-string (vc-call-backend backend 'mode-line-string file)))
    
    (substring vc-string (length (symbol-name backend)) (length vc-string) )))

(defun simple-modeline--make-project-map ()
  "Create a keymap with bindings for either magit or vc.

Assumes we are in a project that's under version control."
  (if (and (featurep 'magit)
           (magit-git-repo-p (project-root (project-current))))
      (let ((map (make-sparse-keymap)))
        (define-key map [mode-line mouse-1] 'magit-file-dispatch)
        (define-key map [mode-line mouse-3] 'magit-status)
        (purecopy map))

    vc-mode-line-map))

(defun simple-modeline--make-help-echo (project-root vc-help)
  "Create a help text including the correct bindings for the mouse map.

Assumes we are in a project with root PROJECT-ROOT. Include the
help text from vc as given by VC-HELP."
  (concat "Project: " project-root
          (unless (string-empty-p vc-help)
            (concat "\n" vc-help
                    (if (and (featurep 'magit)
                             (magit-git-repo-p project-root))
                        "\nmouse-1: Magit File Dispatch\nmouse-2: Magit Status"
                      "\nmouse-1: Version Control Menu")))))

(defun simple-modeline-segment-project ()
  "Display current project name in modeline, followed by vc info.

This is a replacement for the standard `vc-mode' modeline info. Instead of the
vc backend it shows the name of the root directory of the project.

If magit is detected and we are in a Git repo, it will use magit bindings
for the mouse map."
  (when (require 'project)
    (when-let ((file (if (eq major-mode 'dired-mode)
                         default-directory
                       (buffer-file-name)))
               (proj (project-current))
               (root (project-root proj))
               (proj-name (file-name-nondirectory (directory-file-name root)))
               (trunc-name (truncate-string-to-width proj-name 15 nil nil "..")))
      (let ((version-string (simple-modeline--get-version-string file))
            (help-echo "")
            (local-map nil))
        (when version-string
          (setq local-map (simple-modeline--make-project-map))
          (setq help-echo (get-text-property 0 'help-echo version-string)))
        
        `((:propertize ,(concat " " trunc-name version-string)
                       face simple-modeline-project-face
                       mouse-face 'mode-line-highlight
                       help-echo ,(simple-modeline--make-help-echo root help-echo)
                       local-map ,local-map))))))

(defun simple-modeline-segment-vc ()
  "displays color-coded version control information in the mode-line."
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

(defun simple-modeline-segment-encoding ()
  "Displays the encoding style of the buffer in the mode-line."
  `(" "
    ,(propertize
      "%z"
      'help-echo
      (lambda (window &rest args)
        (with-current-buffer (window-buffer window)
          (if buffer-file-coding-system
              (format "Buffer coding system (%s): %s\nmouse-1: Describe coding system\nmouse-3: Set coding system"
                      (if enable-multibyte-characters "multi-byte" "unibyte")
                      (symbol-name buffer-file-coding-system))
            "Buffer coding system: none specified")))
      'face 'simple-modeline-encoding-face
      'mouse-face 'mode-line-highlight
      'local-map simple-modeline-segment-encoding-map)))

(defun simple-modeline-segment-eol ()
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
     'face 'simple-modeline-eol-face
     'mouse-face 'mode-line-highlight)))

(defun simple-modeline-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (let ((misc-info (string-trim (format-mode-line mode-line-misc-info 'simple-modeline-unimportant))))
    (unless (string= misc-info "")
      (concat " " misc-info))))

(defun simple-modeline-segment-input-method ()
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
                  mouse-face 'mode-line-highlight
                  face 'simple-modeline-input-method-face))))

(defun simple-modeline-segment-minor-modes ()
  "Displays the current minor modes in the mode-line."
  (replace-regexp-in-string
   "%" "%%%%"
   (format-mode-line minor-mode-alist)
   t t))

(defun simple-modeline-segment-process ()
  "Displays the current value of `mode-line-process' in the mode-line."
  (when mode-line-process
    (concat " " (string-trim (format-mode-line mode-line-process)))))

(defun simple-modeline-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize
   (concat " "
           (or (and (boundp 'delighted-modes)
                    (cadr (assq major-mode delighted-modes)))
               (format-mode-line mode-name)))
   'face 'simple-modeline-major-mode-face
   'mouse-face 'mode-line-highlight))

(defcustom simple-modeline-word-count-modes '(markdown-mode gfm-mode org-mode)
  "Major modes in which to display word count continuously."
  :type '(repeat (symbol :tag "Major-Mode") )
  :group 'simple-modeline)

(defun simple-modeline-segment-word-count ()
  "Display the buffer word count in the mode-line when in a major mode in `simple-modeline-word-count-modes'."
  (if (member major-mode simple-modeline-word-count-modes)
      (format " %dW" (count-words (point-min) (point-max)))))

(provide 'simple-modeline-segments)
;;; simple-modeline-segments.el ends here
