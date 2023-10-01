;;; doom-modeline-segments.el --- The segments for doom-modeline -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2023 Vincent Zhang

;; This file is not part of GNU Emacs.

;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; The segments for doom-modeline.
;; Use `doom-modeline-def-segment' to create a new segment.
;;

;;; Code:

(require 'doom-modeline-core)
(eval-when-compile
  (require 'cl-lib)
  (require 'seq)
  (require 'subr-x))


;;
;; Externals
;;

(defvar Info-current-file)
(defvar Info-current-node)
(defvar Info-mode-line-node-keymap)
(defvar display-time-string)
(defvar eglot--managed-mode)
(defvar flymake--mode-line-format)
(defvar flymake--state)
(defvar flymake-menu)
(defvar gnus-newsrc-alist)
(defvar gnus-newsrc-hashtb)

(declare-function compilation-goto-in-progress-buffer "compile")
(declare-function eglot "ext:eglot")
(declare-function eglot--major-modes "ext:eglot" t t)
(declare-function eglot--project-nickname "ext:eglot" t t)
(declare-function eglot-clear-status "ext:eglot")
(declare-function eglot-current-server "ext:eglot")
(declare-function eglot-events-buffer "ext:eglot")
(declare-function eglot-forget-pending-continuations "ext:eglot")
(declare-function eglot-managed-p "ext:glot")
(declare-function eglot-reconnect "ext:eglot")
(declare-function eglot-shutdown "ext:eglot")
(declare-function eglot-stderr-buffer "ext:eglot")
(declare-function flymake--diag-type "ext:flymake" t t)
(declare-function flymake--handle-report "ext:flymake")
(declare-function flymake--lookup-type-property "ext:flymake")
(declare-function flymake--state-diags "ext:flymake" t t)
(declare-function flymake-disabled-backends "ext:flymake")
(declare-function flymake-goto-next-error "ext:flymake")
(declare-function flymake-goto-prev-error "ext:flymake")
(declare-function flymake-reporting-backends "ext:flymake")
(declare-function flymake-running-backends "ext:flymake")
(declare-function flymake-show-buffer-diagnostics "ext:flymake")
(declare-function flymake-show-buffer-diagnostics "ext:flymake")
(declare-function flymake-start "ext:flymake")
(declare-function gnus-demon-add-handler "gnus-demon")
(declare-function org-edit-src-save "ext:org-src")
(declare-function tab-bar--current-tab "tab-bar")
(declare-function tab-bar--current-tab-index "tab-bar")



;;
;; Buffer information
;;

(defun doom-modeline-buffer-file-state-icon (text face)
  (doom-modeline-icon text :face face))

(defvar-local doom-modeline--buffer-file-state-icon nil)
(defun doom-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line."
  (setq doom-modeline--buffer-file-state-icon
        (when doom-modeline-buffer-state-icon
          (ignore-errors
            (concat
             (when (or (buffer-narrowed-p)
                       (bound-and-true-p dired-narrow-mode))
               (doom-modeline-buffer-file-state-icon "="
                 'doom-modeline-warning))
             (cond (buffer-read-only
                    (doom-modeline-buffer-file-state-icon "%1*"
                      'doom-modeline-warning))
                   ((and (buffer-modified-p)
                         doom-modeline-buffer-modification-icon)
                    (doom-modeline-buffer-file-state-icon "%1*"
                      'doom-modeline-warning))
                   ((and buffer-file-name
                         ;; Avoid freezing while connection is lost
                         (not (file-remote-p buffer-file-name))
                         (not (file-exists-p buffer-file-name)))
                    (doom-modeline-buffer-file-state-icon "!"
                      'doom-modeline-urgent))
                   (t ""))
             (when (or defining-kbd-macro executing-kbd-macro)
              (doom-modeline-buffer-file-state-icon "@"
                'doom-modeline-warning))
             (when overwrite-mode
              (doom-modeline-buffer-file-state-icon "#"
                'doom-modeline-warning))
             )))))

(defvar-local doom-modeline--buffer-file-name nil)
(defun doom-modeline-update-buffer-file-name (&rest _)
  "Update buffer file name in mode-line."
  (setq doom-modeline--buffer-file-name
        (ignore-errors
          (save-match-data
            (if buffer-file-name
                (doom-modeline-buffer-file-name)
              (propertize "%b"
                          'face 'doom-modeline-buffer-file
                          'mouse-face 'doom-modeline-highlight
                          'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
                          'local-map mode-line-buffer-identification-keymap))))))
(add-hook 'find-file-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'after-save-hook #'doom-modeline-update-buffer-file-name)
;; (add-hook 'clone-indirect-buffer-hook #'doom-modeline-update-buffer-file-name)
(add-hook 'Info-selection-hook #'doom-modeline-update-buffer-file-name)
(advice-add #'rename-buffer :after #'doom-modeline-update-buffer-file-name)
(advice-add #'set-visited-file-name :after #'doom-modeline-update-buffer-file-name)
(advice-add #'pop-to-buffer :after #'doom-modeline-update-buffer-file-name)
;; (advice-add #'primitive-undo :after #'doom-modeline-update-buffer-file-name)
;; (advice-add #'set-buffer-modified-p :after #'doom-modeline-update-buffer-file-name)

(with-no-warnings
  (if (boundp 'after-focus-change-function)
      (progn
        (advice-add #'handle-switch-frame :after #'doom-modeline-update-buffer-file-name)
        (add-function :after after-focus-change-function #'doom-modeline-update-buffer-file-name))
    (progn
      (add-hook 'focus-in-hook #'doom-modeline-update-buffer-file-name)
      (add-hook 'focus-out-hook #'doom-modeline-update-buffer-file-name))))

(doom-modeline-add-variable-watcher
 'doom-modeline-buffer-file-name-style
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-buffer-file-name-style val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when buffer-file-name
           (doom-modeline-update-buffer-file-name)))))))

(defsubst doom-modeline--buffer-state-icon ()
  "The icon of the current buffer state."
  (when doom-modeline-buffer-state-icon
    (when-let ((icon (doom-modeline-update-buffer-file-state-icon)))
      (unless (string-empty-p icon)
        (concat
         (doom-modeline-display-icon icon)
         (doom-modeline-vspc))))))

(defsubst doom-modeline--buffer-simple-name ()
  "The buffer simple name."
  (propertize "%b"
              'face (doom-modeline-face
                     (if (and doom-modeline-highlight-modified-buffer-name
                              (buffer-modified-p))
                         'doom-modeline-buffer-modified
                       'doom-modeline-buffer-file))
              'mouse-face 'doom-modeline-highlight
              'help-echo "Buffer name
mouse-1: Previous buffer\nmouse-3: Next buffer"
              'local-map mode-line-buffer-identification-keymap))

(defsubst doom-modeline--buffer-name ()
  "The current buffer name."
  (when doom-modeline-buffer-name
    (if (and (not (eq doom-modeline-buffer-file-name-style 'file-name))
             doom-modeline--limited-width-p)
        ;; Only display the buffer name if the window is small, and doesn't
        ;; need to respect file-name style.
        (doom-modeline--buffer-simple-name)
      (when-let ((name (or doom-modeline--buffer-file-name
                           (doom-modeline-update-buffer-file-name))))
        ;; Check if the buffer is modified
        (if (and doom-modeline-highlight-modified-buffer-name
                 (buffer-modified-p))
            (propertize name 'face (doom-modeline-face 'doom-modeline-buffer-modified))
          (doom-modeline-display-text name))))))

(doom-modeline-def-segment buffer-info
  "Combined information about the current buffer.

Including the current working directory, the file name, and its state (modified,
read-only or non-existent)."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-state-icon)
   (doom-modeline--buffer-name)
   (doom-modeline-spc)))

(doom-modeline-def-segment buffer-info-simple
  "Display only the current buffer's name, but with fontification."
  (concat
   (doom-modeline--buffer-state-icon)
   (doom-modeline--buffer-simple-name)))

(doom-modeline-def-segment buffer-default-directory
  "Displays `default-directory' with the icon and state.

This is for special buffers like the scratch buffer where knowing the current
project directory is important."
  (let ((face (doom-modeline-face
               (if (and buffer-file-name (buffer-modified-p))
                   'doom-modeline-buffer-modified
                 'doom-modeline-buffer-path))))
    (concat
     (doom-modeline-spc)
     (and doom-modeline-major-mode-icon
          (concat
           (doom-modeline-icon "" :face face)
           (doom-modeline-vspc)))
     (doom-modeline--buffer-state-icon)
     (propertize (abbreviate-file-name default-directory) 'face face))))

(doom-modeline-def-segment buffer-default-directory-simple
  "Displays `default-directory'.

This is for special buffers like the scratch buffer where knowing the current
project directory is important."
  (let ((face (doom-modeline-face 'doom-modeline-buffer-path)))
    (concat
     (doom-modeline-spc)
     (and doom-modeline-major-mode-icon
          (concat
           (doom-modeline-icon "" :face face)
           (doom-modeline-vspc)))
     (propertize (abbreviate-file-name default-directory) 'face face))))

;;
;; Encoding
;;

(doom-modeline-def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer."
  (when doom-modeline-buffer-encoding
    (let ((mouse-face 'doom-modeline-highlight))
      (concat
       (doom-modeline-spc)

       ;; eol type
       (let ((eol (coding-system-eol-type buffer-file-coding-system)))
         (when (or (eq doom-modeline-buffer-encoding t)
                   (and (eq doom-modeline-buffer-encoding 'nondefault)
                        (not (equal eol doom-modeline-default-eol-type))))
           (propertize
            (pcase eol
              (0 "LF ")
              (1 "CRLF ")
              (2 "CR ")
              (_ ""))
            'face (doom-modeline-face)
            'mouse-face mouse-face
            'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
                               (pcase eol
                                 (0 "Unix-style LF")
                                 (1 "DOS-style CRLF")
                                 (2 "Mac-style CR")
                                 (_ "Undecided")))
            'local-map (let ((map (make-sparse-keymap)))
                         (define-key map [mode-line mouse-1] 'mode-line-change-eol)
                         map))))

       ;; coding system
       (let* ((sys (coding-system-plist buffer-file-coding-system))
              (cat (plist-get sys :category))
              (sym (if (memq cat
                             '(coding-category-undecided coding-category-utf-8))
                       'utf-8
                     (plist-get sys :name))))
         (when (or (eq doom-modeline-buffer-encoding t)
                   (and (eq doom-modeline-buffer-encoding 'nondefault)
                        (not (eq cat 'coding-category-undecided))
                        (not (eq sym doom-modeline-default-coding-system))))
           (propertize
            (upcase (symbol-name sym))
            'face (doom-modeline-face)
            'mouse-face mouse-face
            'help-echo 'mode-line-mule-info-help-echo
            'local-map mode-line-coding-system-map)))

       (doom-modeline-spc)))))

;;
;; Remote host
;;

(doom-modeline-def-segment remote-host
  "Hostname for remote buffers."
  (when default-directory
    (when-let ((host (file-remote-p default-directory 'host)))
      (propertize
       (concat "@" host)
       'face (doom-modeline-face 'doom-modeline-host)))))

;;
;; Major mode
;;

(doom-modeline-def-segment major-mode
  "The major mode, including environment and text-scale info."
  (propertize
   (concat
    (doom-modeline-spc)
    (propertize (let ((name (if (listp mode-name)
                                (car mode-name) mode-name)))
                  (format-mode-line (concat
                                      (unless (zerop (recursion-depth))
                                        "* ")
                                      name)))
                'help-echo "Major mode\n\
  mouse-1: Display major mode menu\n\
  mouse-2: Show help for major mode\n\
  mouse-3: Toggle minor modes"
                'mouse-face 'doom-modeline-highlight
                'local-map mode-line-major-mode-keymap)
    (doom-modeline-spc))
   'face (doom-modeline-face 'doom-modeline-buffer-major-mode)))

;;
;; Process
;;

(doom-modeline-def-segment process
  "The process info."
  (doom-modeline-display-text
   (format-mode-line mode-line-process)))

;;
;; Minor modes
;;

(doom-modeline-def-segment minor-modes
  (when doom-modeline-minor-modes
    (let ((face (doom-modeline-face 'doom-modeline-buffer-minor-mode))
          (mouse-face 'doom-modeline-highlight)
          (help-echo "Minor mode
  mouse-1: Display minor mode menu
  mouse-2: Show help for minor mode
  mouse-3: Toggle minor modes"))
      `((:propertize ("" minor-mode-alist " ")
           face ,face
           mouse-face ,mouse-face
           help-echo ,help-echo
           local-map ,mode-line-minor-mode-keymap)
          ,(doom-modeline-spc)))))

;;
;; VCS
;;

(defun doom-modeline-vcs-icon (text face)
  (doom-modeline-icon text :face face))

(defvar-local doom-modeline--vcs-icon nil)
(defun doom-modeline-update-vcs-icon (&rest _)
  "Update icon of vcs state in mode-line."
  (setq doom-modeline--vcs-icon
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state   (vc-state buffer-file-name backend)))
            (cond ((memq state '(edited added))
                   (doom-modeline-vcs-icon "*" 'doom-modeline-info))
                  ((eq state 'needs-merge)
                   (doom-modeline-vcs-icon "?" 'doom-modeline-info))
                  ((eq state 'needs-update)
                   (doom-modeline-vcs-icon "!" 'doom-modeline-warning))
                  ((memq state '(removed conflict unregistered))
                   (doom-modeline-icon "!" :face 'doom-modeline-urgent))
                  (t
                   (doom-modeline-vcs-icon "@" 'doom-modeline-info)))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs-icon)
(add-hook 'after-save-hook #'doom-modeline-update-vcs-icon)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs-icon)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs-icon))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-vcs-icon))))))

(defvar-local doom-modeline--vcs-text nil)
(defun doom-modeline-update-vcs-text (&rest _)
  "Update text of vcs state in mode-line."
  (setq doom-modeline--vcs-text
        (when (and vc-mode buffer-file-name)
          (let* ((backend (vc-backend buffer-file-name))
                 (state (vc-state buffer-file-name backend))
                 (str (if vc-display-status
                          (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                        ""))
                 (face (cond ((eq state 'needs-update)
                              'doom-modeline-warning)
                             ((memq state '(removed conflict unregistered))
                              'doom-modeline-urgent)
                             (t 'doom-modeline-info))))
            (propertize (if (length> str doom-modeline-vcs-max-length)
                            (concat
                             (substring str 0 (- doom-modeline-vcs-max-length 3))
                             doom-modeline-ellipsis)
                          str)
                        'mouse-face 'doom-modeline-highlight
                        'face `(:inherit (,face bold)))))))
(add-hook 'find-file-hook #'doom-modeline-update-vcs-text)
(add-hook 'after-save-hook #'doom-modeline-update-vcs-text)
(advice-add #'vc-refresh-state :after #'doom-modeline-update-vcs-text)

(doom-modeline-def-segment vcs
  "Displays the current branch, colored based on its state."
  (when-let ((icon doom-modeline--vcs-icon)
             (text doom-modeline--vcs-text))
    (concat
     (doom-modeline-spc)
     (propertize (concat
                  (string-join `(" " ,(doom-modeline-display-icon icon)))
                  (doom-modeline-vspc)
                  (string-join `(,(doom-modeline-display-text text) " ")))
                 'face '(:inherit mh/doom-modeline)
                 'mouse-face 'doom-modeline-highlight
                 'help-echo (get-text-property 1 'help-echo vc-mode)
                 'local-map (get-text-property 1 'local-map vc-mode))
     (doom-modeline-spc))))

;; Flymake

;; Compatibility
;; @see https://github.com/emacs-mirror/emacs/commit/6e100869012da9244679696634cab6b9cac96303.
(with-eval-after-load 'flymake
  (unless (boundp 'flymake--state)
    (defvaralias 'flymake--state 'flymake--backend-state))
  (unless (fboundp 'flymake--state-diags)
    (defalias 'flymake--state-diags 'flymake--backend-state-diags)))

(defvar-local doom-modeline--flymake-icon nil)
(defun doom-modeline-update-flymake-icon (&rest _)
  "Update flymake icon."
  (setq flymake--mode-line-format nil) ; remove the lighter of minor mode
  (setq doom-modeline--flymake-icon
        (let* ((known (hash-table-keys flymake--state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported)))
          (when-let
              ((icon
                (cond
                 (some-waiting (doom-modeline-checker-icon "*" 'doom-modeline-debug))
                 ((null known) (doom-modeline-checker-icon "!" 'doom-modeline-urgent))
                 (all-disabled (doom-modeline-checker-icon "!" 'doom-modeline-warning))
                 (t (let ((.error 0)
                          (.warning 0)
                          (.note 0))
                      (progn
                        (cl-loop
                         with warning-level = (warning-numeric-level :warning)
                         with note-level = (warning-numeric-level :debug)
                         for state being the hash-values of flymake--state
                         do (cl-loop
                             with diags = (flymake--state-diags state)
                             for diag in diags do
                             (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                            (warning-numeric-level :error))))
                               (cond ((> severity warning-level) (cl-incf .error))
                                     ((> severity note-level)    (cl-incf .warning))
                                     (t                          (cl-incf .note))))))
                        (if (> (+ .error .warning .note) 0)
                            (doom-modeline-checker-icon "!"
                                                        (cond ((> .error 0) 'doom-modeline-urgent)
                                                              ((> .warning 0) 'doom-modeline-warning)
                                                              (t 'doom-modeline-info)))
                          (doom-modeline-checker-icon "-" 'doom-modeline-info))))))))
            (propertize
             icon
             'help-echo (concat "Flymake\n"
                                (cond
                                 (some-waiting "Checking...")
                                 ((null known) "No Checker")
                                 (all-disabled "All Checkers Disabled")
                                 (t (format "%d/%d backends running
mouse-1: Display minor mode menu
mouse-2: Show help for minor mode"
                                            (length running) (length known)))))
             'mouse-face 'doom-modeline-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line down-mouse-1]
                            flymake-menu)
                          (define-key map [mode-line mouse-2]
                            (lambda ()
                              (interactive)
                              (describe-function 'flymake-mode)))
                          map))))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake-icon)

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake-icon)))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake-icon)))))))

(defun doom-modeline-checker-text (text &optional face)
  "Displays TEXT with FACE."
  (propertize text 'face (or face 'mode-line)))

(defun doom-modeline-checker-icon (text face)
  "Displays the checker ICON with FACE.

UNICODE and TEXT are fallbacks.
Uses `nerd-icons-mdicon' to fetch the icon."
  (doom-modeline-icon text :face face))

(defvar-local doom-modeline--flymake-text nil)
(defun doom-modeline-update-flymake-text (&rest _)
  "Update flymake text."
  (setq doom-modeline--flymake-text
        (let* ((known (hash-table-keys flymake--state))
               (running (flymake-running-backends))
               (disabled (flymake-disabled-backends))
               (reported (flymake-reporting-backends))
               (all-disabled (and disabled (null running)))
               (some-waiting (cl-set-difference running reported))
               (warning-level (warning-numeric-level :warning))
               (note-level (warning-numeric-level :debug))
               (.error 0)
               (.warning 0)
               (.note 0))
          (maphash (lambda (_b state)
                     (cl-loop
                      with diags = (flymake--state-diags state)
                      for diag in diags do
                      (let ((severity (flymake--lookup-type-property (flymake--diag-type diag) 'severity
                                                                     (warning-numeric-level :error))))
                        (cond ((> severity warning-level) (cl-incf .error))
                              ((> severity note-level) (cl-incf .warning))
                              (t (cl-incf .note))))))
                   flymake--state)
          (when-let
              ((text
                (cond
                 (some-waiting (and doom-modeline--flymake-text
                                    (propertize doom-modeline--flymake-text 'face 'doom-modeline-debug)))
                 ((null known) nil)
                 (all-disabled nil)
                 (t (let ((num (+ .error .warning .note)))
                      (when (> num 0)
                        (if doom-modeline-checker-simple-format
                            (doom-modeline-checker-text (number-to-string num)
                                                        (cond ((> .error 0) 'doom-modeline-urgent)
                                                              ((> .warning 0) 'doom-modeline-warning)
                                                              (t 'doom-modeline-info)))
                          (format "%s/%s/%s"
                                  (doom-modeline-checker-text (number-to-string .error)
                                                              'doom-modeline-urgent)
                                  (doom-modeline-checker-text (number-to-string .warning)
                                                              'doom-modeline-warning)
                                  (doom-modeline-checker-text (number-to-string .note)
                                                              'doom-modeline-info)))))))))
            (propertize
             text
             'help-echo (cond
                         (some-waiting "Checking...")
                         ((null known) "No Checker")
                         (all-disabled "All Checkers Disabled")
                         (t (format "error: %d, warning: %d, note: %d
mouse-1: List all problems%s"
                                    .error .warning .note
                                    (if (doom-modeline-mwheel-available-p)
                                        "\nwheel-up/wheel-down: Previous/next problem"))))
             'mouse-face 'doom-modeline-highlight
             'local-map (let ((map (make-sparse-keymap)))
                          (define-key map [mode-line mouse-1]
                            #'flymake-show-buffer-diagnostics)
                          (when (doom-modeline-mwheel-available-p)
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-down-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-prev-error 1 nil t))))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-up-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-next-error 1 nil t))))
                            map)))))))
(advice-add #'flymake--handle-report :after #'doom-modeline-update-flymake-text)

(doom-modeline-add-variable-watcher
 'doom-modeline-checker-simple-format
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-checker-simple-format val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (when (bound-and-true-p flymake-mode)
           (doom-modeline-update-flymake-text)))))))

(doom-modeline-def-segment checker
  "Displays color-coded error status in the current buffer with pretty icons."
  (let* ((seg (cond
               ((and (bound-and-true-p flymake-mode)
                     (bound-and-true-p flymake--state)) ; only support 26+
                `(,doom-modeline--flymake-icon . ,doom-modeline--flymake-text))))
         (icon (car seg))
         (text (cdr seg)))
    (concat
     (and (or icon text) (doom-modeline-spc))
     (and icon (doom-modeline-display-icon icon))
     (and text
          (concat
           (and icon (doom-modeline-vspc))
           (doom-modeline-display-text text)))
     (and (or icon text) (doom-modeline-spc)))))

;;
;; Word Count
;;

(doom-modeline-def-segment word-count
  "The buffer word count.
Displayed when in a major mode in `doom-modeline-continuous-word-count-modes'.
Respects `doom-modeline-enable-word-count'."
  (when (and doom-modeline-enable-word-count
             (member major-mode doom-modeline-continuous-word-count-modes))
    (propertize (format " %dW" (count-words (point-min) (point-max)))
                'face (doom-modeline-face))))

;;
;; Selection
;;

(defsubst doom-modeline-column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(doom-modeline-def-segment selection-info
  "Information about the current selection.

Such as how many characters and lines are selected, or the NxM dimensions of a
block selection."
  (when (and mark-active
             (doom-modeline--active))
    (cl-destructuring-bind (beg . end)
      (cons (region-beginning) (region-end))
      (propertize
       (let ((lines (count-lines beg (min end (point-max)))))
         (concat (doom-modeline-spc)
                 (cond ((or (bound-and-true-p rectangle-mark-mode)
                            (and (bound-and-true-p evil-visual-selection)
                                 (eq 'block evil-visual-selection)))
                        (let ((cols (abs (- (doom-modeline-column end)
                                            (doom-modeline-column beg)))))
                          (format "%dx%dB" lines cols)))
                       ((and (bound-and-true-p evil-visual-selection)
                             (eq evil-visual-selection 'line))
                        (format "%dL" lines))
                       ((> lines 1)
                        (format "%dC %dL" (- end beg) lines))
                       (t
                        (format "%dC" (- end beg))))
                 (when doom-modeline-enable-word-count
                   (format " %dW" (count-words beg end)))
                 (doom-modeline-spc)))
       'face 'doom-modeline-emphasis))))

;;
;; Bars
;;

(defvar doom-modeline--bar-active nil)
(defvar doom-modeline--bar-inactive nil)

(defsubst doom-modeline--bar ()
  "The default bar regulates the height of the mode-line in GUI."
  (unless (and doom-modeline--bar-active doom-modeline--bar-inactive)
    (let ((width doom-modeline-bar-width)
          (height (max doom-modeline-height (doom-modeline--font-height))))
      (setq doom-modeline--bar-active
            (doom-modeline--create-bar-image 'doom-modeline-bar width height)
            doom-modeline--bar-inactive
            (doom-modeline--create-bar-image
             'doom-modeline-bar-inactive width height))))
  (if (doom-modeline--active)
      doom-modeline--bar-active
    doom-modeline--bar-inactive))

(defun doom-modeline-refresh-bars ()
  "Refresh mode-line bars on next redraw."
  (setq doom-modeline--bar-active nil
        doom-modeline--bar-inactive nil))

(cl-defstruct doom-modeline--hud-cache active inactive top-margin bottom-margin)

(defsubst doom-modeline--hud ()
  "Powerline's hud segment reimplemented in the style of Doom's bar segment."
  (let* ((ws (window-start))
         (we (window-end))
         (bs (buffer-size))
         (height (max doom-modeline-height (doom-modeline--font-height)))
         (top-margin (if (zerop bs)
                         0
                       (/ (* height (1- ws)) bs)))
         (bottom-margin (if (zerop bs)
                            0
                          (max 0 (/ (* height (- bs we 1)) bs))))
         (cache (or (window-parameter nil 'doom-modeline--hud-cache)
                    (set-window-parameter
                     nil
                     'doom-modeline--hud-cache
                     (make-doom-modeline--hud-cache)))))
    (unless (and (doom-modeline--hud-cache-active cache)
                 (doom-modeline--hud-cache-inactive cache)
                 (= top-margin (doom-modeline--hud-cache-top-margin cache))
                 (= bottom-margin
                    (doom-modeline--hud-cache-bottom-margin cache)))
      (setf (doom-modeline--hud-cache-active cache)
            (doom-modeline--create-hud-image
             'doom-modeline-bar 'default doom-modeline-bar-width
             height top-margin bottom-margin)
            (doom-modeline--hud-cache-inactive cache)
            (doom-modeline--create-hud-image
             'doom-modeline-bar-inactive 'default doom-modeline-bar-width
             height top-margin bottom-margin)
            (doom-modeline--hud-cache-top-margin cache) top-margin
            (doom-modeline--hud-cache-bottom-margin cache) bottom-margin))
    (if (doom-modeline--active)
        (doom-modeline--hud-cache-active cache)
      (doom-modeline--hud-cache-inactive cache))))

(defun doom-modeline-invalidate-huds ()
  "Invalidate all cached hud images."
  (dolist (frame (frame-list))
    (dolist (window (window-list frame))
      (set-window-parameter window 'doom-modeline--hud-cache nil))))

(doom-modeline-add-variable-watcher
 'doom-modeline-height
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(doom-modeline-add-variable-watcher
 'doom-modeline-bar-width
 (lambda (_sym val op _where)
   (when (and (eq op 'set) (integerp val))
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym _val op _where)
   (when (eq op 'set)
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym _val op _where)
   (when (eq op 'set)
     (doom-modeline-refresh-bars)
     (doom-modeline-invalidate-huds))))

(add-hook 'window-configuration-change-hook #'doom-modeline-refresh-bars)
(add-hook 'window-configuration-change-hook #'doom-modeline-invalidate-huds)

(doom-modeline-def-segment bar
  "The bar regulates the height of the `doom-modeline' in GUI."
  (if doom-modeline-hud
      (doom-modeline--hud)
    (doom-modeline--bar)))

(doom-modeline-def-segment hud
  "Powerline's hud segment reimplemented in the style of Doom's bar segment."
  (doom-modeline--hud))

;;
;; Misc info
;;

(doom-modeline-def-segment misc-info
  "Mode line construct for miscellaneous information.
By default, this shows the information specified by `global-mode-string'."
  (when (and (doom-modeline--segment-visible 'misc-info)
             (or doom-modeline-display-misc-in-all-mode-lines
                 (doom-modeline--active)))
    (doom-modeline-display-text
     (format-mode-line mode-line-misc-info))))

;;
;; Position
;;

(doom-modeline-def-segment buffer-position
  "The buffer position information."
  (let (;(visible (doom-modeline--segment-visible 'buffer-position))
        (lc `(line-number-mode
              (column-number-mode
               (doom-modeline-column-zero-based
                doom-modeline-position-column-line-format
                ,(string-replace
                  "%c" "%C" (car doom-modeline-position-column-line-format)))
               doom-modeline-position-line-format)
              (column-number-mode
               (doom-modeline-column-zero-based
                doom-modeline-position-column-format
                ,(string-replace
                  "%c" "%C" (car doom-modeline-position-column-format))))))
        (mouse-face 'doom-modeline-highlight)
        (local-map mode-line-column-line-number-mode-map))
    (concat
     (doom-modeline-spc)

     ;; Line and column
     (propertize (concat (format-mode-line lc)
                         (and doom-modeline-total-line-number
                              (format "/%d" (line-number-at-pos (point-max)))))
                 'face (doom-modeline-face)
                 'help-echo "Buffer position\n\
mouse-1: Display Line and Column Mode Menu"
                 'mouse-face mouse-face
                 'local-map local-map)

     ;; Percent position
     (when doom-modeline-percent-position
       (concat
        (doom-modeline-spc)
        (propertize (format-mode-line '("" doom-modeline-percent-position "%%"))
                    'face (doom-modeline-face)
                    'help-echo "Buffer percentage\n\
mouse-1: Display Line and Column Mode Menu"
                    'mouse-face mouse-face
                    'local-map local-map)))

     (when (or line-number-mode column-number-mode doom-modeline-percent-position)
       (doom-modeline-spc)))))

;;
;; Input method
;;

(doom-modeline-def-segment input-method
  "The current input method."
  (propertize (cond (current-input-method
                     (concat (doom-modeline-spc)
                             current-input-method-title
                             (doom-modeline-spc)))
                    ((and (bound-and-true-p evil-local-mode)
                          (bound-and-true-p evil-input-method))
                     (concat
                      (doom-modeline-spc)
                      (nth 3 (assoc default-input-method input-method-alist))
                      (doom-modeline-spc)))
                    (t ""))
              'face (doom-modeline-face
                     'doom-modeline-input-method)
              'help-echo (concat
                          "Current input method: "
                          current-input-method
                          "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method")
              'mouse-face 'doom-modeline-highlight
              'local-map mode-line-input-method-map))

;;
;; Info
;;

(doom-modeline-def-segment info-nodes
  "The topic and nodes in the Info buffer."
  (concat
   " ("
   ;; topic
   (propertize (if (stringp Info-current-file)
                   (replace-regexp-in-string
                    "%" "%%"
                    (file-name-sans-extension
                     (file-name-nondirectory Info-current-file)))
                 (format "*%S*" Info-current-file))
               'face (doom-modeline-face 'doom-modeline-info))
   ") "
   ;; node
   (when Info-current-node
     (propertize (replace-regexp-in-string
                  "%" "%%" Info-current-node)
                 'face (doom-modeline-face 'doom-modeline-buffer-path)
                 'help-echo
                 "mouse-1: scroll forward, mouse-3: scroll back"
                 'mouse-face 'doom-modeline-highlight
                 'local-map Info-mode-line-node-keymap))))

;;
;; LSP
;;

(defun doom-modeline-lsp-icon (text face)
  "Display LSP icon (or TEXT in terminal) with FACE."
  (doom-modeline-icon text :face face))

(defvar-local doom-modeline--eglot nil)
(defun doom-modeline-update-eglot ()
  "Update `eglot' state."
  (setq doom-modeline--eglot
        (pcase-let* ((server (and (eglot-managed-p) (eglot-current-server)))
                     (nick (and server (eglot--project-nickname server)))
                     (pending (and server (hash-table-count
                                           (jsonrpc--request-continuations server))))
                     (last-error (and server (jsonrpc-last-error server)))
                     (face (cond (last-error 'doom-modeline-lsp-error)
                                 ((and pending (cl-plusp pending)) 'doom-modeline-lsp-warning)
                                 (nick 'doom-modeline-lsp-success)
                                 (t 'doom-modeline-lsp-warning)))
                     (icon (doom-modeline-lsp-icon "EGLOT" face)))
          (propertize icon
                      'help-echo (cond
                                  (last-error
                                   (format "EGLOT\nAn error occured: %s
mouse-3: Clear this status" (plist-get last-error :message)))
                                  ((and pending (cl-plusp pending))
                                   (format "EGLOT\n%d outstanding requests" pending))
                                  (nick (format "EGLOT Connected (%s/%s)
C-mouse-1: Go to server errors
mouse-1: Go to server events
mouse-2: Quit server
mouse-3: Reconnect to server" nick (eglot--major-modes server)))
                                  (t "EGLOT Disconnected
mouse-1: Start server"))
                      'mouse-face 'doom-modeline-highlight
                      'local-map (let ((map (make-sparse-keymap)))
                                   (cond (last-error
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-clear-status))
                                         ((and pending (cl-plusp pending))
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-forget-pending-continuations))
                                         (nick
                                          (define-key map [mode-line C-mouse-1]
                                            #'eglot-stderr-buffer)
                                          (define-key map [mode-line mouse-1]
                                            #'eglot-events-buffer)
                                          (define-key map [mode-line mouse-2]
                                            #'eglot-shutdown)
                                          (define-key map [mode-line mouse-3]
                                            #'eglot-reconnect))
                                         (t (define-key map [mode-line mouse-1]
                                              #'eglot)))
                                   map)))))
(add-hook 'eglot-managed-mode-hook #'doom-modeline-update-eglot)

(defun doom-modeline-update-lsp-icon ()
  "Update lsp icon."
  (cond ((bound-and-true-p eglot--managed-mode)
         (doom-modeline-update-eglot))))

(doom-modeline-add-variable-watcher
 'doom-modeline-icon
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-icon val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-lsp-icon))))))

(doom-modeline-add-variable-watcher
 'doom-modeline-unicode-fallback
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-unicode-fallback val)
     (dolist (buf (buffer-list))
       (with-current-buffer buf
         (doom-modeline-update-lsp-icon))))))

(doom-modeline-def-segment lsp
  "The LSP server state."
  (when doom-modeline-lsp
    (let ((icon (cond ((bound-and-true-p eglot--managed-mode)
                       doom-modeline--eglot))))
      (when icon
        (concat
         (doom-modeline-spc)
         (doom-modeline-display-icon icon)
         (doom-modeline-spc))))))

(defun doom-modeline-override-eglot ()
  "Override `eglot' mode-line."
  (if (and doom-modeline-lsp
           (bound-and-true-p doom-modeline-mode))
      (setq mode-line-misc-info
            (delq (assq 'eglot--managed-mode mode-line-misc-info) mode-line-misc-info))
    (add-to-list 'mode-line-misc-info
                 `(eglot--managed-mode (" [" eglot--mode-line-format "] ")))))
(add-hook 'eglot-managed-mode-hook #'doom-modeline-override-eglot)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-eglot)

(doom-modeline-add-variable-watcher
 'doom-modeline-battery
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-lsp val)
     (doom-modeline-override-eglot))))

;;
;; Display time
;;

(doom-modeline-def-segment time
  (when (and doom-modeline-time
             (bound-and-true-p display-time-mode)
             (doom-modeline--segment-visible 'time))
    (concat
     (doom-modeline-spc)
     (when doom-modeline-time-icon
       (concat
        (doom-modeline-icon ""
                            :face '(:inherit doom-modeline-time :weight normal))
        (and (or doom-modeline-icon doom-modeline-unicode-fallback)
             (doom-modeline-spc))))
     (propertize display-time-string
                 'face (doom-modeline-face 'doom-modeline-time)))))

(defun doom-modeline-override-time ()
  "Override default `display-time' mode-line."
  (or global-mode-string (setq global-mode-string '("")))
  (if (and doom-modeline-time
           (bound-and-true-p doom-modeline-mode))
      (setq global-mode-string (delq 'display-time-string global-mode-string))
    (setq global-mode-string (append global-mode-string '(display-time-string)))))
(add-hook 'display-time-mode-hook #'doom-modeline-override-time)
(add-hook 'doom-modeline-mode-hook #'doom-modeline-override-time)

(doom-modeline-add-variable-watcher
 'doom-modeline-time
 (lambda (_sym val op _where)
   (when (eq op 'set)
     (setq doom-modeline-time val)
     (doom-modeline-override-time))))

;;
;; Compilation
;;

(doom-modeline-def-segment compilation
  (and (bound-and-true-p compilation-in-progress)
       (propertize "[Compiling] "
                   'face (doom-modeline-face 'doom-modeline-compilation)
	               'help-echo "Compiling; mouse-2: Goto Buffer"
                   'mouse-face 'doom-modeline-highlight
                   'local-map
                   (make-mode-line-mouse-map
                    'mouse-2
			        #'compilation-goto-in-progress-buffer))))

;;
;; Eldoc
;;

(doom-modeline-def-segment eldoc
  (and (bound-and-true-p eldoc-mode)
       '(eldoc-mode-line-string
		 (" " eldoc-mode-line-string " "))))

(defun doom-modeline-eldoc-minibuffer-message (format-string &rest args)
  "Display message specified by FORMAT-STRING and ARGS on the mode-line as needed.
This function displays the message produced by formatting ARGS
with FORMAT-STRING on the mode line when the current buffer is a minibuffer.
Otherwise, it displays the message like `message' would."
  (if (minibufferp)
      (progn
	    (add-hook 'minibuffer-exit-hook
		          (lambda () (setq eldoc-mode-line-string nil
			                  ;; https://debbugs.gnu.org/16920
			                  eldoc-last-message nil))
		          nil t)
	    (with-current-buffer
	        (window-buffer
	         (or (window-in-direction 'above (minibuffer-window))
                 (minibuffer-selected-window)
		         (get-largest-window)))
          (setq eldoc-mode-line-string
                (when (stringp format-string)
                  (apply #'format-message format-string args)))
          (force-mode-line-update)))
    (apply #'message format-string args)))

;;
;; Viper
;;

(doom-modeline-def-segment viper
  (when (bound-and-true-p viper-mode)
    (when-let ((state (cl-case viper-current-state
                        (emacs-state " E> ")
                        (vi-state " V> ")
                        (insert-state " I> ")
                        (replace-state " R> "))))
      (doom-modeline-icon state :face 'mh/doom-modeline))))

(provide 'doom-modeline-segments)

;;; doom-modeline-segments.el ends here
