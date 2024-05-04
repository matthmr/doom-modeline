;;; doom-modeline.el --- A minimal and modern mode-line -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2024 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Homepage: https://github.com/seagle0128/doom-modeline
;; Version: 4.2.0
;; Package-Requires: ((emacs "25.1") (compat "29.1.4.2") (nerd-icons "0.1.0") (shrink-path "0.3.1"))
;; Keywords: faces mode-line

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
;; This package offers a fancy and fast mode-line inspired by minimalism design.
;;
;; It's integrated into Doom Emacs (https://github.com/hlissner/doom-emacs) and
;; Centaur Emacs (https://github.com/seagle0128/.emacs.d).
;;
;; The doom-modeline offers:
;; - A match count panel (for anzu, iedit, multiple-cursors, symbol-overlay,
;;   evil-search and evil-substitute)
;; - An indicator for recording a macro
;; - Current environment version (e.g. python, ruby, go, etc.) in the major-mode
;; - A customizable mode-line height (see doom-modeline-height)
;; - A minor modes segment which is compatible with minions
;; - An error/warning count segment for flymake/flycheck
;; - A workspace number segment for eyebrowse
;; - A perspective name segment for persp-mode
;; - A window number segment for winum and window-numbering
;; - An indicator for modal editing state, including evil, overwrite, god, ryo
;;   and xah-fly-keys, etc.
;; - An indicator for battery status
;; - An indicator for current input method
;; - An indicator for debug state
;; - An indicator for remote host
;; - An indicator for LSP state with lsp-mode or eglot
;; - An indicator for github notifications
;; - An indicator for unread emails with mu4e-alert
;; - An indicator for unread emails with gnus (basically builtin)
;; - An indicator for irc notifications with circe, rcirc or erc.
;; - An indicator for buffer position which is compatible with nyan-mode or poke-line
;; - An indicator for party parrot
;; - An indicator for PDF page number with pdf-tools
;; - An indicator for markdown/org previews with grip
;; - Truncated file name, file icon, buffer state and project name in buffer
;;   information segment, which is compatible with project, find-file-in-project
;;   and projectile
;; - New mode-line for Info-mode buffers
;; - New package mode-line for paradox
;; - New mode-line for helm buffers
;; - New mode-line for git-timemachine buffers
;;
;; Installation:
;; From melpa, `M-x package-install RET doom-modeline RET`.
;; In `init.el`,
;; (require 'doom-modeline)
;; (doom-modeline-mode 1)
;; or
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))
;;

;;; Code:

(require 'doom-modeline-core)
(require 'doom-modeline-segments)


;;
;; Mode lines
;;

;; `buffer-info' now also displays `kmacro' and `overwrite-mode'
(doom-modeline-def-modeline 'main
  '(eldoc viper buffer-info major-mode buffer-position remote-host selection-info)
  '(compilation misc-info lsp process check time vcs input-method buffer-encoding minor-modes))


;;
;; Interfaces
;;

;;;###autoload
(defun doom-modeline-set-main-modeline (&optional default)
  "Set main mode-line.
If DEFAULT is non-nil, set the default mode-line for all buffers."
  (doom-modeline-set-modeline 'main default))


;;
;; Minor mode
;;

;; Suppress warnings
(defvar 2C-mode-line-format)
(defvar flymake-mode-line-format)
(defvar helm-ag-show-status-function)
(declare-function helm-display-mode-line "ext:helm-core")

(defvar doom-modeline-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode doom-modeline-mode
  "Toggle `doom-modeline' on or off."
  :group 'doom-modeline
  :global t
  :lighter nil
  :keymap doom-modeline-mode-map
  (if doom-modeline-mode
      (progn
        (doom-modeline-refresh-bars)        ; Create bars
        (doom-modeline-set-main-modeline t) ; Set default mode-line

        ;; Apply to all existing buffers.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (doom-modeline-set-main-modeline)))

        ;; For flymake
        (setq flymake-mode-line-format nil) ; remove the lighter of minor mode

        ;; For Eldoc
        (setq eldoc-message-function #'doom-modeline-eldoc-minibuffer-message)

        ;; For two-column editing
        (setq 2C-mode-line-format (doom-modeline 'special)))
    (progn
      ;; Restore mode-line
      (let ((original-format (doom-modeline--original-value 'mode-line-format)))
        (setq-default mode-line-format original-format)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (setq mode-line-format original-format))))

      ;; For flymake
      (setq flymake-mode-line-format (doom-modeline--original-value 'flymake-mode-line-format))

      ;; For Eldoc
      (setq eldoc-message-function #'eldoc-minibuffer-message)

      ;; For two-column editing
      (setq 2C-mode-line-format (doom-modeline--original-value '2C-mode-line-format)))))

(provide 'doom-modeline)

;;; doom-modeline.el ends here
