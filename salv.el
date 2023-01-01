;;; salv.el --- Local minor mode to save a modified buffer when  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Free Software Foundation

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/salv.el
;; Version: 0.1
;; Package-Requires: ((emacs "24.3"))
;; Keywords: convenience

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

;;   Salve, v. t. & i.
;;   To save, as a ship or goods, from the perils of the sea.

;;   --Webster, 1913

;; Q: How does this package differ from other ones that automatically
;; save buffers?

;; A: Salve is a buffer-local minor mode, rather than being a global
;; mode.  It is activated in buffers the user wants to be saved,
;; rather than in all buffers (requiring the user to exclude ones that
;; aren't to be saved).  It uses per-buffer timers, rather than a
;; global timer.  It only runs a timer when a buffer is modified after
;; being saved, rather than constantly.

;; Because of these characteristics, it's simple and lightweight.  To
;; use, just, e.g. add `salv-mode' to a major mode hook.  Or, if you
;; only want it activated in certain conditions, write a simple lambda
;; to activate it when appropriate.  For example, the author uses:

;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (when (file-in-directory-p (buffer-file-name) "~/org")
;;                 (salv-mode))))

;;; Code:

;;;; Variables

(defvar-local salv-timer nil
  "Per-buffer timer.")

(defvar salv-remote-buffer-fn (lambda () (current-buffer))
  "Use remote buffer instead of the current buffer.")

;;;; Customization

(defgroup salv nil
  "Automatically save buffer after so many seconds"
  :group 'convenience)

(defcustom salv-interval 5
  "Seconds before saving buffer."
  :type 'number)

(defcustom salv-save-function 'save-buffer
  "Function that saves the buffer."
  :type 'symbol)

;;;; Commands

;;;###autoload
(define-minor-mode salv-mode
  "Automatically save buffer after so many seconds.
When enabled in a buffer, it will be automatically saved
according to `salv-interval'."
  :lighter " Salv"
  (if salv-mode
      (progn
        (add-hook 'first-change-hook #'salv--setup nil t)
        (when (buffer-modified-p)
          (salv--setup)))
    ;; Disable mode.
    (setq-local first-change-hook (remq #'salv--setup first-change-hook))
    (when (equal first-change-hook (default-value 'first-change-hook))
      (kill-local-variable 'first-change-hook))
    (salv--stop)))

;;;; Functions
(defun salv--setup ()
  "Run timer to save current buffer."
  ;; If the file to save is the current file, start saving timer.
  (when (eq (funcall salv-remote-buffer-fn) (current-buffer))
    (salv--start-timer))

  ;; Timer could be running if the current buffer been postponed from a remote buffer
  (when salv-timer
    (salv--postpone))
  ;; `after-change-functions` is buffer local
  (add-to-list 'after-change-functions #'salv--postpone))

(defun salv--start-timer ()
  "Run timer to save current buffer."
  (unless salv-timer
    (setq salv-timer (run-with-timer salv-interval nil #'salv-save-buffer (current-buffer)))))

(defun salv--postpone (&rest _)
  "Postpone save of current buffer."
  (with-current-buffer (funcall salv-remote-buffer-fn)
    (salv--stop-timer)
    (salv--start-timer)))

(defun salv--stop-timer ()
  "Reset salv back to pre first change."
  (when salv-timer
    (cancel-timer salv-timer)
    (setq salv-timer nil)))

(defun salv--stop ()
  "Reset salv back to pre first change."
  (setq-local after-change-functions (remove #'salv--postpone after-change-functions))
  (salv--stop-timer))

(defun salv-save-buffer (&optional buffer)
  "Save BUFFER and unset timer."
  (let ((buf (or buffer (funcall salv-remote-buffer-fn))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (funcall salv-save-function)
        (salv--stop)))))

;;;; Footer

(provide 'salv)

;;; salv.el ends here
