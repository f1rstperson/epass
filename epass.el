;;; epass.el --- the standard UNIX password-manager in emacs-lisp -*- lexical-binding: t -*-

;; Author: user
;; Maintainer: user
;; Version: 0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/f1rstperson/epass
;; Keywords: pass password-store helm gpg epa


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; A password-manager based on pass (see
;; https://www.passwordstore.org/). The main advantage over other
;; solutions is that the only external dependency is gpg, not 'pass'
;; itself. As a result, it runs very well on all platforms, including
;; Windows. It includes clipboard-management and a major-mode that can
;; display the password as asterisks('*'). It uses the default emacs
;; mechanisms for encryption and decryption of files.

;;; Code:

;; ----------------------------------------------------------------------
;; clipboard interaction
;; ----------------------------------------------------------------------
(defvar epass-clipboard-timeout 10
  "The amount of time in seconds before the clipboard is cleared")
(defvar epass-current-pw ""
  "Stores the last password. It should never be set without
calling `epass-clear-clipboard' afterwards, perhaps with a
timer.")

(defun epass-clipboard-from-file (file &optional close)
  "Silently opens FILE, copies the first line into the clipboard
and, if CLOSE is non-nil, closes the file again, so the password
is not visibly displayed anywhere. After
`epass-clipboard-timeout' seconds, it will call
`epass-clear-clipboard'."
  (epass-clear-clipboard)
  (find-file file)
  (save-excursion
    (beginning-of-buffer)
    (kill-ring-save (point-min) (point-at-eol)))
  (when close (kill-buffer (current-buffer)))
  (setq epass-current-pw (current-kill 0))
  (message "clearing clipboard in %d s" epass-clipboard-timeout)
  (run-at-time epass-clipboard-timeout nil 'epass-clear-clipboard))

(defun epass-clipboard-dired-silent ()
  "Simply calls `epass-clipboard-from-file' on the file under
point in dired."
  (interactive)
  (epass-clipboard-from-file (dired-get-filename) t))

(defun epass-clipboard-current-file ()
  "Calls `epass-clipboard-from-file' on the current file."
  (interactive)
  (epass-clipboard-from-file (buffer-file-name (current-buffer))))

(defun epass-clear-clipboard ()
  "This deletes any occurrence of `epass-current-pw' from the kill-ring.
It also clears the systems clipboard if it currently still
contains `epass-current-pw' and otherwise leaves it untouched. As
a result it is safe to call this whenever without affecting the
normal entries in the kill-ring or normal clipboard values."
  (interactive)
  (let ((clipb-status (string-equal (current-kill 0) epass-current-pw)))
    (when clipb-status (gui-select-text ""))
    (setq kill-ring (seq-filter
                     (lambda (elt) (not (string-equal elt epass-current-pw)))
                     kill-ring)
          epass-current-pw "")
    (current-kill 1)
    (message (concat "cleared kill-ring " (when clipb-status "and clipboard ")
                     "of the current password"))))

(define-key dired-mode-map (kbd ": p") 'epass-clipboard-dired-silent)

;; ----------------------------------------------------------------------
;; editing
;; ----------------------------------------------------------------------

(defvar-local epass-edit-password-hidden-status nil
  "Whether the password is currently hidden or not. If you use
`setq-default' to set this to t, then future epass-edit buffers
will start with the password shown.")

(defun epass-edit-hide-chars-fun (beg end _len)
  "More or less copied from `read-passwd', but applied to the
first line instead of the minibuffer."
  (if (eq (line-number-at-pos) 1)
      (dotimes (i (- end beg))
        (put-text-property (+ i beg) (+ 1 i beg)
                           'display (string ?*)))))

(defmacro epass-edit-backup-undo-and-modified (body)
  "Backs up `buffer-undo-list', `buffer-undo-tree' and whether the
buffer is modified. After executing BODY, restores those values."
  `(let ((buffer-undo-list-before buffer-undo-list)
         (buffer-undo-tree-before buffer-undo-tree)
         (buffer-modified-p-before (buffer-modified-p)))
     (message "modified: %s" buffer-modified-p-before)
     (progn ,body)
     (setq buffer-undo-list buffer-undo-list-before
           buffer-undo-tree buffer-undo-tree-before)
     (set-buffer-modified-p buffer-modified-p-before)))


(defun epass-edit-hide-show-password ()
  "Toggles whether the password (aka the first line) should be
displayed as asterisks('*') or normally and consequently changes
`epass-edit-password-hidden-status'. Also sets appropriate hooks
so that changes will also be shown as asterisks if
`epass-edit-password-hidden-status' is non-nil."
  (interactive)
  (epass-edit-backup-undo-and-modified
   (let ((pw-end (save-excursion (beginning-of-buffer) (point-at-eol))))
     (if epass-edit-password-hidden-status
         (progn (setq epass-edit-password-hidden-status nil
                      header-line-format " password shown")
                (remove-hook 'after-change-functions 'epass-edit-hide-chars-fun t)
                (set-text-properties (point-min) pw-end nil))
       (progn (setq epass-edit-password-hidden-status t
                    header-line-format " password hidden")
              (add-hook 'after-change-functions 'epass-edit-hide-chars-fun nil t)
              (dotimes (i (- pw-end (point-min)))
                (put-text-property (+ 1 i) (+ 2 i)
                                   'display (string (or read-hide-char ?*)))))))))

(defvar epass-edit-mode-map nil "Keymap for `epass-edit-mode'")
(setq epass-edit-mode-map (make-sparse-keymap))
(define-key epass-edit-mode-map (kbd "C-c h") 'epass-edit-hide-show-password)
(define-key epass-edit-mode-map (kbd "C-c c") 'epass-clipboard-current-file)

(define-derived-mode epass-edit-mode text-mode "epass-edit"
  "Major mode for editing password-files. Mainly handles hiding
the password. The first line of the file is considered the
password."
  (epass-edit-hide-show-password)
  (next-line))

;; ----------------------------------------------------------------------
;; interaction with the store
;; ----------------------------------------------------------------------

(defvar epass--store-location "~/.password-store/"
  "Internal use. You may set this manually, but it is recommended
  to set this with `epass-set-store-location', as that will
  properly set up `auto-mode-alist'.")

(defun epass-set-store-location (location)
  "Set the LOCATION for your passwords. Some functions will use
  this as a base directory for searching and '.gpg' files in this
  directory will be opened with `epass-edit-mode'."
  (let ((old-regexp (concat (expand-file-name epass--store-location) ".+\\.gpg\\'"))
        (new-regexp (concat (expand-file-name location) ".+\\.gpg\\'")))
    (setq auto-mode-alist (delete (cons old-regexp 'epass-edit-mode) auto-mode-alist))
    (setq epass--store-location location)
    (add-to-list 'auto-mode-alist (cons new-regexp 'epass-edit-mode))))

(epass-set-store-location "~/.password-store/")

(defun epass-store-files ()
  "Return a list of all '.gpg' files in and relative to
`epass--store-location'"
  (mapcar (lambda (filename) (cadr (split-string filename (expand-file-name epass--store-location))))
          (directory-files-recursively epass--store-location "\\.gpg")))

(defun epass-store-clipboard ()
  "Interactively select a password to copy to clipboard."
  (interactive)
  (let ((selection (completing-read "Password file: " (epass-store-files))))
    (epass-clipboard-from-file (concat epass--store-location selection) t)))

(defun epass-store-edit ()
  "Interactively select a password to edit."
  (interactive)
  (let ((selection (completing-read "Password file: " (epass-store-files))))
    (find-file (concat epass--store-location selection) t)))

(when (featurep 'helm)
  (defun epass-store-helm ()
    "Interactively select a password to edit or copy to clipboard."
    (interactive)
    (let ((source (helm-build-sync-source "epass-files"
                    :candidates (epass-store-files))))
      (helm-add-action-to-source "Copy password to clipboard"
                                 (lambda (filename)
                                   (epass-clipboard-from-file (concat epass--store-location filename)
                                                              t))
                                 source)
      (helm-add-action-to-source "Find File"
                                 (lambda (filename) (find-file (concat epass--store-location filename)))
                                 source)
      (helm-delete-action-from-source 'identity source)
      (helm :sources source :buffer "*helm-epass*"))))


(provide 'epass)

;;; epass.el ends here
