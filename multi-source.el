;;; multi-source.el --- Configure multi source -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/multi-source
;; Version: 0.1.0
;; Keywords: lisp abbrev
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Configure multi source

;;; Code:

(defcustom multi-source-restore-last-input t
  "Whether to insert last typed source input."
  :group 'multi-source
  :type 'boolean)

(defvar multi-source-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C->") #'multi-source-select-next)
    (define-key map (kbd "C-<") #'multi-source-select-prev)
    (define-key map (kbd "C-.") #'multi-source-read-source)
    map)
  "Keymap to use in minibuffer.")

(defun multi-source-select-next ()
  "Throw to the catch tag ='next with 1."
  (interactive)
  (throw 'next
         1))

(defun multi-source-select-prev ()
  "Throw to the catch tag ='next with -1."
  (interactive)
  (throw 'next
         -1))

(defvar multi-source--sources-list nil
  "Normalized sources.")

(defvar multi-source--current-index nil
  "Index of active source.")

(defvar multi-source-last-input nil
  "Last typed input in minibuffer.")

(defun multi-source-set-last-input ()
  "Save last typed input in mininubbfer."
  (when (minibufferp)
    (setq multi-source-last-input
          (buffer-substring (minibuffer-prompt-end)
                            (point)))))

(defun multi-source-switcher (step current-index switch-list)
  "Increase or decrease CURRENT-INDEX depending on STEP value and SWITCH-LIST."
  (cond ((> step 0)
         (if (>= (+ step current-index)
                 (length switch-list))
             0
           (+ step current-index)))
        ((< step 0)
         (if (or (<= 0 (+ step current-index)))
             (+ step current-index)
           (1- (length switch-list))))
        (t current-index)))

(defun multi-source-read-source ()
  "Select a source by label and calculate index offset."
  (interactive)
  (let* ((source-label
          (completing-read "Source: " (nth 1 multi-source--sources-list)))
         (pos (seq-position (nth 1 multi-source--sources-list) source-label)))
    (throw 'next (- pos multi-source--current-index))))

(defun multi-source-map-sources (sources)
  "Normalize SOURCES to list of functions, labels and arguments."
  (let ((curr)
        (labels)
        (args)
        (fns))
    (while (setq curr (pop sources))
      (pcase curr
        ((pred stringp)
         (let ((fn (pop sources)))
           (push curr labels)
           (push fn fns)
           (push nil args)))
        ((pred functionp)
         (let ((label
                (if (symbolp curr)
                    (symbol-name curr)
                  "")))
           (push label labels)
           (push curr fns)
           (push nil args)))
        ((pred listp)
         (let* ((label (car curr))
                (rest (cdr curr))
                (fn (if (listp rest)
                        (car rest)
                      rest))
                (extra-args (when (listp rest)
                              (cdr rest))))
           (push label labels)
           (push (if (or (functionp fn)
                         (symbolp fn))
                     fn
                   `(lambda () ,fn))
                 fns)
           (push extra-args args)))))
    (list (reverse fns)
          (reverse labels)
          (reverse args))))


(defun multi-source-read (sources)
  "Combine minibuffer SOURCES into a command with several alternatives.

Every alternative should be a function that reads data from minibuffer.

By default the first source is called and user can switch between
alternatives dynamically with commands:

 `multi-source-select-next' (bound to \\<multi-source-minibuffer-map>\
`\\[multi-source-select-next]') - select next alternative.
 `multi-source-select-prev' (bound to \\<multi-source-minibuffer-map>\
`\\[multi-source-select-prev]') - select previus alternative.
 `multi-source-read-source' (bound to \\<multi-source-minibuffer-map>\
`\\[multi-source-read-source]') - select from completions list.

Allowed forms for SOURCES are
 - a list of functions
 - a plist of backend's name and corresponding function,
-  an alist of backend's name, corresponding function and optionally extra
 arguments to pass."
  (setq multi-source--sources-list (multi-source-map-sources sources))
  (setq multi-source--current-index 0)
  (setq multi-source-last-input nil)
  (let ((curr)
        (fns (nth 0 multi-source--sources-list))
        (args (nth 2 multi-source--sources-list)))
    (while (numberp
            (setq curr
                  (catch 'next
                    (minibuffer-with-setup-hook
                        (lambda ()
                          (use-local-map
                           (let ((map
                                  (copy-keymap
                                   multi-source-minibuffer-map)))
                             (set-keymap-parent map (current-local-map))
                             map))
                          (when (minibuffer-window-active-p
                                 (selected-window))
                            (when (and multi-source-restore-last-input
                                       multi-source-last-input
                                       (string-empty-p (minibuffer-contents-no-properties)))
                              (insert
                               multi-source-last-input))
                            (add-hook
                             'post-command-hook
                             #'multi-source-set-last-input
                             nil t)
                            (add-hook
                             'minibuffer-exit-hook
                             #'multi-source-set-last-input
                             nil t)
                            (add-hook
                             'post-self-insert-hook
                             #'multi-source-set-last-input
                             nil t)))
                      (apply (nth multi-source--current-index fns)
                             (nth multi-source--current-index args))))))
      (setq multi-source--current-index
            (multi-source-switcher curr
                                   multi-source--current-index
                                   fns)))
    (setq multi-source-last-input nil)
    (setq multi-source--sources-list nil)
    curr))

(provide 'multi-source)
;;; multi-source.el ends here