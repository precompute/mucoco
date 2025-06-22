;;; MuCoCo.el --- Multiple Compile Commands  -*- lexical-binding: t -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/mucoco
;; Created: February 2, 2025
;; Modified: June 23, 2025
;; Version: 0.0.5
;; Package-Requires:

;; MuCoCo.el -- Multiple Compile Commands

;; Copyright (C) 2025 precompute

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;; MuCoCo shows a list of multiple compile commands in a transient.
;; It also allows defining mode-specific compile commands.

;;; Code:
;;;; Variables
(defcustom mucoco-commands nil
  "Commands for MuCoCo."
  :type 'sexp :group 'mucoco)

;;;; Functions
(defun mucoco--compile (command)
  "Run `compile’ for COMMAND."
  (compile command))

;;;;; Transient
;;;;;; Helper Functions
;;;;;;; Compile Command
(defun mucoco--compile-command-empty ()
  "Check whether `compile-coammnd’ is nil."
  (null compile-command))

;;;;;;; Compile History
(defun mucoco--compile-history-empty ()
  "Check whether `compile-history’ is nil."
  (null compile-history))

(defun mucoco--compile-history-transient-generator ()
  "Generates transient forms for the `compile-history’ varible."
  (when (not (mucoco--compile-history-empty))
    (mapcar (lambda (z)
              (let ((c (nth z compile-history))
                    (k (char-to-string (+ ?a z))))
                (list k c (lambda () (interactive) (mucoco--compile c)))))
            (number-sequence 0 (1- (length compile-history))))))

;;;;;;; Mucoco Commands
(defun mucoco--mucoco-commands-empty ()
  "Check whether `mucoco-commands’ is empty."
  (null mucoco-commands))

(defun mucoco--mucoco-commands-transient-generator ()
  "Generates transient forms for the `mucoco-commands’ varible."
  (when (not (mucoco--mucoco-commands-empty))
    (mapcar (lambda (z)
              (let ((c (nth z mucoco-commands))
                    (k (char-to-string (+ ?a z))))
                (list k c (lambda () (interactive) (mucoco--compile c)))))
            (number-sequence 0 (1- (length mucoco-commands))))))

;;;;;; Transient
(transient-define-prefix mucoco ()
  "Transient for MuCoCo."
  :transient-suffix     'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  ["compile-history" :if-not mucoco--compile-history-empty
   :setup-children
   (lambda (_) (transient-parse-suffixes 'transient--prefix
                                         (mucoco--compile-history-transient-generator)))]
  ["compile-command" :if-not mucoco--compile-command-empty
   ("CC" (lambda () (format "%s" compile-command)) compile)]
  ["mucoco-commands" :if-not mucoco--mucoco-commands-empty
   :setup-children
   (lambda (_) (transient-parse-suffixes 'transient--prefix
                                         (mucoco--mucoco-commands-transient-generator)))])

;;; mucoco.el ends here
