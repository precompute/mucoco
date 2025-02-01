;;; MuCoCo.el --- Multiple Compile Commands  -*- lexical-binding: t -*-

;; Copyright (C) 2025 precompute

;; Author: precompute <git@precompute.net>
;; URL: https://github.com/precompute/mucoco
;; Created: February 2, 2025
;; Modified: February 2, 2025
;; Version: 0.0.2
;; Package-Requires: ((emacs "26.1") (evil "1.2.10"))

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
;;; Code:
;;;; Variables
(defcustom mucoco-list
  "List of plists with mucoco definitions."
  :type 'list
  :group 'evil-lispops)

;;;; Functions
(defun mucoco-get-plist-with-member (key val &optional plistlist)
  "Return plist with member KEY VAL from PLISTLIST."
  (interactive)
  (let ((plistlist (or plistlist mucoco-list)))
   (while (and plistlist
               (not (equal val (plist-get (car plistlist) key))))
     (setq plistlist (cdr plistlist)))
   (car plistlist)))

(defun mucoco-resolve-plist (plist &optional plistlist)
  (let* ((plistlist (or plistlist mucoco-list))
         (template (plist-get plist :template))
         (plist (if template
                    (map-delete plist :template) ;; map-delete deletes *all* instances of KEY
                  plist))
         (plist0 (if template
                     (mucoco-get-plist-with-member :name template plistlist)
                   nil))
         (join-plists (lambda (x y) (map-merge-with 'plist #'(lambda (a b) a) x y))))
    (if (and template plist0)
        (mucoco-resolve-plist (funcall join-plists plist plist0) plistlist)
      plist)))

;;; mucoco.el ends here
