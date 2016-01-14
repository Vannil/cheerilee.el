;;; cheerilee-destroy.el --- Disposing of objects -*- lexical-binding: t -*-

;; Copyright (C) 2015 Alessio Vanni

;; Author: Alessio Vanni
;; Created: December 2015

;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides functions to handle the termination of an application
;; by cleaning up the system data structures.

;;; Code:
(require 'cheerilee-classes)
(require 'cheerilee-core)

(defun cheerilee-destroy-window-event (data fake)
  "Event triggered when a frame is closed.

As an event, it associate DATA with a new instance of the correct event.
FAKE is used to determine if the event
is synthetic (i.e. sent with the function `xcb:SendEvent')."
  (let ((ev (make-instance 'xcb:DestroyNotify)))
    (xcb:unmarshal ev data)
    (with-slots (event window) ev
      (let* ((fr (cheerilee-get-frame event cheerilee--model-tree))
	     (lst (nthcdr 4 fr)))
	(cheerilee--dispose-control (nth 1 fr))
	(cheerilee--apply-dispose lst))))
  (xcb:flush cheerilee-connection))

(defun cheerilee--apply-dispose (list)
  "Begin clean-up operations on each element of LIST.

The function is called recursively on each element, so that
its children are disposed too."
  (when (and (car list)
	     (listp list))
    (dolist (el list)
      (cheerilee--dispose-control (nth 1 el))
      (cheerilee--apply-dispose (list (nth 4 el))))))

(defun cheerilee--cdr-delete-all (el alist)
  "Remove EL from ALIST, returning a copy of it.

The purpose of this function is to examine alists structured as
\(((car . cdr) values) ((car . cdr) values) ...), and to remove
any element with the cdr of the car equal to EL."
  (let ((e (cdr alist))
	(res))
    (setq res
	  (delq nil
		(mapcar (lambda (x)
			  (unless (equal el (cdar x))
			    x))
			e)))
    res))

(defun cheerilee--remove-from-tree (id)
  "Remove frame ID from `cheerilee--model-tree'."
  (let ((el (cheerilee-get-frame id (cheerilee-get-element-list))))
    (setcdr cheerilee--model-tree (delete el (cheerilee-get-element-list)))))

(defmethod cheerilee--dispose-control ((ctrl cheerilee-control))
  "Remove CTRL from the objects to be displayed."
  (let ((g (oref ctrl id))
	(r (assoc 'rectangles cheerilee--shapes-alist))
	(p (assoc 'points cheerilee--shapes-alist))
	(s (assoc 'segments cheerilee--shapes-alist))
	(a (assoc 'arcs cheerilee--shapes-alist))
	(fr (assoc 'fillrect cheerilee--shapes-alist))
	(fa (assoc 'fillarcs cheerilee--shapes-alist))
	(tx (assoc 'text8 cheerilee--shapes-alist)))
    (when (cdr r)
      (setcdr r (cheerilee--cdr-delete-all g r)))
    (when (cdr p)
      (setcdr p (cheerilee--cdr-delete-all g p)))
    (when (cdr s)
      (setcdr s (cheerilee--cdr-delete-all g s)))
    (when (cdr a)
      (setcdr a (cheerilee--cdr-delete-all g a)))
    (when (cdr fr)
      (setcdr fr (cheerilee--cdr-delete-all g fr)))
    (when (cdr fa)
      (setcdr fa (cheerilee--cdr-delete-all g fa)))
    (when (cdr tx)
      (setcdr tx (cheerilee--cdr-delete-all g tx)))
    (xcb:+request cheerilee-connection
	(make-instance 'xcb:FreeGC
		       :gc g))))

(defmethod cheerilee--dispose-control ((frame cheerilee-frame))
  "Remove FRAME and its children from `cheerilee--model-tree'."
  (xcb:+request cheerilee-connection
      (make-instance 'xcb:DestroyWindow
		     :window (oref frame id)))
  (cheerilee--remove-from-tree (oref frame id)))

(defmethod cheerilee--dispose-control ((window cheerilee-window))
  "Remove WINDOW from the objects to be displayed."
  (call-next-method window)
  (when (slot-boundp window 'list)
    (slot-makeunbound window 'list)))

(defmethod cheerilee--dispose-control ((button cheerilee-button))
  "Remove BUTTON from the objects to be displayed."
  (call-next-method button)
  (oset button txt-rend nil))

(defmethod cheerilee--dispose-control ((text cheerilee-text))
  "Remove TEXT from the objects to be displayed."
  (call-next-method text)
  (oset text txt-rend nil))

(defmethod cheerilee--dispose-control ((box cheerilee-textbox))
  "Remove BOX from the object to be displayed."
  (call-next-method box)
  (oset box txt-rend nil))

(provide 'cheerilee-destroy)

;;; cheerilee-destroy.el ends here
