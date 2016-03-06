;;; cheerilee-actions.el --- Interacting elements -*- lexical-binding: t -*-

;; Copyright (C) 2015 Alessio Vanni

;; Author: Alessio Vanni <vannilla@firemail.cc>
;; Created: December 2015

;; This file is not part of GNU Emacs.

;; Cheerilee is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Cheerilee is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This file provides functions to interact with elements in different ways.

;;; Code:
(require 'cheerilee-classes)
(require 'cheerilee-core)
(require 'cheerilee-clear)
(require 'cheerilee-display)
(require 'cheerilee-destroy)

;;;###autoload
(defun cheerilee-add-to-element (tree id new)
  "Add to the element of TREE called ID the element NEW as a child."
  (when (and tree
	     (listp tree)
	     (listp (car tree)))
    (let ((al (car tree))
	  (dl (cdr tree)))
      (cheerilee-add-to-element (nthcdr 4 al) id new)
      (when (equal (nth 3 al) id)
	(setf (nthcdr 4 al) (append (nthcdr 4 al) (list new))))
      (cheerilee-add-to-element dl id new))))

;;;###autoload
(defun cheerilee-remove-element (tree id)
  "Remove _all_ elements from TREE named ID."
  (when (and tree
	     (listp tree))
    (let ((l (nthcdr 4 tree)))
      (dotimes (i (length l))
	(when (equal (nth 3 (nth i l)) id)
	  (cheerilee--apply-dispose (list (nth i l)))
	  (setf (nth i l) nil)))
      (setq tree (delq nil tree))
      (dolist (el l)
	(cheerilee-remove-element el id)))))

(defmethod cheerilee-move-control ((window cheerilee-window) dx dy)
  "Move WINDOW, with DX and DY as the distance from current location.

A positive DX moves to the right, a negative one to the left.
A positive DY moves down, a negative one moves up."
  (let* ((l (oref window location))
	 (s (oref window size))
	 (w (oref window line-width))
	 (dl (cons (+ (car l) dx) (+ (cdr l) dy)))
	 (fr (cheerilee-get-frame (oref window frame)
				   (cheerilee-get-element-list))))
    (cheerilee-clear-area fr (- (car l) w) (- (cdr l) w)
			  (+ (car s) w w) (+ (cdr s) w w))
    (oset window location dl)
    (setq l (oref window location))
    (dolist (el (oref window list))
      (oset el x (car dl))
      (oset el y (cdr dl)))
    (cheerilee-clear-area fr (- (car l) w) (- (cdr l) w)
			  (+ (car s) w w) (+ (cdr s) w w))))

(defmethod cheerilee-move-control ((button cheerilee-button) dx dy)
    "Move BUTTON, with DX and DY as the distance from current location.

A positive DX moves to the right, a negative one to the left.
A positive DY moves down, a negative one moves up."
  (call-next-method button dx dy)
  (let ((l (oref button location))
	(p (oref button text-prop)))
    (setf (nth 1 p) l)))

(defmethod cheerilee-change-line-width ((ctrl cheerilee-bordered-area) new)
  "Change CTRL's line width, adding NEW to the current one.

A positive argument makes the line thicker, a negative one makes it narrower."
  (let ((lw (+ (oref ctrl line-width) new))
	(l (oref ctrl location))
	(s (oref ctrl size)))
    (when (< 0 lw 13)
      (cheerilee-clear-area
       (cheerilee-get-frame (oref ctrl frame) (cheerilee-get-element-list))
       (- (car l) lw) (- (cdr l) lw) (+ (car s) lw lw) (+ (cdr s) lw lw))
      (oset ctrl line-width lw)
      (xcb:-+request cheerilee-connection
	  (make-instance 'xcb:ChangeGC
			 :gc (oref ctrl id)
			 :value-mask xcb:GC:LineWidth
			 :line-width lw)))))

(defmethod cheerilee-change-font ((ctrl cheerilee-with-text) new)
  "Change CTRL's font with NEW.

NEW must be a string identifying a font name, according
to the X11 specifications."
  (cheerilee--open-a-font new)
  (let ((f (assoc new cheerilee--fonts-alist)))
    (xcb:-+request cheerilee-connection
	(make-instance 'xcb:ChangeGC
		       :gc (oref ctrl id)
		       :value-mask xcb:GC:Font
		       :font (cdr f))))
  (let ((l (oref ctrl location))
	(s (oref ctrl size)))
  (cheerilee-clear-area
   (cheerilee-get-frame (oref ctrl frame) (cheerilee-get-element-list))
   (car l) (cdr l) (car s) (cdr s))))

(defmethod cheerilee-change-text-content ((ctrl cheerilee-window) new)
  "Change the text field of CTRL with NEW."
  (let ((l (oref ctrl location))
	(s (oref ctrl size))
	(q (cdr (assoc 'text8 cheerilee--shapes-alist))))
    (oset ctrl text new)
    (dolist (el q)
      (when (equal (cdar el) (oref ctrl id))
	(setf (nth 2 el) (oref ctrl text))
	(cheerilee-clear-area
	 (cheerilee-get-frame
	  (caar el) (cheerilee-get-element-list))
	 (car l) (cdr l) (car s) (cdr s))))))

(defmethod cheerilee-get-text-content ((box cheerilee-with-text))
  "Return the content of BOX's text field."
  (oref box text))

(defmethod cheerilee-change-cursor-shape ((ctrl cheerilee-control) value)
  "Change FRAME's cursor shape to VALUE."
  (let ((cursor (cdr (assoc "cursor" cheerilee--fonts-alist)))
	(frame (nth 1
		    (cheerilee-get-frame
		     (oref ctrl frame) cheerilee--model-tree))))
    (xcb:+request cheerilee-connection
	(make-instance 'xcb:FreeCursor
		       :cursor (oref frame cursor)))
    (xcb:+request cheerilee-connection
	(make-instance 'xcb:CreateGlyphCursor
		     :cid (oref frame cursor)
		     :source-font cursor
		     :mask-font cursor
		     :source-char value
		     :mask-char value
		     :fore-red 1 :fore-green 0 :fore-blue 1
		     :back-red 0 :back-green 1 :back-blue 0))
  (xcb:+request cheerilee-connection
      (make-instance 'xcb:ChangeWindowAttributes
		     :window (oref frame frame)
		     :value-mask xcb:CW:Cursor
		     :cursor (oref frame cursor)))))

(provide 'cheerilee-actions)

;;; cheerilee-actions.el ends here
