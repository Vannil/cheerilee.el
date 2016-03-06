;;; cheerilee-display.el --- Display functions -*- lexical-binding: t -*-

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
;; This file provides functions for the drawing operations
;; of each available element.
;; These functions are considered 'primitives' of the system, and are not
;; meant to be used by users.

;;; Code:
(require 'cheerilee-classes)
(require 'cheerilee-core)

;; There is a lot of copy&paste here, beware!

(defun cheerilee--string-to-char2b (string)
  "Return a list of the characters in STRING converted to `xcb:CHAR2B'."
  (mapcar (lambda (char)
	    (make-instance 'xcb:CHAR2B
			   :byte1 (logand char #xff)
			   :byte2 (ash (logand char #xff00) -8)))
	  string))

(defun cheerilee-expose-event (data &optional fake)
  "Render each control defined in `cheerilee--model-tree' on the screen.

As an event, it associate DATA with a new instance of the correct event.
FAKE is used to determine if the event
is synthetic (i.e. sent with the function `xcb:SendEvent')."
  (let ((ev (make-instance 'xcb:Expose)))
    (xcb:unmarshal ev data)
    (with-slots (window) ev
	(dolist (el (cdr (assoc 'rectangles cheerilee--shapes-alist)))
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:PolyRectangle
			     :drawable (caar el)
			     :gc (cdar el)
			     :rectangles (cdr el))))
	(dolist (el (cdr (assoc 'points cheerilee--shapes-alist)))
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:PolyPoint
			     :coordinate-mode xcb:CoordMode:Origin
			     :drawable (caar el)
			     :gc (cdar el)
			     :points (cdr el))))
	(dolist (el (cdr (assoc 'arcs cheerilee--shapes-alist)))
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:PolyArc
			     :drawable (caar el)
			     :gc (cdar el)
			     :arcs (cdr el))))
	(dolist (el (cdr (assoc 'fillrect cheerilee--shapes-alist)))
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:PolyFillRectangle
			     :drawable (caar el)
			     :gc (cdar el)
			     :rectangles (cdr el))))
	(dolist (el (cdr (assoc 'fillarcs cheerilee--shapes-alist)))
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:PolyFillArc
			     :drawable (caar el)
			     :gc (cdar el)
			     :arcs (cdr el))))
	(dolist (el (cdr (assoc 'text8 cheerilee--shapes-alist)))
	  (let ((s (nth 2 el))
		(p (nth 1 el))
		(i 0))
	    (dolist (sp (split-string s "[\n\r]" nil " "))
	      (let* ((rq (xcb:+request cheerilee-connection
			     (make-instance
			      'xcb:QueryTextExtents
			      :font (cdr (assoc (nth 0 p)
						cheerilee--fonts-alist))
			      :string (cheerilee--string-to-char2b sp))))
		     (sz (car (xcb:+reply cheerilee-connection rq))))
		(xcb:-+request cheerilee-connection
			       (make-instance 'xcb:ImageText8
					      :string-len (length sp)
					      :drawable (caar el)
					      :gc (cdar el)
					      :x (+ (car (nth 1 p)) 4)
					      :y (+ (cdr (nth 1 p)) 20 i)
					      :string sp))
		(setq i (+ i (oref sz font-descent) (oref sz font-ascent)))))))
	(xcb:flush cheerilee-connection))))

(defun cheerilee--display-tree (list &rest data)
  "Draw each element of LIST to the screen.
LIST should be the application tree, and DATA any additional
information the examined element should know about."
  (when (and list
	     (listp list))
    (let ((o (nth 1 list))
	  (xid (nth 2 list)))
      (when o
	(cheerilee--display-control o xid data))
      (let ((frm (if (equal (car list) 'frame)
		     xid
		   (nth 2 data))))
	(dolist (el (nthcdr 4 list))
	  (cheerilee--display-tree el
				   (if (equal (car list) 'frame)
				       (cons 0 0)
				     (oref o location))
				   (oref o size) frm))))))

(defun cheerilee--open-a-font (font-name)
  "Open the font called FONT-NAME."
  (when (and font-name
	     (stringp font-name))
    (let ((f (assoc font-name cheerilee--fonts-alist)))
      (unless f
	(let ((id (xcb:generate-id cheerilee-connection)))
	  (push (cons font-name id) cheerilee--fonts-alist)
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:OpenFont
			     :fid id
			     :name-len (length font-name)
			     :name font-name)))))))

(defun cheerilee--open-all-fonts (list)
  "Open all fonts defined in each element of LIST."
  (when (and list
	     (listp list))
    (dolist (el list)
      (unless (or (equal (car el) 'frame)
		  (null el))
	(let ((o (nth 1 el)))
	  (cheerilee--open-a-font (oref o font))))
      (cheerilee--open-all-fonts (nthcdr 4 el)))))

(defmethod cheerilee--display-control ((frame cheerilee-frame) xid data)
  "Create a new frame from FRAME and map it to the screen.

XID is the associated X11 ID, DATA any additional information."
  (unless (oref frame open)
    (let ((d (cheerilee-get-display))
	  (w (or (car (oref frame size)) 320))
	  (h (or (cdr (oref frame size)) 240))
	  (n (oref frame name))
	  (l (or (car data) (cons 0 0)))
	  cursor curscol white)
      (oset frame id xid)
      (oset frame frame xid)
      (unless (oref frame open)
	;; This opens the "cursor" at least once,
	;; without the overhead that would generate
	;; if inside the recursive `open-all-fonts'
	(cheerilee--open-a-font "cursor")
	(setq cursor (cdr (assoc "cursor" cheerilee--fonts-alist)))
	(oset frame cursor (xcb:generate-id cheerilee-connection))
	(setq white (color-values "white"))
	(setq curscol (color-values (oref frame cursor-color)))
	(xcb:-+request cheerilee-connection
	    (make-instance 'xcb:CreateGlyphCursor
			   :cid (oref frame cursor)
			   :source-font cursor
			   :mask-font cursor
			   :source-char cheerilee-cursor-default-cursor
			   :mask-char (1+ cheerilee-cursor-default-cursor)
			   :fore-red (nth 0 curscol)
			   :fore-green (nth 1 curscol)
			   :fore-blue (nth 2 curscol)
			   :back-red (nth 0 white)
			   :back-green (nth 1 white)
			   :back-blue (nth 2 white)))
	(xcb:-+request cheerilee-connection
	    (make-instance 'xcb:CreateWindow
			   :depth xcb:WindowClass:CopyFromParent
			   :wid xid
			   :parent (nth 0 d)
			   :x (car l) :y (cdr l)
			   :width w :height h
			   :border-width 1
			   :class xcb:WindowClass:InputOutput
			   :visual (nth 1 d)
			   :value-mask (logior xcb:CW:EventMask
					       xcb:CW:Cursor
					       xcb:CW:BackPixel)
			   :event-mask (logior xcb:EventMask:Exposure
					       xcb:EventMask:ButtonPress
					       xcb:EventMask:ButtonRelease
					       xcb:EventMask:StructureNotify
					       xcb:EventMask:PointerMotion
					       xcb:EventMask:KeyPress
					       xcb:EventMask:KeyRelease)
			   :background-pixel (cheerilee--get-color
					      (oref frame background))
			   :cursor (oref frame cursor)))
	(when n
	  (xcb:-+request cheerilee-connection
	      (make-instance 'xcb:ChangeProperty
			     :mode xcb:PropMode:Replace
			     :window xid
			     :property xcb:Atom:WM_NAME
			     :type xcb:Atom:STRING
			     :format 8
			     :data-len (length n)
			     :data n)))
	(xcb:-+request cheerilee-connection
	    (make-instance 'xcb:MapWindow :window xid))
	(oset frame open t)))))

(defmethod cheerilee--display-control ((window cheerilee-window) xid data)
    "Display WINDOW on the screen.

XID is the associated X11 ID, DATA any additional information."
  (let ((d (cheerilee-get-display))
	(l (oref window location))
	(s (oref window size))
	(i (nth 0 data))
	(r (nth 1 data))
	(f (nth 2 data)))
    (when (slot-boundp window 'id)
      (let ((cr (cheerilee-get-clip-rectangle l i s r))
	    (lw (oref window line-width)))
	(oset window clip-region cr)
	(cheerilee-set-clip-rectangle xid cr lw)))
    (unless (slot-boundp window 'id)
      (setq l (cons (+ (car l) (car i)) (+ (cdr l) (cdr i))))
      (oset window location l)
      (oset window id xid)
      (oset window frame f)
      (oset window font-id
	    (cdr (assoc (oref window font) cheerilee--fonts-alist)))
      (xcb:-+request cheerilee-connection
	  (make-instance 'xcb:CreateGC
			 :cid xid
			 :drawable (nth 0 d)
			 :value-mask (logior xcb:GC:Foreground
					     xcb:GC:Background
					     xcb:GC:GraphicsExposures
					     xcb:GC:LineWidth
					     xcb:GC:Font)
			 :foreground (cheerilee--get-color
				      (oref window foreground))
			 :background (cheerilee--get-color
				      (oref window background))
			 :line-width (oref window line-width)
			 :font (oref window font-id)
			 :graphics-exposures xcb:GX:clear)))
    (unless (slot-boundp window 'list)
      (oset window list (list (make-instance 'xcb:RECTANGLE
					     :x (car l)
					     :y (cdr l)
					     :width (car s)
					     :height (cdr s))))
      (push (cons (cons f (oref window id)) (oref window list))
	    (cdr (assoc 'rectangles cheerilee--shapes-alist))))))

(defmethod cheerilee--display-control ((button cheerilee-button) xid data)
  "Display BUTTON on the screen.

XID is the associated X11 ID, DATA any additional information."
  (call-next-method button xid data)
  (let ((f (nth 2 data)))
    (unless (oref button txt-rend)
      (oset button text-prop
	    (list (oref button font) (oref button location)))
      (push (list (cons f (oref button id))
		  (oref button text-prop) (oref button text))
	    (cdr (assoc 'text8 cheerilee--shapes-alist)))
      (oset button txt-rend t))))

(defmethod cheerilee--display-control ((text cheerilee-text) xid data)
  "Display TEXT on the screen.

XID is the associated X11 ID, DATA any additional information."
  (let ((d (cheerilee-get-display))
	(l (oref text location))
	(i (nth 0 data))
	(f (nth 2 data)))
    (unless (slot-boundp text 'id)
      (setq l (cons (+ (car l) (car i)) (+ (cdr l) (cdr i))))
      (oset text clip-region (list 0 0 0 0))
      (oset text location l)
      (oset text id xid)
      (oset text frame f)
      (oset text font-id
	    (cdr (assoc (oref text font) cheerilee--fonts-alist)))
      (xcb:-+request cheerilee-connection
	  (make-instance 'xcb:CreateGC
			 :cid xid
			 :drawable (nth 0 d)
			 :value-mask (logior xcb:GC:Foreground
					     xcb:GC:Background
					     xcb:GC:GraphicsExposures
					     xcb:GC:Font)
			 :foreground (cheerilee--get-color
				      (oref text foreground))
			 :background (cheerilee--get-color
				      (oref text background))
			 :font (oref text font-id)
			 :graphics-exposures xcb:GX:clear)))
    (unless (oref text txt-rend)
      (oset text text-prop
	    (list (oref text font) (oref text location)))
      (push (list (cons f (oref text id))
		  (oref text text-prop) (oref text text))
	    (cdr (assoc 'text8 cheerilee--shapes-alist)))
      (oset text txt-rend t))))

(defmethod cheerilee--display-control ((box cheerilee-textbox) xid data)
  "Display BOX on the screen.

XID is the associated X11 ID, DATA any additional information."
  (let ((d (cheerilee-get-display))
	(l (oref box location))
	(s (oref box size))
	(i (nth 0 data))
	(r (nth 1 data))
	(f (nth 2 data)))
    (when (slot-boundp box 'id)
      (let ((cr (cheerilee-get-clip-rectangle l i s r))
	    (lw (oref box line-width)))
	(oset box clip-region cr)
	(cheerilee-set-clip-rectangle xid cr lw)))
    (unless (slot-boundp box 'id)
      (setq l (cons (+ (car l) (car i)) (+ (cdr l) (cdr i))))
      (oset box location l)
      (oset box id xid)
      (oset box frame f)
      (oset box font-id
	    (cdr (assoc (oref box font) cheerilee--fonts-alist)))
      (xcb:-+request cheerilee-connection
	  (make-instance 'xcb:CreateGC
			 :cid xid
			 :drawable (nth 0 d)
			 :value-mask (logior xcb:GC:Foreground
					     xcb:GC:Background
					     xcb:GC:GraphicsExposures
					     xcb:GC:LineWidth
					     xcb:GC:Font)
			 :foreground (cheerilee--get-color
				      (oref box foreground))
			 :background (cheerilee--get-color
				      (oref box background))
			 :line-width (oref box line-width)
			 :font (oref box font-id)
			 :graphics-exposures xcb:GX:clear)))
    (unless (slot-boundp box 'list)
      (oset box list (list (make-instance 'xcb:RECTANGLE
					     :x (car l)
					     :y (cdr l)
					     :width (car s)
					     :height (cdr s))))
      (push (cons (cons f (oref box id)) (oref box list))
	    (cdr (assoc 'rectangles cheerilee--shapes-alist))))
    (unless (oref box txt-rend)
      (oset box text-prop
	    (list (oref box font) (oref box location)))
      (push (list (cons f (oref box id))
		  (oref box text-prop) (oref box text))
	    (cdr (assoc 'text8 cheerilee--shapes-alist)))
      (oset box txt-rend t))))

(provide 'cheerilee-display)

;;; cheerilee-display.el ends here
