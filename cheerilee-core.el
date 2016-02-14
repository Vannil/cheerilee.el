;;; cheerilee-core.el --- Core functions -*- lexical-binding: t -*-

;; Copyright (C) 2015 Alessio Vanni

;; Author: Alessio Vanni <vannilla@firemail.cc>
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
;; This file defines important functions and variables
;; used through all the library.
;; It also defines functions for user interaction.

;;; Code:
(defvar cheerilee-connection nil
  "Connection with the X Server.")

(defvar cheerilee--model-tree nil
  "Tree of elements to be shown.")

(defconst cheerilee--empty-shapes-alist
  (list (cons 'rectangles ())
	(cons 'points ())
	(cons 'segments ())
	(cons 'arcs ())
	(cons 'fillrect ())
	(cons 'fillarcs ())
	(cons 'text8 ()))
  "Default value of `cheerilee--shapes-alist'.")

(defvar cheerilee--shapes-alist (copy-alist cheerilee--empty-shapes-alist)
  "Alist of shapes to draw.")

(defvar cheerilee--fonts-alist ()
  "Alist of opened fonts.")

(defvar cheerilee-event-initialized nil
  "If event handling was initialized.")

;;;###autoload
(defun cheerilee-connect ()
  "Connect to the X Server."
  (unless cheerilee-connection
    (setq cheerilee-connection (xcb:connect-to-socket))
    (let* ((set (xcb:get-setup cheerilee-connection))
	   (screen (car (oref set roots)))
	   (lscr (list (oref screen root) (oref screen root-visual))))
      (setq cheerilee--model-tree (list (list 'display lscr))))
    (xcb:keysyms:init cheerilee-connection)
    ;; Apparently, Emacs tries to close all processes before running
    ;; `kill-emacs-hook'. This is incovenient, because closing Emacs
    ;; before disconnecting will always prompt the user.
    ;; This is pretty annoying, so as a workaround, the process
    ;; will not query the user before bing killed.
    (set-process-query-on-exit-flag (oref cheerilee-connection process) nil)))

;;;###autoload
(defun cheerilee-close-absolutely-everything ()
  "Disconnect from the X Server.
Every application using this library will be closed as a consequence."
  (setq cheerilee--model-tree nil)
  (setq cheerilee--shapes-alist (copy-alist cheerilee--empty-shapes-alist))
  (when (and cheerilee-connection
	     (member (oref cheerilee-connection process) (process-list)))
	(dolist (el cheerilee--fonts-alist)
	  (xcb:+request cheerilee-connection
	      (make-instance 'xcb:CloseFont :font (cdr el))))
	(xcb:flush cheerilee-connection)
	(xcb:disconnect cheerilee-connection))
  (setq cheerilee--fonts-alist nil)
  (setq cheerilee-connection nil)
  (setq cheerilee-event-initialized nil))

;;;###autoload
(defun cheerilee-start-operations ()
  "Begins the rendering operations."
  (let ((l (cheerilee-get-element-list)))
    (cheerilee--open-all-fonts l)
    (xcb:flush cheerilee-connection)
    (dotimes (i 2)
      (dolist (el l)
	(cheerilee--display-tree el))
      (xcb:flush cheerilee-connection))
    (dolist (el l)
      (cheerilee-clear-area el)
      (xcb:flush cheerilee-connection))))

;;;###autoload
(defun cheerilee-process-alive-p ()
  "Return t if the connection with the server is still open.

The variable `cheerilee-connection' is unreliable in some situations,
this function checks if Emacs has a process with the same name as
the connection open."
  (if (member (oref cheerilee-connection process) (process-list))
      t
    nil))

;;;###autoload
(defun cheerilee-add-tree (tree)
  "Make TREE visible to the rendering operations."
  (nconc cheerilee--model-tree tree))

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

(defsubst cheerilee-get-display ()
  "Return the ID associated with the display."
  (cadar cheerilee--model-tree))

(defsubst cheerilee-get-element-list ()
  "Return a list of all defined elements in `cheerilee--model-tree'."
  (cdr cheerilee--model-tree))

(defun cheerilee--get-color (name)
  "Return the value of the color specified by NAME."
  (let ((v (color-values name)))
    (let ((r (nth 0 v))
	  (g (nth 1 v))
	  (b (nth 2 v)))
      (logxor (lsh r 32) (lsh g 16) b))))

(defun cheerilee-get-frame (id list)
  "Return the frame with ID as the associated X11 id.

LIST is searched linearly, any nested frame (e.g. a frame defined
as a child of a window) is ignored."
  (let (res)
    (dolist (el list res)
      (when (and list
		 (listp list)
		 (equal (car el) 'frame))
	(when (equal (nth 2 el) id)
	  (setq res el))))))

;;;###autoload
(defun cheerilee-search-by-id (list id)
  "Search LIST for elements labeled ID and put them in a list.

LIST is searched recursively.  It might be a good idea to avoid
too many nested elements."
  (when (and list
	     (listp list)
	     (listp (car list)))
    (let ((al (car list))
	  (dl (cdr list)))
      (delq nil (append (if (equal (nth 3 al) id) (list al) nil)
			(cheerilee-search-by-id (nthcdr 4 al) id)
			(cheerilee-search-by-id dl id))))))

(defun cheerilee--apply-function (list fun &rest args)
  "Scan LIST recursively and apply FUN with ARGS to each element found."
  (when (and list
	     (listp list)
	     (listp (car list)))
    (let ((al (car list))
	  (dl (cdr list))
	  (ag (if (listp (car args)) (car args) args)))
      (apply fun (nth 1 al) ag)
      (cheerilee--apply-function (nthcdr 4 al) fun ag)
      (cheerilee--apply-function dl fun ag))))

(defun cheerilee-get-clip-rectangle (p1 p2 s1 s2)
  "Calculate the area between two intersecting rectangles that won't be clipped.
The area is relative to the first rectangle passed as argument.

The rectangles are calculated based on their origins (P1 and P2),
and their sizes (S1 and S2)."
  (let* ((ap1 (car p1))
	 (dp1 (cdr p1))
	 (as1 (car s1))
	 (ds1 (cdr s1))
	 (ap2 (car p2))
	 (dp2 (cdr p2))
	 (as2 (car s2))
	 (ds2 (cdr s2))
	 (aps1 (+ ap1 as1))
	 (aps2 (+ ap2 as2))
	 (dps1 (+ dp1 ds1))
	 (dps2 (+ dp2 ds2))
	 dx dy dw dh)
    (setq dx (if (< ap1 ap2)
		 ap2
	       (if (> ap1 aps2)
		   aps2
		 ap1)))
    (setq dy (if (< dp1 dp2)
		 dp2
	       (if (> dp1 dps2)
		   dps2
		 dp1)))
    (setq dw (if (> aps1 aps2)
		 (if (> ap1 aps2)
		     0
		   (- as1 (- aps1 aps2)))
	       (if (< ap1 ap2)
		   (if (< aps1 ap2)
		       0
		     (- aps1 ap2))
		 as1)))
    (setq dh (if (> dps1 dps2)
		 (if (> dp1 dps2)
		     0
		   (- ds1 (- dps1 dps2)))
	       (if (< dp1 dp2)
		   (if (< dps1 dp2)
		       0
		     (- dps1 dp2))
		 ds1)))
    (list dx dy dw dh)))

(defun cheerilee-set-clip-rectangle (gc area line-width)
  "Set the area that remains unclipped.

Anything drawn over GC outside of AREA is clipped.
LINE-WIDTH makes sure to keep the objects border."
  (xcb:+request cheerilee-connection
      (make-instance
       'xcb:SetClipRectangles
       :ordering xcb:ClipOrdering:Unsorted
       :gc gc
       :clip-x-origin 0
       :clip-y-origin 0
       :rectangles (list
		    (make-instance
		     'xcb:RECTANGLE
		     :x (- (nth 0 area) line-width)
		     :y (- (nth 1 area) line-width)
		     :width (+ (nth 2 area) line-width line-width)
		     :height (+ (nth 3 area) line-width line-width))))))

(provide 'cheerilee-core)

;;; cheerilee-core.el ends here
