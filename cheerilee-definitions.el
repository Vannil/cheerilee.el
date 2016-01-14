;;; cheerilee-definitions.el --- Elements definitions -*- lexical-binding: t -*-

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
;; This file provides macros to generate new elements for
;; the application tree.

;;; Code:
(require 'cheerilee-classes)
(require 'cheerilee-core)

(defmacro cheerilee-defframe (id &rest args)
  "Define a new frame.

ID can be any Lisp object, and it is used as a name for the frame.
ARGS begins with any combinations of keywords, and ends with
the definition of any number of children (such as windows or buttons).
When no children are provided, it is not required to specify nil.

The available keywords for frames are:
:name - The string to be shown in the Window Manager title bar
:background - The default color for the frame background
:size - The frame's size relative to the display."
  (declare (debug (form body))
	   (indent defun))
  (let ((instlist)
	(restlist))
    (when args
      (setq instlist
	    (delq nil
		  (mapcar (lambda (x) (unless (and (not (eq (car-safe x) 'cons))
					      (not (eq (car-safe x) 'quote))
					      (listp x)) x)) args)))
      (setq restlist
	    (delq nil
		  (mapcar (lambda (x) (when (and (not (eq (car-safe x) 'cons))
					    (not (eq (car-safe x) 'quote))
					    (listp x)) x)) args))))
    `(list (list 'frame
		 (make-instance cheerilee-frame
				,@instlist)
		 (xcb:generate-id cheerilee-connection)
		 ,id ,@restlist))))

(defmacro cheerilee-defwindow (id &rest args)
  "Define a new window.

ID can be any Lisp object, and it is used as a name for the frame.
ARGS begins with any combinations of keywords, and ends with
the definition of any number of children (such as windows or buttons).
When no children are provided, it is not required to specify nil.

The available keywords for windows are:
:location - Where the window is placed, relative to the parent position
:size - The window dimensions
:foreground - The default color for text and borders
:background - The default color for text background
:line-width - How much wide the text, borders, etc.  are drawn"
  (declare (debug (form body))
	   (indent defun))
  (let ((instlist)
	(restlist))
    (when args
      (setq instlist
	    (delq nil
		  (mapcar (lambda (x) (unless (and (not (eq (car-safe x) 'cons))
					      (not (eq (car-safe x) 'quote))
					      (listp x)) x)) args)))
      (setq restlist
	    (delq nil
		  (mapcar (lambda (x) (when (and (not (eq (car-safe x) 'cons))
					    (not (eq (car-safe x) 'quote))
					    (listp x)) x)) args))))
    `(list 'window
	   (make-instance cheerilee-window
			  ,@instlist)
	   (xcb:generate-id cheerilee-connection)
	   ,id ,@restlist)))

(defmacro cheerilee-defbutton (id &rest args)
  "Define a new button.

ID can be any Lisp object, and it is used as a name for the frame.
ARGS begins with any combinations of keywords, and ends with
the definition of any number of children (such as windows or buttons).
When no children are provided, it is not required to specify nil.

The available keywords for buttons are:
:location - Where the button is placed, relative to the parent position
:size - The button dimensions
:foreground - The default color for text and borders
:background - The default color for text background
:line-width - How much wide the borders, etc.  are drawn
:font - The font used to display text
:text - Any string to be drawn inside the element"
  (declare (debug (form body))
	   (indent defun))
  (let ((instlist)
	(restlist))
    (when args
      (setq instlist
	    (delq nil
		  (mapcar (lambda (x) (unless (and (not (eq (car-safe x) 'cons))
					      (not (eq (car-safe x) 'quote))
					      (listp x)) x)) args)))
      (setq restlist
	    (delq nil
		  (mapcar (lambda (x) (when (and (not (eq (car-safe x) 'cons))
					    (not (eq (car-safe x) 'quote))
					    (listp x)) x)) args))))
      `(list 'button
	   (make-instance cheerilee-button
			  ,@instlist)
	   (xcb:generate-id cheerilee-connection)
	   ,id ,@restlist)))

(defmacro cheerilee-deftext (id &rest args)
  "Define a new text element.

ID can be any Lisp object, and it is used as a name for the frame.
ARGS begins with any combinations of keywords, and ends with
the definition of any number of children (such as windows or buttons).
When no children are provided, it is not required to specify nil.

The available keywords for buttons are:
:location - Where the element is placed, relative to the parent position
:foreground - The default color for text
:background - The default color for text background
:line-width - How much wide the text, borders, etc.  are drawn
:font - The font used to display text
:text - Any string to be drawn inside the element"
  (declare (debug (form body))
	   (indent defun))
  (let ((instlist)
	(restlist))
    (when args
      (setq instlist
	    (delq nil
		  (mapcar (lambda (x) (unless (and (not (eq (car-safe x) 'cons))
					      (not (eq (car-safe x) 'quote))
					      (listp x)) x)) args)))
      (setq restlist
	    (delq nil
		  (mapcar (lambda (x) (when (and (not (eq (car-safe x) 'cons))
					    (not (eq (car-safe x) 'quote))
					    (listp x)) x)) args))))
      `(list 'text
	   (make-instance cheerilee-text
			  ,@instlist)
	   (xcb:generate-id cheerilee-connection)
	   ,id ,@restlist)))

(defmacro cheerilee-deftextbox (id &rest args)
  "Define a new textbox.

ID can be any Lisp object, and it is used as a name for the frame.
ARGS begins with any combinations of keywords, and ends with
the definition of any number of children (such as windows or buttons).
When no children are provided, it is not required to specify nil.

The available keywords for buttons are:
:location - Where the element is placed, relative to the parent position
:size - The box dimensions
:foreground - The default color for text
:background - The default color for text background
:line-width - How much wide the text, borders, etc.  are drawn
:font - The font used to display text
:text - Initial string inside the box."
  (declare (debug (form body))
	   (indent defun))
  (let ((instlist)
	(restlist))
    (when args
      (setq instlist
	    (delq nil
		  (mapcar (lambda (x) (unless (and (not (eq (car-safe x) 'cons))
					      (not (eq (car-safe x) 'quote))
					      (listp x)) x)) args)))
      (setq restlist
	    (delq nil
		  (mapcar (lambda (x) (when (and (not (eq (car-safe x) 'cons))
					    (not (eq (car-safe x) 'quote))
					    (listp x)) x)) args))))
      `(list 'textbox
	   (make-instance cheerilee-textbox
			  ,@instlist)
	   (xcb:generate-id cheerilee-connection)
	   ,id ,@restlist)))

(provide 'cheerilee-definitions)

;;; cheerilee-definitions.el ends here
