;;; cheerilee-classes.el --- Classes definitions -*- mode: emacs-lisp -*-

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
;; This file provides the definitions of the objects used by
;; the library.

;;; Code:
(defclass cheerilee-control ()
  ((location :initarg :location
	     :initform (0 . 0)
	     :type cons
	     :documentation "The control's location.")
   (id :type number
       :protection :protected
       :documentation "The control's id.")
   (frame :type number
	   :protection :protected
	   :documentation "The frame that contains the control.")
   (background :initarg :background
	       :initform "white"
	       :type string
	       :documentation "The control's background color.")
   (foreground :initarg :foreground
	       :initform "black"
	       :type string
	       :documentation "The control's foreground color.")
   (clip-region :initform ()
		:type list
		:protection :protected
		:documentation "The control's clip region.")
   (button-press :initform ()
		 :type list
		 :documentation "Mouse Button Press event handling functions.")
   (button-rel :initform ()
	       :type list
	       :documentation "Mouse Button Release event handling functions.")
   (key-press :initform ()
	      :type list
	      :documentation "Key Button Press event handling functions.")
   (key-release :initform ()
		:type list
		:documentation "Key Button Release event handling functions.")
   (motion-note :initform ()
		:type list
		:documentation "Motion Notify event handling functions.")
   (capture :initform nil
	    :type nil
	    :documentation "If the Pointer has been captured by an event."))
  :documentation "Base class for every graphical control."
  :abstract t)

(defclass cheerilee-bordered-area (cheerilee-control)
  ((size :initarg :size
	 :initform (100 . 100)
	 :type cons
	 :documentation "The control's size.")
   (line-width :initarg :line-width
	       :initform 1
	       :type number
	       :documentation "The control's line width.")
   (list :type list
	 :protection :protected
	 :documentation "A list of rectangles to draw."))
   :documentation "Base class for graphical controls with a border."
   :abstract t)

(defclass cheerilee-with-text (cheerilee-control)
  ((font-id :type number
	   :protection :protected
	   :documentation "The control's font id.")
   (font :initarg :font
	 :initform "7x14"
	 :type string
	 :documentation "The control's font for text.")
   (text-prop :type list
	      :protection :protected
	      :documentation "Text properties.")
   (txt-rend :initform nil
	     :type symbol
	     :protection :protected
	     :documentation "Has the text field been rendered at least once?")
   (text :initarg :text
	 :initform ""
	 :type string
	 :documentation "Text to be displayed."))
  :documentation "Base class for graphical controls with text in them."
  :abstract t)

(defclass cheerilee-frame (cheerilee-control)
  ((size :initarg :size
	 :initform (320 . 240)
	 :type cons
	 :documentation "The frame's size.")
   (name :initarg :name
	 :initform "Application"
	 :type string
	 :documentation "The frame's name.")
   (open :initarg :open
	 :initform nil
	 :type symbol
	 :protection :private
	 :documentation "Has the frame been mapped at least once?"))
  :documentation "A frame control.")

(defclass cheerilee-window (cheerilee-bordered-area cheerilee-with-text)
  ()
  :documentation "A window control.")

(defclass cheerilee-button (cheerilee-window)
  ()
  :documentation "A button control.")

(defclass cheerilee-text (cheerilee-with-text)
  ()
  :documentation "An element displaying some text.")

(defclass cheerilee-textbox (cheerilee-bordered-area cheerilee-with-text)
  ((selected :initform nil
	     :type symbol
	     :documentation "If the textbox was selected by the user."))
  :documentation "An element in which is possible to write text.")

(provide 'cheerilee-classes)

;;; cheerilee-classes.el ends here
