;;; cheerilee-clear.el --- Clearing the screen -*- lexical-binding: t -*-

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
;; This file defines a function to clear an area of the frame,
;; so that is can be re-drawn.

;;; Code:
(require 'cheerilee-display)

;; This variable is used to "slow down" clearing requests
;; done automatically by events
(defvar cheerilee--clear-delay 0
  "Delay between `cheerilee-clear-area' calls.")

(defun cheerilee-clear-area (frame &optional x y width height)
  "Clear the area inside FRAME and generate a new Expose event.

X and Y, if provided, are the starting point of the region to
clear.  WIDTH and HEIGHT, if provided, are the dimensions of the region
to clear."
  (let ((dx (if x x 0))
	(dy (if y y 0))
	(w (if width width (car (oref (nth 1 frame) size))))
	(h (if height height (cdr (oref (nth 1 frame) size)))))
    (xcb:-+request cheerilee-connection
	(make-instance 'xcb:ClearArea
		       :exposures 1
		       :window (nth 2 frame)
		       :x dx :y dy
		       :width w
		       :height h))
    (dolist (el (cheerilee-get-element-list))
      (cheerilee--display-tree el))
    (xcb:flush cheerilee-connection)))

(provide 'cheerilee-clear)

;;; cheerilee-clear.el ends here
