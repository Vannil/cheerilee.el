;;; cheerilee-textbox.el --- Handling textboxes -*- lexical-binding: t -*-

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
;; This file provides functions to specifically handle textboxes.
;; It defines features for selection and text inserting.

;;; Code:
(require 'cheerilee-classes)
(require 'cheerilee-core)
(require 'cheerilee-clear)

(defconst cheerilee-keycode-backspace 22
  "The value of the keycode associated with backspace.")

(defmethod cheerilee-change-text-content ((ctrl cheerilee-textbox) new)
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

(defmethod cheerilee--button-release ((box cheerilee-textbox) x y detail tree)
  "Select or unselect BOX when the Mouse Button is released.

X, Y, and DETAIL are additional informations passed to any user-defined
handlers for this type of events."
  (let ((cr (oref box clip-region)))
    (when (eq (oref box capture) t)
      (oset box capture nil))
    (if (and (<= (nth 0 cr) x (+ (nth 0 cr) (nth 2 cr)))
	     (<= (nth 1 cr) y (+ (nth 1 cr) (nth 3 cr))))
	(progn
	  (cheerilee--select-textbox box)
	  (dolist (el (oref box button-rel))
	    (funcall el box x y detail (list tree))))
      (cheerilee--unselect-textbox box))))

(defmethod cheerilee--select-textbox ((box cheerilee-textbox))
  "Make BOX selected, allowing text insertion."
  (unless (oref box selected)
    (let ((b (oref box text)))
      (oset box selected t)
      (oset box text (concat b "|")))
    (cheerilee-change-text-content box (oref box text))))

(defmethod cheerilee--unselect-textbox ((box cheerilee-textbox))
  "Make BOX unselected, disabling text insertion."
  (when (oref box selected)
    (let ((b (oref box text)))
      (oset box selected nil)
      (oset box text (substring b 0 (1- (length b)))))
    (cheerilee-change-text-content box (oref box text))))

(defmethod cheerilee--key-press ((box cheerilee-textbox) detail modifier tree)
  "Manages text insertion for BOX.

Detail is processed to show the proper symbol. MODIFIER is the value of
any modifier key pressed to change the representation of DETAIL."
  (when (oref box selected)
    (let* ((b (oref box text))
	   (k (xcb:keysyms:keycode->keysym cheerilee-connection
					   detail modifier))
	   (d (if (equal detail cheerilee-keycode-backspace)
		  (concat (substring b 0 (- (length b) 2)) "|")
		(concat (substring b 0 (1- (length b)))
			(when (<= k 255) (list k)) "|"))))
      (oset box text d))
    (cheerilee-change-text-content box (oref box text))))

(defmethod cheerilee-get-text-content ((box cheerilee-textbox))
  "Return the content of BOX's text field."
  (let ((b (oref box text))
	q)
    (setq q (if (oref box selected)
		(substring b 0 (1- (length b)))
	      b))
    q))

(defmethod cheerilee-clear-text-content ((box cheerilee-textbox))
  "Delete the content of BOX's text field."
  (cheerilee-change-text-content box (if (oref box selected) "|" "")))

(provide 'cheerilee-textbox)

;;; cheerilee-textbox.el ends here
