;;; cheerilee.el --- Toolkit library -*- lexical-binding: t -*-

;; Copyright (C) 2015 Alessio Vanni

;; Author: Alessio Vanni <vannilla@airmail.cc>
;; Created: November 2015
;; Version: 0.1
;; Keywords: multimedia tools
;; Package-Requires: ((xelb "0.1"))

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
;; cheerilee.el is a graphical toolkit library for the X Window System,
;; written entirely in Emacs Lisp.
;;
;; The library allows the creation of an 'application tree', in which
;; each element to be displayed is declared, similar to markup languages
;; such as HTML.
;; This tree is then appropriately inserted in a structure shared with
;; the system, so that it can be properly displayed and operated on.
;;
;; Quick Start:
;;
;; First thing first, you need to load the library: (require 'cheerilee)
;;
;; After that, the application needs to connect with the X server,
;; before doing any operation: (cheerilee-connect)
;;
;; At this point, a tree can be defined by calling appropriate macros.
;; These macros begin all with the 'cheerilee-def' prefix, followed
;; by the element's name.
;; For example, (cheerilee-defframe ARGS) define a frame with argument ARGS.
;;
;; A frame is required to have a working application, as that is the window
;; actually mapped to the screen, containing the other elements.
;; The 'window' element is a rectangular area with no purpose but to contain
;; other elements inside, and eventually handle events.
;;
;; The tree defined this way must then added to the system's tree, by calling
;; (cheerilee-add-tree TREE), where TREE is your application.
;;
;; Event handling can be added with funcions whose name begin with
;; 'cheerilee-add-' and ends in '-event'. Anything inside describes
;; the handled event.
;;
;; The function 'cheerilee-close-absolutely-everything' disconnects Emacs
;; from the X server, meaning that every application using this library
;; will be closed.
;;
;; There is currently a bug in which certain window managers still close the
;; connection when the application's frame is closed, leaving Emacs without
;; a socket, but with the variable 'cheerilee-connection' non-nil, breaking
;; the whole library.
;;
;; Until that bug is fixed, it's a good idea to call
;; 'cheerilee-process-alive-p' and 'cheerilee-close-absolutely-everything'
;; if the former returns t.

;;; Code:
(require 'xcb)
(require 'xcb-keysyms)
(require 'cheerilee-classes)
(require 'cheerilee-core)
(require 'cheerilee-display)
(require 'cheerilee-destroy)
(require 'cheerilee-handle)
(require 'cheerilee-definitions)
(require 'cheerilee-actions)
(require 'cheerilee-textbox)

;; The purpose of this file is to load all the necessary resources.
;; Please, always (require 'cheerilee), instead of a specific part
;; of the library.

(provide 'cheerilee)

;;; cheerilee.el ends here
