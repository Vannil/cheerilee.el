# cheerilee.el

cheerilee.el is a toolkit library for the X Window System, written in
Emacs Lisp.

It is a presentation system, in the same spirit of the DOM or WPF.

Events are handled by other ELisp functions, by attaching them to the
defined elements.

It uses the [XELB](https://github.com/ch11ng/xelb) library to
communicate with the X server.

To use it, add it to your load path:

`(add-to-list 'load-path "/path/to/library")`, then you must
load it:

`(require 'cheerilee)`.

There's a Quick Start guide inside cheerilee.el, or you can read
the Texinfo documentation in cheerilee.info.