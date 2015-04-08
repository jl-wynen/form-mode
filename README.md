# form-mode
Emacs major mode for [FORM](http://www.nikhef.nl/~form/) source code providing syntax highlighting and functions for basic use (indentation, adding/removing comments, etc.).

The mode currently provides:
- Syntax highlighting
- Custom function to (un-)comment lines
- Code indentation
- Support for [Flycheck](https://flycheck.readthedocs.org/en/0.22/)
- Support for ac-mode

## Installation
Put form-mode.el in your emacs load-path.

You can optionally (though recommended) byte compile the file by calling the `byte-compile-file` function in Emacs.

## Usage
Put the following into your .emacs:
```lisp
(require 'form-mode)
```
The default `comment-dwim function` does not work here, use the provided `form-comment-dwim` instead.
