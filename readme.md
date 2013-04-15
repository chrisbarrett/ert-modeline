# ERT Modeline

Show the status of currently loaded ERT tests in the mode-line.

## Description

This is a minor mode that will run ERT tests whenever you save an elisp buffer
or evaluate an expression. You will know immediately if you have introduced a
breaking change.

## Installation

Clone this repo, add it to your load path, then

```lisp
(autoload 'ert-modeline-mode "ert-modeline")
(add-hook 'emacs-lisp-mode-hook 'ert-modeline-mode)
```
