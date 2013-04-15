# ERT Modeline

Run ERT tests and show the results in the modeline.

![Example of modeline with ert-modeline-mode enabled](https://raw.github.com/chrisbarrett/ert-modeline/master/ert-modeline.png)

## Description

This is a minor mode that will run ERT tests in the background whenever you save
an elisp buffer or evaluate an expression. The results are displayed in the
modeline, so you will know immediately if you have introduced a breaking change.

## Installation

Clone this repo, add it to your load path, then

```lisp
(autoload 'ert-modeline-mode "ert-modeline")
(add-hook 'emacs-lisp-mode-hook 'ert-modeline-mode)
```
