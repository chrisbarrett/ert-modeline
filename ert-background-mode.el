;;; ert-background-mode --- Background test runner for ERT.

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Background test runner for ERT. Displays the current test status in the
;; modeline in elisp buffers.

;;; Code:

(require 'ert)

(defvar ebg--timer nil
  "Idle timer used to run tests.")

(defvar ebg--lighter " ERT"
  "The lighter to use for this mode.")

(defvar-local ebg--current-lighter ebg--lighter
  "The current state of the lighter.")

(define-minor-mode ert-background-mode
  "Displays the current status of ERT tests in the modeline."
  nil ebg--current-lighter nil
  ;; We do a test run when nelwines are inserted.
  (cond
   (ert-background-mode
    ;; Start idle timer.
    (setq ebg--timer (run-with-idle-timer 1 t 'ebg--run-tests)))

   (t
    ;; Cancel idle timer.
    (when ebg--timer
      (cancel-timer ebg--timer))
    (setq ebg--timer nil))))

;;; ----------------------------------------------------------------------------

(defconst ebg--circles-alist
  '((dotted . "◌")
    (0 . "●")
    (1 . "➊")
    (2 . "➋")
    (3 . "➌")
    (4 . "➍")
    (5 . "➎")
    (6 . "➏")
    (7 . "➐")
    (8 . "➑")
    (9 . "➒"))
  "Circle chars used to indicate test status in mode lighter.")

(defun ebg--run-tests ()
  "Run ERT in the background and update the modeline."
  (when (and (boundp 'ert-background-mode)
             ert-background-mode
             (not (active-minibuffer-window))
             (not cursor-in-echo-area)))
  ;; Rebind `message' so that we do not see printed results.
  (flet ((message (&rest _)))
    (ebg--set-mode-line (ebg--summarize (ert-run-tests-batch t)))))

(defun ebg--number->circle (n)
  "Return the circle corresponding with the number N."
  (cdr (cond ((< n 0) (assoc 'dotted ebg--circles-alist))
             ((or (= n 0) (> n 9)) (assoc 0  ebg--circles-alist))
             (t (assoc n ebg--circles-alist)))))

(defun ebg--summarize (results)
  "Select a circle corresponding to the type and number of RESULTS."
  (let ((failing (ert--stats-failed-unexpected results)))
    (cond
     ;; Show blank circle if no tests are running.
     ((>= 0 (length (ert--stats-tests results)))
      (propertize
       (ebg--number->circle -1)
       'face
       'ebg-warning-face))
     ;; Show circle for number of failures.
     ((< 0 failing)
      (propertize
       (ebg--number->circle failing)
       'face
       'ebg-failing-face))
     ;; Show solid circle for passing.
     (t
      (propertize
       (ebg--number->circle 0)
       'face
       'ebg-passing-face)))))

(defun ebg--set-mode-line (str)
  "Update the modeline with status STR."
  (let ((ml (concat " [ERT "str "]")))
    (setq ebg--current-lighter ml)
    (force-mode-line-update)))

;;; ----------------------------------------------------------------------------

(defface ebg-failing-face
  '((t :inherit error))
  "Face for error indicator."
  :group 'ert-background-mode)

(defface ebg-warning-face
  '((t :inherit warning))
  "Face for warning indicator."
  :group 'ert-background-mode)

(defface ebg-passing-face
  '((((type tty) (class color))
     (:background "green"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((type x w32 mac))
     (:foreground "green")))
  "Face for passing tests indicator."
  :group 'ert-background-mode)

(provide 'ert-background-mode)

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; ert-background-mode.el ends here
