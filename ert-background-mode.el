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

;;; Customization

(defgroup ert-background-mode nil
  "Runs ert tests while you edit and displays the results in the modeline."
  :prefix "ebg-"
  :group 'tools)

(defcustom ebg-idle-delay 1
  "The duration to wait for idle input before running tests."
  :group 'ert-background-mode
  :type 'integer)

;;; ----------------------------------------------------------------------------
;;; Faces

(defface ebg-failing-face
  '((t :inherit error))
  "Face for error indicator."
  :group 'ert-background-mode)

(defface ebg-warning-face
  '((t :inherit warning))
  "Face for warning indicator."
  :group 'ert-background-mode)

(defface ebg-passing-face
  '((t (:foreground "green")))
  "Face for passing tests indicator."
  :group 'ert-background-mode)

;;; ----------------------------------------------------------------------------
;;; Mode functions

(defvar ebg--timer nil
  "Idle timer used to run tests.")

(defvar ebg--status-text " [ert]"
  "The string to use to represent the current status in the modeline.")

(define-minor-mode ert-background-mode
  "Displays the current status of ERT tests in the modeline."
  :init-value nil
  :lighter (:eval ebg--status-text)

  (cond
   (ert-background-mode
    (ebg--run-timer)
    (add-hook 'after-save-hook 'ebg--run-timer nil t))

   (t
    (remove-hook 'after-save-hook 'ebg--run-timer t)
    ;; Cancel idle timer.
    (when ebg--timer (cancel-timer ebg--timer))
    (setq ebg--timer nil))))

(defun ebg--run-timer (&rest _args)
  "Configure idle timer."
  (when ebg--timer (cancel-timer ebg--timer))
  (setq ebg--timer (run-with-idle-timer ebg-idle-delay nil 'ebg--run-tests)))

(defun ebg--run-tests (&rest _)
  "Run ERT in the background and update the modeline."
  ;; Rebind `message' so that we do not see printed results.
  (flet ((message (&rest _)))
    (setq ebg--status-text (ebg--summarize (ert-run-tests-batch t)))))

(defun ebg--summarize (results)
  "Select a circle corresponding to the type and number of RESULTS."
  (let ((failing (ert--stats-failed-unexpected results)))
    (cond
     ;; No tests are enabled.
     ((>= 0 (length (ert--stats-tests results)))
      (propertize " [ert]" 'font-lock-face 'ebg-warning-face))
     ;; Indicate number of failing tests.
     ((< 0 failing)
      (propertize (format " [%s]" failing) 'font-lock-face 'ebg-failing-face))
     ;; Show OK for all passing.
     (t
      (propertize " [OK]" 'font-lock-face 'ebg-passing-face)))))

(provide 'ert-background-mode)

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; ert-background-mode.el ends here
