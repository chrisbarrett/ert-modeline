;;; ert-modeline --- displays ert test results in the modeline.

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

;; Runs ERT tests whenever you save an elisp buffer or eval an
;; expression. Displays the results in the modeline.

;;; Code:

(require 'ert)

;;; Customization

(defgroup ert-modeline nil
  "Runs ert tests while you edit and displays the results in the modeline."
  :prefix "ertml-"
  :group 'tools)

(defcustom ertml-selector t
  "The selector to use for running ERT tests."
  :group 'ert-modeline
  :type 'symbol)

(defface ertml-failing-face
  '((t :inherit error))
  "Face for error indicator."
  :group 'ert-modeline)

(defface ertml-warning-face
  '((t :inherit warning))
  "Face for warning indicator."
  :group 'ert-modeline)

(defface ertml-passing-face
  '((t (:foreground "green")))
  "Face for passing tests indicator."
  :group 'ert-modeline)

;;; ----------------------------------------------------------------------------
;;; Mode functions

(defvar ertml--status-text " [ert]"
  "The string to use to represent the current status in the modeline.")

;;;###autoload
(define-minor-mode ert-modeline-mode
  "Displays the current status of ERT tests in the modeline."
  :init-value nil
  :lighter (:eval ertml--status-text)

  (cond
   (ert-modeline-mode
    (ertml--run-tests)
    (add-hook 'after-save-hook 'ertml--run-tests nil t))

   (t
    (remove-hook 'after-save-hook 'ertml--run-tests t))))

(defun ertml--run-tests (&rest _)
  "Run ERT in the modeline and update the modeline."
  ;; Rebind `message' so that we do not see printed results.
  (flet ((message (&rest _)))
    (setq ertml--status-text (ertml--summarize (ert-run-tests-batch ertml-selector)))))

(defun ertml--summarize (results)
  "Select a circle corresponding to the type and number of RESULTS."
  (let ((failing (ert--stats-failed-unexpected results)))
    (cond
     ;; No tests are enabled.
     ((>= 0 (length (ert--stats-tests results)))
      (propertize " [ert]" 'font-lock-face 'ertml-warning-face))
     ;; Indicate number of failing tests.
     ((< 0 failing)
      (propertize (format " [%s]" failing) 'font-lock-face 'ertml-failing-face))
     ;; Show OK for all passing.
     (t
      (propertize " [OK]" 'font-lock-face 'ertml-passing-face)))))

;;; ----------------------------------------------------------------------------
;;; Eval advice
;;;
;;; Ensures that tests are re-run when the buffer is evaluated.

(dolist (fn '(eval-buffer
              eval-defun
              eval-current-buffer
              eval-region)
            )
  (eval `(defadvice ,fn (after ertml--run activate)
           (when (and (boundp 'ert-modeline-mode) ert-modeline-mode)
             (ertml--run-tests)))))

(provide 'ert-modeline)

;; Local Variables:
;; lexical-binding: t
;; byte-compile-warnings: (not obsolete)
;; End:

;;; ert-modeline.el ends here
