;;; substitute.el --- Efficiently replace targets in the buffer or context -*- lexical-binding: t -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou General Issues <~protesilaos/general-issues@lists.sr.ht>
;; URL: https://git.sr.ht/~protesilaos/substitute
;; Mailing-List: https://lists.sr.ht/~protesilaos/general-issues
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; WORK-IN-PROGRESS.
;;
;; Some Utilities Built to Substitute Targets Independent of Their
;; Utterances, Thoroughly and Easily.

;;; Code:

(defgroup substitute nil
  "Efficiently replace targets in the buffer or context."
  :group 'editing)

(defcustom substitute-highlight nil
  "If non-nil, highlight target during prompt for its substitute.

If nil, do not highlight anything: just prompt for a substitute.

At any rate, always specify at the minibuffer prompt the target
of the substitution."
  :group 'substitute
  :type 'boolean)

(defcustom substitute-post-replace-hook nil
  "Special hook to run after a substitution command.
Every function is called with four arguments: TARGET, SUB, COUNT,
and SCOPE.

For a reference function, see `substitute-report-operation'."
  :group 'substitute
  :type 'hook)

(defvar substitute--history '()
  "Minibuffer history for substitution commands.")

(defun substitute--scope (scope)
  "Return string that describes SCOPE in plain terms.

Possible meaningful values for SCOPE are:

- `below' :: from point to the end of the buffer.
- `above' :: from point to the beginning of the buffer.
- nil     :: across the whole buffer.
- non-nil :: limit to the current defun per `narrow-to-defun'."
  (pcase scope
    ('below "from point to the END of the buffer")
    ('above "from point to the BEGINNING of the buffer")
    ('nil "across the BUFFER")
    (_ "in the current DEFUN")))

(defun substitute--pretty-target (target)
  "Remove regexp delimiters from TARGET.
Use this to produce a more readable version of TARGET for prompts
and related."
  (replace-regexp-in-string "\\\\_<\\(?1:.*?\\)\\\\_>" "\\1" target))

(defun substitute--prompt-without-highlight (target scope)
  "Prompt for string while referencing TARGET and SCOPE."
  (let ((pretty-target (substitute--pretty-target target)))
    (read-string
     (format "Replace `%s' %s with: "
             (propertize pretty-target 'face 'error)
             (substitute--scope scope))
     nil
     'substitute--history
     pretty-target)))

(defun substitute--highlight-face ()
  "Return face to highlight target of substitute."
  (if-let* ((face 'lazy-highlight)
            (facep face))
      face
    'secondary-selection))

(defun substitute--prompt-with-highlight (target scope)
  "Prompt for string while referencing TARGET and SCOPE.
Highlight the TARGET's matching occurences per the user option
`substitute-highlight'."
  (let ((pretty-target (substitute--pretty-target target)))
    (unwind-protect
        (progn
          (highlight-regexp target (substitute--highlight-face))
          (substitute--prompt-without-highlight pretty-target scope))
      (unhighlight-regexp target))))

(defun substitute--prompt (target scope)
  "Return appropriate prompt based on `substitute-highlight'.
Pass to it the TARGET and SCOPE arguments."
  (funcall
   (if substitute-highlight
       'substitute--prompt-with-highlight
     'substitute--prompt-without-highlight)
   target
   scope))

(defun substitute--scope-current-and-below (target)
  "Position point to match current TARGET and all below."
  (lambda ()
    (widen)
    (cond
     ((looking-at target)
      (goto-char (match-beginning 0)))
     ((save-excursion (looking-back target (beginning-of-line)))
      (goto-char (match-beginning 0))))))

(defun substitute--scope-current-and-above (target)
  "Position point to match current TARGET and all above."
  (lambda ()
    (widen)
    (cond
     ((looking-at target)
      (goto-char (match-end 0)))
     ((save-excursion (looking-back target (beginning-of-line)))
      (goto-char (match-end 0))))))

(defun substitute--scope-current-defun ()
  "Position point to the top after `narrow-to-defun'."
  (lambda ()
    (narrow-to-defun)
    (goto-char (point-min))))

(defun substitute--scope-top-of-buffer ()
  "Position point to the top of the buffer."
  (lambda ()
    (widen)
    (goto-char (point-min))))

(defun substitute--setup-scope (target scope)
  "Derive SCOPE for TARGET."
  (funcall
   (pcase scope
     ('below (substitute--scope-current-and-below target))
     ('above (substitute--scope-current-and-above target))
     ('defun (substitute--scope-current-defun))
     (_ (substitute--scope-top-of-buffer)))))

(defun substitute--operate (target sub &optional scope)
  "Substitute TARGET with SUB in SCOPE.
This is the subroutine of `substitute-target' and related."
  (let (count)
    (save-excursion
      (save-restriction
        (substitute--setup-scope target scope)
        (while (funcall (if (eq scope 'above)
                            're-search-backward
                          're-search-forward)
                        target nil t)
          (push (match-string-no-properties 0) count)
          (replace-match sub nil t))))
    (run-hook-with-args 'substitute-post-replace-hook
                        target sub (length count)
                        (substitute--scope scope))))

(defun substitute--target ()
  "Return target or report an error.
If the region is active, the target of the substitute is the text
within the region's boundaries.  Otherwise the target is the
target at point.

Report a `user-error' if no target is found."
  (cond
   ((region-active-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   (t (or (format "\\_<%s\\_>" (thing-at-point 'symbol t))
          (user-error "No substitution target at point")))))

(defmacro substitute-command (fn doc scope)
  "Produce substitute command using FN, DOC, and SCOPE."
  `(defun ,fn (target sub)
     ,(format
      "Replace TARGET with SUB %s.

When called interactively, TARGET is the symbol at point and SUB
is a string that is provided at the minibuffer prompt.

If the region is active, TARGET is the text within the region's
boundaries." doc)
     (interactive
      (let ((target (substitute--target)))
        (list target
              (substitute--prompt target current-prefix-arg))))
     (substitute--operate target sub ,scope)))

;;;###autoload
(substitute-command
 substitute-target-in-buffer
 "throughout the buffer"
 nil)

;;;###autoload
(substitute-command
 substitute-target-in-defun
 "in the defun (per `narrow-to-defun')"
 'defun)

;;;###autoload
(substitute-command
 substitute-target-below-point
 "to the end of the buffer"
 'below)

(defalias 'substitute-target-to-end-of-buffer
  'substitute-target-below-point)

;;;###autoload
(substitute-command
 substitute-target-above-point
 "to the beginning of the buffer"
 'above)

(defalias 'substitute-target-to-beginning-of-buffer
  'substitute-target-below-point)

(defun substitute-report-operation (target sub count scope)
  "Print message of substitution.
Report COUNTth substitutions of TARGET with SUB in SCOPE."
  (message "Substituted `%s' with `%s' %d times %s"
           (propertize (substitute--pretty-target target) 'face 'error)
           (propertize sub 'face 'success)
           count
           (propertize scope 'face 'warning)))

(provide 'substitute)
;;; substitute.el ends here
