;;; latex-smart-comment.el --- Smart (un)commenting for latex-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/latex-smart-comment
;; Version: 0.0.1
;; Keywords: convenience

;; Package-Requires: ((emacs "27.1") (auctex "12.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines two interactive commands.

;; The command `latex-smart-comment-dwim' has four cases:

;;   1) region is not active, and
;;      point is not in a comment, and
;;      point is at the end of the line, or there is only whitespace
;;      between point and the end of the line;

;;   2) region is not active, and
;;      point is not in a comment;

;;   3) region is not active;

;;   4) region is active.

;; In case 1), `comment-dwim' is called, which will insert an inline
;; comment.  In case 2), the line where point is on is commented.
;; In case 3), the current line is uncommented.  In case 4),
;; `latex-smart-comment-do-region' is called on the active region.

;; `latex-smart-comment-do-region' also works on a discontinuous
;; region (such that the return value of `region-bounds' contains more
;; than one cons cell), and throws an error in four cases:

;;   1) the region is not active;

;;   2) the continuous region is neither fully commented nor
;;      uncommented;

;;   3) the discontinous region either contains a section that
;;      violates condition 2);

;;   4) the sections of the discontinuous region are not either all
;;      commented or all uncommented.

;; When all these conditions are met, the effect of this command is
;; to change the status of the region: if it's commented, it
;; uncomments it; if it is uncommented, it comments it.

;;; Code:

(require 'tex)
(require 'latex)

(defun latex-smart-comment-do-region ()
  "Comment or uncomment region of LaTeX text.

This commands also works on a discontinuous region (such that the
return value of `region-bounds' contains more than one cons
cell), and throws an error in four cases:

  1) the region is not active;

  2) the continuous region is neither fully commented nor
     uncommented;

  3) the discontinous region either contains a section that
     violates condition 2);

  4) the sections of the discontinuous region are not either all
     commented or all uncommented.

When all these conditions are met, the effect of this command is
to change the status of the region: if it's commented, it
uncomments it; if it is uncommented, it comments it."
  (interactive)
  (save-excursion
    (let* ((rgb (if (region-active-p)
                    (nreverse (region-bounds))
                  (user-error
                   "latex-smart-comment-do-region: Region is not active")))
           (sfn (lambda (b e)
                  (goto-char e)
                  (let* ((endcomm-p (TeX-in-comment))
                         (linen-b (line-number-at-pos b))
                         (oneline-p (= linen-b (line-number-at-pos e)))
                         bad-region)
                    (cond
                     ((and oneline-p endcomm-p)
                      'comm)
                     (oneline-p
                      (goto-char (1+ b))
                      (unless (TeX-in-comment)
                        'uncomm))
                     (endcomm-p
                      (while (> (line-number-at-pos) linen-b)
                        (end-of-line 0)
                        (unless (or (TeX-in-comment)
                                    (progn
                                      (beginning-of-line)
                                      (looking-at-p "[[:space:]]*$")))
                          (setq bad-region t)))
                      (unless bad-region
                        (goto-char (1+ b))
                        (when (TeX-in-comment)
                          'comm)))
                     (t
                      (unless (TeX-search-unescaped comment-start
                                                    'backward nil b t)
                        'uncomm))))))
           status)
      (dolist (r rgb)
        (let ((s (funcall sfn (car r) (cdr r))))
          (cond ((not s)
                 (user-error
                  "latex-smart-comment-do-region: Invalid region"))
                ((not status)
                 (setq status s))
                ((not (eq status s))
                 (user-error
                  "latex-smart-comment-do-region: Invalid region")))))
      (dolist (r rgb)
        (let ((beg (car r))
              (end (cdr r)))
          (if (eq status 'uncomm)
              (comment-region (car r) (cdr r))
            (goto-char end)
            (unless (looking-at-p "[[:space:]]*$")
              (save-excursion (insert "\n" comment-start)))
            (while (TeX-search-unescaped (concat comment-start "+")
                                         'backward t beg t)
              (delete-region (match-beginning 0) (match-end 0)))
            (goto-char beg)
            (when (TeX-in-comment)
              (if (not (looking-back (concat comment-start "+[[:space:]]*")
                                     (line-beginning-position)))
                  (insert "\n")
                (delete-region (match-beginning 0) (point)))
              (just-one-space))))))))

(defun latex-smart-comment-dwim ()
  "A smart Do What I Mean command LaTeX (un)commenting.

This command has four cases:

  1) region is not active, and
     point is not in a comment, and
     point is at the end of the line, or there is only whitespace
     between point and the end of the line;

  2) region is not active, and
     point is not in a comment;

  3) region is not active;

  4) region is active.

In case 1), `comment-dwim' is called, which will insert an inline
comment.  In case 2), the line where point is on is commented.
In case 3), the current line is uncommented.  In case 4),
`latex-smart-comment-do-region' is called on the active region."
  (interactive)
  (let ((ra (region-active-p))
        (comm (or (TeX-in-comment)
                  (TeX-in-commented-line))))
    (cond ((and (not ra) (not comm) (looking-at-p "[[:space:]]*$"))
           (funcall-interactively #'comment-dwim nil))
          ((and (not ra) (not comm))
           (comment-region (line-beginning-position)
                           (line-end-position)))
          ((not ra)
           (save-restriction
             (narrow-to-region (line-beginning-position)
                               (line-end-position))
             (funcall-interactively #'TeX-uncomment)))
          (t
           (funcall-interactively #'latex-smart-comment-do-region)))))

(provide 'latex-smart-comment)

;;; latex-smart-comment.el ends here
