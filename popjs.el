;;; popjs.el --- Pop a separate buffer for JavaScript multiline string editing

;; Copyright © 2014 Progiciels Bourbeau-Pinard inc.

;; Author: François Pinard <pinard@iro.umontreal.ca>
;; Maintainer: François Pinard <pinard@iro.umontreal.ca>
;; URL: https://github.com/pinard/popjs

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

;;; Commentary:

;; This pops a separate edit buffer to edit the contents of a
;; JavaScript multiline string, then reinserts the modified text in
;; place once the edition is done.

;;; Code:

(defface popjs-face
  '((((class color) (background light))
     (:foreground "gray"))
    (((class color) (background dark))
     (:foreground "gray")))
  "Face for a region while it is being edited.")

(defvar popjs-delimiter nil)
(defvar popjs-overlay nil)

(defun popjs-edit ()
  ;; Place cursor within JavaScript string first.
  (interactive)
  (if (not (memq major-mode '(js-mode js2-mode)))
      (popjs-edit-exit)
    (when popjs-overlay
      (error "Already editing a string as HTML."))
    (unless (eq (face-at-point) 'font-lock-string-face)
      ;; FIXME: Fuzzy at the very beginning or end of string.
      (error "Not within a string."))
    (let* ((start (or (previous-single-property-change (point) 'face)
                      (point-min)))
           (end (or (next-single-property-change (point) 'face)
                    (point-max)))
           ;; FIXME: Might fail because of lazy fontification!
           (first (save-excursion (goto-char start) (following-char)))
           (last (save-excursion (goto-char end) (preceding-char))))
      (unless (and (memq first '(?\' ?\")) (= last first))
        (error "Inconsistent string delimiters"))
      (setq popjs-delimiter first)
      (let ((string (buffer-substring (1+ start) (1- end)))
            (edit-buffer (generate-new-buffer (concat "*" (buffer-name) "*"))))
        ;; Dim and protect the original text.
        (setq popjs-overlay (make-overlay start end))
        (overlay-put popjs-overlay 'face 'popjs-face)
        (overlay-put popjs-overlay 'intangible t)
        (overlay-put popjs-overlay 'read-only t)
        ;; Initialize a popup edit buffer.
        (pop-to-buffer edit-buffer)
        (insert string)
        (goto-char (point-min))
        (while (search-forward "\\" nil t)
          (case (following-char)
            ((?\' ?\" ?\\) (delete-char -1) (forward-char))
            (?n (delete-char -1) (delete-char 1) (insert "\n"))
            (?\n (delete-char -1) (delete-char 1))))
        (normal-mode)))))

(defun popjs-edit-exit ()
  (interactive)
  (let ((buffer (overlay-buffer popjs-overlay))
        (start (overlay-start popjs-overlay))
        (end (overlay-end popjs-overlay)))
    (delete-overlay popjs-overlay)
    (setq popjs-overlay nil)
    (unless buffer
      (error "Original buffer vanished"))
    (goto-char (point-min))
    (insert "\\\n")
    (while (re-search-forward "['\"\\\\\n]" nil t)
      (cond ((= (preceding-char) ?\\)
             (backward-char) (insert "\\") (forward-char))
            ((= (preceding-char) ?\n)
             (backward-char) (insert "\\n\\") (forward-char))
            ((memq (preceding-char) (list popjs-delimiter ?\\))
             (backward-char) (insert "\\") (forward-char))))
    (goto-char (- (point-max) 2))
    (when (looking-at "\\\\\n")
      (replace-match ""))
    (let ((string (buffer-substring-no-properties (point-min) (point-max))))
      (with-current-buffer buffer
        (goto-char start)
        (delete-region start end)
        (insert popjs-delimiter
                string
                popjs-delimiter))
      (set-buffer-modified-p nil))
    (kill-buffer)
    (unless (one-window-p)
      (delete-window))
    (switch-to-buffer buffer)))

(provide 'popjs)
  
;;; popjs.el ends here
