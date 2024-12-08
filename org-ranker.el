;;; org-ranker.el --- Org-mode headline sorting extension

;; Author: CJ
;; Keywords: org, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; org-ranker sorts Org-mode headlines based on user-defined criteria.

;;; Code:

(require 'org)

;;; Variables

(defcustom org-ranker-score-property "ORG-RANKER-SCORE"
  "The property name that org-ranker uses to retrieve scores for sorting.
Defaults to 'ORG-RANKER-SCORE'."
  :type 'string
  :group 'org-ranker)

;;; Basic movement
(defun org-ranker-move-headline-up ()
  "Move the current headline up."
  (interactive)
  (let ((current-point (point)))
    (save-excursion
      (org-metaup)
      (goto-char current-point))))

(defun org-ranker-move-headline-down ()
  "Move the current headline-down."
  (interactive)
  (let ((current-point (point)))
    (save-excursion
      (org-metadown)
      (goto-char current-point))))

(defun org-ranker-move-headline-start ()
  "Move the current headline to the start of the buffer, after any non-headline content."
  (interactive)
  (org-cut-subtree)
  (let ((current-point (point)))
    (save-excursion
      (goto-char (point-min))
      (if (org-at-heading-p)
	  ()
	(org-next-visible-heading 1))
      (org-paste-subtree))
    (pop kill-ring)
    (goto-char current-point)))

(defun org-ranker-move-headline-end ()
  "Move the current headline to the end of the buffer, after all other headlines."
  (interactive)
  (org-cut-subtree)
  (let ((current-point (point)))
    (save-excursion
      (goto-char (point-max))
      (org-previous-visible-heading 1)
      (org-end-of-subtree t t)
      (org-paste-subtree))
    (pop kill-ring)
    (goto-char current-point)))

(defun org-ranker-move-headline-up-n (n)
  "Move the current Org heading up by N entries."
  (interactive "nNumber of positions to move up: ")
  (when (org-at-heading-p)
    (dotimes (_ n)
      (org-metaup))))

(defun org-ranker-move-headline-down-n (n)
  "Move the current Org headline down by N entries."
  (interactive "nNumber of position to move down: ")
  (when (org-at-heading-p)
    (dotimes (_n)
      (org-metadown))))

;;; Sorting
(defun org-ranker-get-headlines-with-scores ()
  "Retrieve a list of headlines with their scores from the current Org buffer."
  (org-map-entries
   (lambda ()
     (let ((title (substring-no-properties (org-get-heading t t t t)))
	   (score (string-to-number (or (org-entry-get (point) org-ranker-score-property) "0"))))
       (list (point) title score)))))

(defun org-ranker-delete-headlines ()
  "Delete all headlines in the current Org buffer.
Iterates from the end of the buffer to avoid position shifting issues."
  (let ((headlines (reverse (org-ranker-get-headlines-with-scores))))
    (dolist (headline headlines)
      (goto-char (nth 0 headline))
      (org-cut-subtree)
      (pop kill-ring) ; Avoid cluttering the kill-ring
      ))) 

(defun org-ranker-merge (left right)
  "Merge two sorted lists LEFT and RIGHT based on their scores."
  (let (result)
    (while (and left right)
      (if (<= (caddr (car left)) (caddr (car right))); Compare scores
	  (push (pop left) result)
	(push (pop right) result)))
    (nconc (nreverse result) left right)))

(defun org-ranker-merge-sort (headlines)
  "Sort HEADLINES using merge sort based on the score."
  (if (<= (length headlines) 1)
      headlines
    (let* ((mid (/ (length headlines) 2))
	   (left (seq-subseq headlines 0 mid))
	   (right (seq-subseq headlines mid)))
      (org-ranker-merge
       (org-ranker-merge-sort left)
       (org-ranker-merge-sort right)))))

(defun org-ranker-sort-headlines ()
  "Sort the headlines in the current Org buffer by their 'SCORE' property."
  (interactive)
  (let ((headlines (org-ranker-get-headlines-with-scores))
	sorted-headlines)
    ;; Sort
    (setq sorted-headlines (reverse (org-ranker-merge-sort headlines)))

    ;; Collect content for each sorted headline
    (let ((sorted-content (mapcar (lambda (h)
				    (goto-char (car h))
				    (buffer-substring-no-properties
				     (point)
				     (org-end-of-subtree t t)))
				  sorted-headlines)))
      ;; Remove all original headlines
      (org-ranker-delete-headlines)

      ;; Reinsert sorted headlines
      (dolist (content sorted-content)
	(goto-char (point-max))
	(insert content))
      )))

;;; Assign Score

;;; Exclude

;;; Markup (highlight, more?)

(provide 'org-ranker)

;;; org-ranker.el ends here
