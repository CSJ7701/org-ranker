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

(defcustom org-ranker-base-score-property "ORG-RANKER-BASE-SCORE"
  "The property name that org-ranker uses to set a base score.
Base scores are not changed by org-ranker, but contribute to the heading's score. They must be set manually.
Defaults to 'ORG-RANKER-BASE-SCORE'."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-rule-keyword "RANKER-RULE"
  "The keyword name that org-ranker uses to score rules in each buffer.
Defaults to 'RANKER-RULE'."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-exclude-header-name "EXCLUDE"
  "The name of the top-level heading under which org-ranker will place all excluded entries.
Defaults to 'EXCLUDE'."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-exclude-tag "exclude"
  "The tag which org-ranker uses to indicate excluded headings.
Defaults to 'exclude'."
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
  (delq nil
	(org-map-entries
	 (lambda ()
	   (unless (member org-ranker-exclude-tag (org-get-tags))
	     (let ((title (substring-no-properties (org-get-heading t t t t)))
		   (score (string-to-number (or (org-entry-get (point) org-ranker-score-property) "0"))))
	       (list (point) title score)))))))

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

      ;; If a headline with the 'exclude' tag exists, move it to the end of the buffer.
      (save-excursion
	(goto-char (point-min))
	(unless (org-at-heading-p)
	  (org-next-visible-heading 1))
	(org-map-entries
	 (org-ranker-move-headline-end)
	 "+exclude"
	 nil 'file))
      )))

;;; Assign Score
(defun org-ranker-parse-rule (rule)
  "Parse a single #+RANKER-RULE string into structured date.
Return a list: (key comparator value score)."
  (if (string-match "\\([^~=><!~]+\\)\\([~=><!~]=?\\|~~\\)?\\([^:]*\\):\\(.*\\)" rule)
      (let ((key (match-string 1 rule))
	    (comparator (match-string 2 rule))
	    (value (match-string 3 rule))
	    (score-or-func (match-string 4 rule)))
	(list key comparator value
	      (if (string-prefix-p "(" score-or-func)
		  (intern (substring score-or-func 1 -1)) ;; Parse as a function symbol
		(string-to-number score-or-func))))))

(defun org-ranker-get-rules ()
  "Retrieve and parse all #+RANKER-RULE lines in the current buffer."
  (let (rules)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+RANKER-RULE: \\(.+\\)$" nil t)
	(let ((rule (match-string 1)))
	  (push (org-ranker-parse-rule (substring-no-properties rule)) rules))))
    (reverse rules)))

(defun org-ranker-evaluate-rule (rule value)
  "Evaluate a RULE against a VALUE and return the corresponding score."
  (let ((key (nth 0 rule))
	(comparator (nth 1 rule))
	(target (nth 2 rule))
	(score-or-func (nth 3 rule)))
    ;; -- Check Comparison Type --
    (cond
     ;; String equality
     ((string= comparator "==")
      (if (string= value target) score-or-func 0))
     ;; String inequality
     ((string= comparator "!=")
      (if (string= value target) 0 score-or-func))
     ;; Substring matching (case insensitive)
     ((string= comparator "~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  score-or-func 0))
     ;; Numeric comparison
     ((member comparator '(">" "<" ">=" "<="))
      (let ((value-num (string-to-number value))
	    (target-num (string-to-number target)))
	(cond
	 ((and (equal comparator ">") (> value-num target-num)) score-or-func)
	 ((and (equal comparator "<") (< value-num target-num)) score-or-func)
	 ((and (equal comparator ">=") (>= value-num target-num)) score-or-func)
	 ((and (equal comparator "<=") (<= value-num target-num)) score-or-func)
	 (t 0))))

     ;; Custom scoring function
     ((functionp score-or-func)
      (funcall score-or-func value))

     ;; Default to 'no match' - score of 0
     (t 0))))

(defun org-ranker-calculate-score (rules)
  "Calculate the total score for the current heading using RULES."
  (let ((score 0))
    (dolist (rule rules)
      (let ((key (nth 0 rule))
	    (value (org-entry-get (point) (nth 0 rule)))) ;; Get the property value
	(when value
	  (setq score (+ score (org-ranker-evaluate-rule rule value))))))
    score))

(defun org-ranker-populate-scores ()
  "Calculates scores for all Org headings and insert/update a property with the score."
  (interactive)
  (let ((rules (org-ranker-get-rules))
	(score-property-name (or org-ranker-score-property "ORG-RANKER-SCORE"))
	(base-score-property-name (or org-ranker-base-score-property "ORG-RANKER-BASE-SCORE")))
    (org-map-entries
     (lambda ()
       (let* ((calculated-score (org-ranker-calculate-score rules))
	      (base-score (or (org-entry-get (point) base-score-property-name) "0"))
	      (score (+ (string-to-number base-score) calculated-score)))
	 (org-entry-put (point) score-property-name (number-to-string score)))))))

(defun org-ranker-set-base-score (score)
  "Sets the base score for the entry at point. Org-ranker will not change an entry's base score, but it will contribute to the total score org-ranker uses to sort entries."
  (interactive "nScore: ")
  (let ((property-name (or org-ranker-base-score-property "ORG-RANKER-BASE-SCORE")))
    (org-entry-put (point) property-name (number-to-string score))))
     
      
;;; Exclude

(defun org-ranker-create-exclude-heading ()
  "Creates the a heading with title 'ORG-RANKER-EXCLUDE-HEADER-NAME' and tag 'ORG-RANKER-EXCLUDE-TAG' at the end of the current buffer, if it does not already exist."
  (save-excursion
    (goto-char (point-max))
    (let ((exclude-header-name (or org-ranker-exclude-header-name "EXCLUDE"))
	  (exclude-tag (or org-ranker-exclude-tag "exclude"))
	  exclude-found)

      ;; Check for existing exclude header
      (org-map-entries
       (lambda ()
	 (and (string= (org-get-heading t t t t) exclude-header-name)
	      (setq exclude-found t)))
       (concat "+" org-ranker-exclude-tag)
       'file)

      ;; Create a heading at (point-max) if it doesnt exist
      (unless exclude-found
	(org-insert-heading)
	(insert exclude-header-name)
	(org-set-tags (list exclude-tag))))))

(defun org-ranker-get-exclude-heading-position ()
  "Returns the position of the EXCLUDE heading if it exists, nil otherwise."
  (save-excursion
    (goto-char (point-min))
    (let ((exclude-heading org-ranker-exclude-header-name))
      (catch 'found
	(org-map-entries
	 (lambda ()
	   (when (string= (org-get-heading t t t t) exclude-heading)
	     (throw 'found (point))))
	 (concat "+" org-ranker-exclude-tag) 'file)
	nil))))

(defun org-ranker-parse-exclude (rule)
  "Parse a single exclusion rule into structured data.
Returns a list: (key comparator value)."
  (if (string-match "\\([^~=><!~]+\\)\\([~=><!~]=?\\|~~\\)?\\([^:]*\\)" rule)
      (let ((key (match-string 1 rule))
	    (comparator (match-string 2 rule))
	    (value (match-string 3 rule)))
	(list key comparator value))))

(defun org-ranker-get-excludes ()
  "Retrieve the list of exclusion rules from the #+RANKER-EXCLUDE property."
  (let (excludes)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^#\\+RANKER-EXCLUDE: \\(.+\\)$" nil t)
	(let ((rule (match-string 1)))
	  (push (org-ranker-parse-exclude (substring-no-properties rule)) excludes))))
    (reverse excludes)))

(defun org-ranker-evaluate-exclude (exclude value)
  "Evaluate an EXCLUDE rule against a VALUE. Returns '1' if the evaluated heading should be excluded, 'nil' if not."
  (let ((key (nth 0 exclude))
	(comparator (nth 1 exclude))
	(target (nth 2 exclude)))
    ;; -- Check Comparison tyle --
    
    (cond
     ((string= comparator "==")
      (if (string= value target) 1 nil))
     ((string= comparator "!=")
      (if (string= value target) nil 1))
     ((string= comparator "~~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  1 nil))
     ((string= comparator "!~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  nil 1))
     ((member comparator '(">" "<" ">=" "<="))
      (let ((value-num (string-to-number value))
	    (target-num (string-to-number target)))
	(cond
	 ((and (equal comparator ">") (> value-num target-num)) 1)
	 ((and (equal comparator "<") (< value-num target-num)) 1)
	 ((and (equal comparator ">=") (>= value-num target-num)) 1)
	 ((and (equal comparator "<=") (<= value-num target-num)) 1)
	 (t nil))))
     (t nil))))

(defun org-ranker-evaluate-excludes (excludes)
  "Evaluate heading against all defined 'RANKER-EXCLUDE' rules. Returns a '1' if heading should be excluded, 'nil' if not."
  (let ((exclude-p nil))
    (dolist (exclude excludes)
      (let ((key (nth 0 exclude))
	    (value (org-entry-get (point) (nth 0 exclude))))
	(when value
	  (setq exclude-p (org-ranker-evaluate-exclude exclude value)))))
    exclude-p))

(defun org-ranker-exclude ()
  "Evaluates all headings in the current file, refiling them to the 'exclude' heading if they match any 'RANKER-EXCLUDE' rules."
  (interactive)
  (org-ranker-create-exclude-heading)
  (let ((rules (org-ranker-get-excludes))
	headings-to-move)
    (org-map-entries
     (lambda ()
       (let ((exclude-p (org-ranker-evaluate-excludes rules)))
	 (when exclude-p
	   (push (point) headings-to-move)))
       nil 'file))

    (dolist (pos headings-to-move)
      (goto-char pos)
      (org-cut-subtree)
      (goto-char (org-ranker-get-exclude-heading-position))
      (org-end-of-subtree t)
      (org-paste-subtree)
      (if (equal (org-current-level) 1)
	  (org-demote-subtree))
      (message "Excluded: %s" (org-get-heading t t t t)))))


;; TODO: FIX THIS.
;; Can't preallocate the list of points to visit, since paste will screw it up
;; Trying to count the number of headings I have to move,

(defun org-ranker-unexclude-one ()
  "Evaluates all subheadings under the 'exclude' heading, moving any that no longer match exclusion rules back to the main body."
  (interactive)
  (let ((rules (org-ranker-get-excludes))
        (exclude-pos (org-ranker-get-exclude-heading-position))
	headings-to-move)
    (save-excursion
      (goto-char exclude-pos) ;; Navigate to the exclude heading
      (org-map-entries
       (lambda ()
         ;; Check if the current heading matches any exclusion rules
	 (unless (org-ranker-evaluate-excludes rules)
	   (unless (string= (substring-no-properties (org-get-heading t t t t)) org-ranker-exclude-header-name)
	     (push (point) headings-to-move))))
       nil 'tree))
      (goto-char (car headings-to-move))
      (org-cut-subtree)
      (goto-char (point-min))
      (org-paste-subtree)
      ;(org-promote-subtree)
      ))

(defun org-ranker-unexclude ()
  "Evaluates all subheadings under the 'exclude' heading, moving any that no longer match exclusion rules back to the main body."
  (interactive)
  (let ((rules (org-ranker-get-excludes))
        (exclude-pos (org-ranker-get-exclude-heading-position))
        (moved-count 0))
    (save-excursion
      (goto-char exclude-pos) ;; Navigate to the exclude heading
      ;; Count how many subheadings need to be moved
      (let ((headings-to-move 0))
        (org-map-entries
         (lambda ()
           (unless (org-ranker-evaluate-excludes rules)
             (unless (string= (substring-no-properties (org-get-heading t t t t)) org-ranker-exclude-header-name)
               (setq headings-to-move (1+ headings-to-move)))))
         nil 'tree)
        ;; Iterate over the collected number of headings
        (dotimes (_ headings-to-move)
	  (org-ranker-unexclude-one)
	  (goto-char (org-ranker-get-exclude-heading-position))
	  (message "Unexcluded %d heading(s)." moved-count))))))

;;; Markup (highlight, more?)
;; TODO

;;; Wrapper
(defun org-ranker-sort ()
    "Perform all ranking and organizational tasks for Org headlines.

This function serves as the primary entry point for the `org-ranker` package. 
It evaluates the user-defined rules to calculate scores for each headline, 
excludes headlines that match exclusion rules, re-evaluates previously excluded 
headlines to bring back those that no longer match the exclusion criteria, and 
finally sorts the remaining headlines based on their calculated scores.

Steps performed:
1. Populate scores for all headlines based on defined scoring rules.
2. Move headlines that match exclusion rules under the 'exclude' heading.
3. Reintegrate headlines from the 'exclude' heading if they no longer match 
   exclusion rules.
4. Sort all remaining headlines in descending order of their scores.

Users who prefer more granular control over each step can use the following 
functions individually:
- `org-ranker-populate-scores`: Calculate and assign scores to headlines.
- `org-ranker-exclude`: Move headlines matching exclusion rules under 'exclude'.
- `org-ranker-unexclude`: Reintegrate previously excluded headlines.
- `org-ranker-sort-headlines`: Sort headlines based on their scores."
  (interactive)
  (org-ranker-populate-scores)
  (org-ranker-exclude)
  (org-ranker-unexclude)
  (org-ranker-sort-headlines)
  )

(provide 'org-ranker)

;;; org-ranker.el ends here


;; TODO: '~~', '!~' not being read correctly by org-ranker-parse-rule
;; TODO: Generalize get-excludes and get-rules to use custom keyword variables

