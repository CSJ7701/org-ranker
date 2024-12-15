;;; org-ranker.el --- Org-mode headline sorting extension

;; Author: CJ
;; Keywords: org, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:
;;
;; `org-ranker.el` is an Org-mode extension that allows users to sort Org-mode
;; headlines based on custom criteria defined through properties and keywords.
;; This package provides functionality to assign scores to headlines, apply 
;; sorting based on those scores, and exclude or highlight certain entries.
;;
;; Main features include:
;; - Sorting headlines with a "score" property.
;; - Support for custom rules to define the score based on properties.
;; - Exclusion of specific entries based on rules.
;; - Highlighting specific entries based on custom criteria.
;;
;; Usage:
;; 1. Define `RANKER-RULE` headers with specific rules to assign scores to headings.
;; 2. Use `RANKER-EXCLUDE` to specify headings that should not be included in sorting.
;; 3. Apply `RANKER-HIGHLIGHT` to mark certain headings for visual emphasis.
;; 4. The package provides various customization options for adjusting sorting behavior.
;;
;; Example:
;;   #+RANKER-RULE: PROPERTY==high:value
;;   #+RANKER-RULE: LOCATION~=substring:other_value
;;   #+RANKER-EXCLUDE: KEYWORD==TEST
;;
;;   * Top-level heading
;;     :PROPERTIES:
;;     :PROPERTY: high
;;     :ORG-RANKER-SCORE: value
;;     :END:
;;   
;;   * Second heading
;;     :PROPERTIES:
;;     :LOCATION: String with a substring
;;     :ORG-RANKER-SCORE: 20
;;     :END:
;;
;;   * Excluded heading
;;      :PROPERTIES:
;;      :KEYWORD: TEST
;;      :END:
;; 
;; Customize the behavior via `M-x customize-group RET org-ranker RET`.
;;
;; For more details on available keywords, see the documentation below.
;;
;; To get started, load the package and apply sorting with:
;;   M-x org-ranker-sort
;;
;; This package requires Emacs 27.1 and Org-mode 9.4 or later.

;;; Code:

(require 'org)
(require 'csv)
(require 'hydra)

;;; =======  Variables  =======

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

(defcustom org-ranker-exclude-keyword "RANKER-EXCLUDE"
  "The keyword name that org-ranker uses to identify exclusion rules in each buffer.
Defaults to 'RANKER-EXCLUDE'."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-highlight-keyword "RANKER-HIGHLIGHT"
  "The keyword name that org-ranker uses to identify highlight rules in each buffer.
Defaults to 'RANKER-HIGHLIGHT'."
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

(defcustom org-ranker-rule-regex "\\([^~=><!~]+\\)\\(~~\\|!~\\|[~=><!~]=?\\)\\([^:]*\\):\\(.*\\)"
  "The regex string org-ranker uses to identify and split rule strings.
Used in 'org-ranker-get-rules'."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-rule-keyword-regex (concat "^#\\+" org-ranker-rule-keyword  ": \\(.+\\)$")
  "The regex string org-ranker uses to identify rule keywords."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-exclude-regex "\\([^~=><!~]+\\)\\(~~\\|!~\\|[~=><!~]=?\\)\\([^:]*\\)"
  "The regex string org-ranker uses to identify and split exclude strings.
Used in 'org-ranker-parse-exclude'."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-exclude-keyword-regex (concat "^#\\+" org-ranker-exclude-keyword ": \\(.+\\)$")
  "The regex string org-ranker uses to identify exclude keywords.
Used in 'org-ranker-get-excludes'"
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-highlight-regex "\\([^~=><!~]+\\)\\(~~\\|!~\\|[~=><!~]=?\\)\\([^:]*\\):\\(#\\(?:[0-9a-fA-F]\\{6\\}\\|[0-9a-fA-F]\\{3\\}\\)\\)"
  "The regex string org-ranker uses to identify and split highlight strings.
Used in org-ranker-parse-highlight."
  :type 'string
  :group 'org-ranker)

(defcustom org-ranker-highlight-keyword-regex (concat "^#\\+" org-ranker-highlight-keyword ": \\(.+\\)$")
  "The regex string org-ranker uses to identify highlight keywords.
Used in 'org-ranker-get-highlights'."
  :type 'string
  :group 'org-ranker)
						    

;;; =====  Basic movement  =======
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

;;; ===== Interface =====

(defun org-ranker-insert-keyword (keyword regex content)
  "Insert or update a line with the given KEYWORD and CONTENT, ensuring it matches REGEX.
This function ensures keywords of the same type are grouped together."
  (if (not (string-match-p regex content))
      (message "Input does not match the expected format.")
    (save-excursion
      (goto-char (point-min))
      ;; Search for the last instance of the keyword section
      (let ((section-found nil))
        (while (re-search-forward (format "^#\\+%s:.*" (regexp-quote keyword)) nil t)
          (setq section-found t))
        ;; Insert the keyword in the right place
        (if section-found
            ;; If the section exists, insert the new content right after the last match
            (progn
              (end-of-line)
              (insert (format "\n#+%s: %s" keyword content)))
          ;; Otherwise, add a new section at the top
          (goto-char (point-min))
          (insert (format "#+%s: %s\n" keyword content)))))))

(defun org-ranker-add-rule (rule)
  "Add RULE keyword to the org document at point."
  (interactive "sRULE: ")
  (let ((regex org-ranker-rule-regex)
        (keyword org-ranker-rule-keyword))
    (org-ranker-insert-keyword keyword regex rule)))

(defun org-ranker-add-exclude (exclude)
  "Add EXCLUDE keyord to the org document at point."
  (interactive "sEXCLUDE: ")
  (let ((regex org-ranker-exclude-regex)
        (keyword org-ranker-exclude-keyword))
    (org-ranker-insert-keyword keyword regex exclude)))

(defun org-ranker-add-highlight (highlight)
  "Add HIGHLIGHT keyword to the org document at point."
  (interactive "sHIGHLIGHT: ")
  (let ((regex org-ranker-highlight-regex)
        (keyword org-ranker-highlight-keyword))
    (org-ranker-insert-keyword keyword regex highlight)))

(defhydra org-ranker-hydra (:color blue :hint nil)
  "Org Ranker Actions: "
  ;; Basic Actions
  ("s" org-ranker-sort "Process Rules" :column "Common")
  ("b" org-ranker-set-base-score "Set Entry's Base Score" :column "Common")
  ("r" org-ranker-add-rule "Add Rule" :column "Common")
  ("x" org-ranker-add-exclude "Add Exclude" :column "Common")
  ("h" org-ranker-add-highlight "Add Highlight" :column "Common")
  ;; Manual
  ("mh" org-ranker-manual-highlight "Highlight Entry" :column "Manual")
  ("mH" org-ranker-remove-highlight "Remove Highlight on Entry" :column "Manual")
  ("mp" org-ranker-move-headline-up "Move Up" :column "Manual")
  ("mn" org-ranker-move-headline-down "Move Down" :column "Manual")
  ("ma" org-ranker-move-headline-start "Move to Start" :column "Manual")
  ("me" org-ranker-move-headline-end "Move to End" :column "Manual")
  ("mP" org-ranker-move-headline-up-n "Move Up N Lines" :column "Manual")
  ("mN" org-ranker-move-headline-down-n "Move Down N Lines" :column "Manual")
  ;; Manual Actions
  ("]" org-ranker-exclude "Process New Excludes" :column "Actions")
  ("[" org-ranker-unexclude "Remove Old Excludes" :column "Actions")
  ("{" org-ranker-highlight "Process New Highlights" :column "Actions")
  ("}" org-ranker-remove-highlights "Remove Old Highlights" :column "Actions")
  ("'" org-ranker-sort "Sort Headlines by Score" :column "Actions")
  ("\"" org-ranker-populate-scores "Populate Scores" :column "Actions")
  ;; Import
  ("c" org-ranker-import-csv "Import CSV File" :column "Import"))

;;; ===== Sorting =====
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
  (if (string-match org-ranker-rule-regex rule)
      (let ((key (upcase (match-string 1 rule)))
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
      (while (re-search-forward org-ranker-rule-keyword-regex nil t)
	(let ((rule (match-string 1)))
	  (push (org-ranker-parse-rule (substring-no-properties rule)) rules))))
    (reverse (delq nil rules))))

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
     ((string= comparator "~~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  score-or-func 0))
     ((string= comparator "!~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  0 score-or-func))
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
	    (value (or (org-entry-get (point) (nth 0 rule)) ""))) ;; Get the property value
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
  (if (string-match org-ranker-exclude-regex rule)
      (let ((key (match-string 1 rule))
	    (comparator (match-string 2 rule))
	    (value (match-string 3 rule)))
	(list key comparator value))))

(defun org-ranker-get-excludes ()
  "Retrieve the list of exclusion rules from the #+RANKER-EXCLUDE property."
  (let (excludes)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-ranker-exclude-keyword-regex nil t)
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
	    (value (or (org-entry-get (point) (nth 0 exclude)) "")))
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
       nil 'tree))
    (dolist (pos (reverse headings-to-move))
      (goto-char pos)
      (org-cut-subtree)
      (goto-char (org-ranker-get-exclude-heading-position))
      (org-end-of-subtree t)
      (org-paste-subtree)
      (if (equal (org-current-level) 1)
	  (org-demote-subtree))
      (message "Excluded: %s" (org-get-heading t t t t)))))

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

(defun org-ranker-parse-highlight (highlight-rule)
  "Parse a single RANKER-HIGHLIGHT rule into its components.
Returns a list of (PROPERTY COMPARATOR VALUE COLOR)."
  (when (string-match org-ranker-highlight-regex highlight-rule)
    (let* ((property (match-string 1 highlight-rule))
	   (comparator (match-string 2 highlight-rule))
	   (value (match-string 3 highlight-rule))
           (color (match-string 4 highlight-rule)))
      (list property comparator value color))))

(defun org-ranker-get-highlights ()
  "Retrieve all RANKER-HIGHLIGHT rules from the file and parse them."
  (let (highlights)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-ranker-highlight-keyword-regex nil t)
        (let ((rule (match-string 1)))
          (push (org-ranker-parse-highlight (substring-no-properties rule)) highlights))))
    (reverse highlights)))

(defun org-ranker-evaluate-highlight (highlight value)
  "Evaluate a HIGHLIGHT rule against a VALUE.
Returns the color if the evaluated heading should be highlighted, 'nil' if not."
  (let ((key (nth 0 highlight))
	(comparator (nth 1 highlight))
	(target (nth 2 highlight))
	(color (nth 3 highlight)))
    (cond
     ((string= comparator "==")
      (if (string= value target) color nil))
     ((string= comparator "!=")
      (if (string= value target) nil color))
     ((string= comparator "~~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  color nil))
     ((string= comparator "!~")
      (if (string-match-p (regexp-quote (downcase target))
			  (downcase value))
	  nil color))
     ((member comparator '(">" "<" ">=" "<="))
      (let ((value-num (string-to-number value))
	    (target-num (string-to-number target)))
	(cond
	 ((and (equal comparator ">") (> value-num target-num)) color)
	 ((and (equal comparator "<") (< value-num target-num)) color)
	 ((and (equal comparator ">=") (>= value-num target-num)) color)
	 ((and (equal comparator "<=") (<= value-num target-num)) color)
	 (t nil))))
     (t nil))))

(defun org-ranker-evaluate-highlights (highlights)
  "Evaluate a heading against all defined 'RANKER-HIGHLIGHT' rules. Returns a hex code if the heading should be highlighted, 'nil' if not.
This function does not handle conflicts - if there are multiple matching rules, the one that is defined first takes precedence."
  (let ((matches nil))
    (dolist (rule highlights)
      (let* ((key (nth 0 rule))
	     (value (or (org-entry-get (point) key) ""))
	     (new-color (when value (org-ranker-evaluate-highlight rule value))))
	(when new-color
	  (push new-color matches))))
    (car (last matches))))

(defun org-ranker-highlight-current-line (color)
  "Apply an overlay to the current line with specified COLOR.
Helper function for 'org-ranker-highlight' - not meant to be used manually.
If you want to manually highlight a heading, use 'org-ranker-manual-highlight'."
  (let ((start (line-beginning-position))
	(end (line-end-position)))
    (remove-overlays start end 'org-ranker-overlay t)
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'face `(:background ,color))
      (overlay-put overlay 'org-ranker-overlay t))))

(defun org-ranker-remove-highlights ()
  "Remove all 'org-ranker' overlays in the current buffer."
  (interactive) (remove-overlays (point-min) (point-max) 'org-ranker-overlay t))

(defun org-ranker-remove-highlight ()
  "Remove the overlay from the current line."
  (interactive)
  (let ((start (line-beginning-position))
	(end (line-end-position)))
    (remove-overlays start end 'org-ranker-overlay t)))

(defun org-ranker-highlight ()
  "Highlight Org headings based on 'org-ranker-evaluate-highlights'.
A heading cannot have more than one highlight color. If there are multiple highlight rules that match a single headline, the one that was defined first will take precedence. In practice, the higher a highlight rule is in the org-buffer itself, the higher its priority."
  (interactive)
  (org-map-entries
   (lambda ()
     (let ((color (org-ranker-evaluate-highlights (org-ranker-get-highlights))))
       (unless (string= (org-get-heading t t t t) org-ranker-exclude-header-name)
	 (when color
	   (org-ranker-highlight-current-line color)
	   ;;(insert color)
	   ))))))

(defun org-ranker-manual-highlight (color)
  "Manually apply a highlight to the current line with the specified COLOR."
  (interactive "sEnter color (e.g., #ff0000): ")
  (org-ranker-highlight-current-line color))

;;; Import
;; From CSV
;; From JSON?
;; From YAML?

(defun org-ranker-get-csv-headers (csv-file &optional delimiter row-separator)
  "Get the headers from the first row of a CSV file.
CSV-FILE is the file path.
DELIMITER specifies the column delimiter (default: \",\").
ROW-SEPARATOR specifies the row separator (default: newline)."
  (let ((delimiter (or delimiter ","))
	(row-separator (or row-separator "\n"))
	headers)
    (with-temp-buffer
      (insert-file-contents csv-file)
      (let ((first-line (car (split-string (buffer-string) row-separator t))))
	(setq headers (split-string first-line delimiter t))))
    headers))

(defun org-ranker-import-csv-arguments ()
  "Get the arguments for 'org-ranker-import-csv'."
  (let* ((file (read-file-name "CSV File: "))
	 (name-column (completing-read "Select Header Column: " (org-ranker-get-csv-headers file))))
    (list file name-column)))
    

(defun org-ranker-import-csv (csv-file header-column &optional first-line-contains-keys keys)
  (interactive (org-ranker-import-csv-arguments))
  (with-temp-buffer
    (insert-file-contents csv-file)
    (let* ((first-line-keys (or first-line-contains-keys t))
	   (contents (csv-parse-buffer first-line-keys (current-buffer)))
	   )
      (with-current-buffer (generate-new-buffer "*Org-Ranker CSV Import*")
	(org-mode)
	;; Loop over each line (row) in the CSV data
	(mapc (lambda (line)
		(let* ((headline (cdr (assoc header-column line)))
		       (properties (cl-remove-if-not #'cdr line)))
		  (if headline
		      (progn
			;; Create the org-mode heading
			(insert (format "* %s\n" headline))

			;;Add properties under the headline
			(mapc (lambda (prop) ;; Loop over each field
				(org-entry-put nil (car prop) (cdr prop)))
			      properties))
			(error "Header column '%s' not found in row" header-column))))
	      contents)
	(switch-to-buffer (current-buffer))))))

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
  (org-ranker-highlight)
  (org-cycle-overview)
  (goto-char (point-min))
  )

(provide 'org-ranker)

;;; org-ranker.el ends here

;; TODO: Add import types
;;       - YAML
;;       - JSON
;;       Not sure these fit the structure of this package

