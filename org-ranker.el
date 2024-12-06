;;; org-ranker.el --- Org-mode headline sorting extension

;; Author: CJ
;; Keywords: org, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.4"))

;;; Commentary:

;; org-ranker sorts Org-mode headlines based on user-defined criteria.

;;; Code:

(require 'org)

;;;###autoload
(defcustom org-ranker-sort-method 'priority
  "Sorting method for org-mode headlines."
  :type '(choice (const :tag "By Priority" priority)
                 (const :tag "By Date" date)
                 (const :tag "Custom Tag" custom))
  :group 'org-ranker)

;;;###autoload
(defun org-ranker-sort-headlines ()
  "Sort the headlines in the current Org buffer based on user-defined criteria."
  (interactive)
  (save-excursion
    (let ((headlines (org-ranker-extract-headlines)))
      (setq headlines (org-ranker-sort-headlines-by-criteria headlines))
      (org-ranker-update-buffer headlines))))

;;;###autoload
(defun org-ranker-extract-headlines ()
  "Extract headlines from the current Org buffer."
  (let (headlines)
    (org-map-entries
     (lambda ()
       (push (org-get-heading) headlines))
     nil 'tree)
    headlines))

;;;###autoload
(defun org-ranker-sort-headlines-by-criteria (headlines)
  "Sort headlines based on the user-defined sorting criteria."
  (pcase org-ranker-sort-method
    ('priority (sort headlines
                     (lambda (a b)
                       (let ((pri-a (org-entry-get (point-at-heading a) "PRIORITY"))
                             (pri-b (org-entry-get (point-at-heading b) "PRIORITY")))
                         (string< pri-a pri-b)))))
    ('date (sort headlines
                 (lambda (a b)
                   (let ((date-a (org-entry-get (point-at-heading a) "TIMESTAMP"))
                         (date-b (org-entry-get (point-at-heading b) "TIMESTAMP")))
                     (string< date-a date-b)))))
    ('custom (sort headlines
                   (lambda (a b)
                     (let ((tag-a (org-entry-get (point-at-heading a) "CUSTOM-TAG"))
                           (tag-b (org-entry-get (point-at-heading b) "CUSTOM-TAG")))
                       (string< tag-a tag-b)))))
    (_ headlines))  ; Return unsorted if invalid type
  headlines)

;;;###autoload
(defun org-ranker-update-buffer (headlines)
  "Update the buffer with sorted headlines."
  (let ((sorted-buffer (mapconcat #'identity headlines "\n")))
    (erase-buffer)
    (insert sorted-buffer)))

(provide 'org-ranker)

;;; org-ranker.el ends here
