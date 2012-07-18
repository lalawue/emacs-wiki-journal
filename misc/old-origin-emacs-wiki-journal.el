;;; emacs-wiki-journal.el --- Maintain a weblog style journal with emacs-wiki

;; Copyright (C) 2003, 2004 Gary V. Vaughan (gary AT gnu DOT org)
;; Copyright (C) 2004 Ole Arndt (ole AT sugarshark DOT com)
;; Copyright (C) 2004 Hoan Ton-That (hoan AT ton-that DOT org)
;; Copyright (C) 2004 Jose A. Ortega Ruiz (jao AT gnu DOT org)
;; Copyright (C) 2004, 2005 Michael Olson
;; Copyright (C) 2004 Yamagata Yoriyuki
;; Copyright (C) 2004, 2005 Yu Li

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-journal.el
;; Version: 2.66
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Maintain weblog style journal in a local Emacs Wiki
;; URL: http://www.mwolson.org/projects/EmacsWiki.html
;; Compatibility: XEmacs21

;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
;; MA 02111-1307, USA.

;;;_* Commentary

;; This file is the part of the Emacs Wiki project that allows you to
;; maintain a journal.

;;;_ + Startup

;; To begin using emacs-wiki-journal, put this in your .emacs file:
;;
;;   (require 'emacs-wiki-journal)
;;
;; Now you can add an entry to your journal with M-x
;; emacs-wiki-journal-add-entry, give it a WikiName for a category to
;; index the entry under (usually beginning "Category") and a header
;; line for the new entry.  A suitable header for today's entry will
;; added to emacs-wiki-journal-wiki where you add the body for the
;; entry and a cross reference will be added to the category index.
;; If you have a PNG icon in the emacs-wiki-category-icons-url then
;; the icon named after the CategoryWiki will be inserted in the
;; journal header.
;;
;; You may need to set up a stylesheet to layout the journal page as
;; you want it.
;;
;; You should also type M-x customize-group, and give the name
;; "emacs-wiki-journal".  Change it to suit your preferences.  Each of
;; the options has its own documentation.

;;;_ + Key Bindings

;; You might want to bind emacs-wiki-journal-add-entry to a key in your
;; global keymap:
;;
;;   (define-key ctl-x-4-map
;;               "j" 'emacs-wiki-journal-add-entry-other-window)
;;
;; And also in emacs-wiki mode:
;;
;;   (add-hook 'emacs-wiki-mode-hook
;;             (lambda ()
;;               (local-set-key "C-cj" 'emacs-wiki-journal-add-entry)))

;;;_ + Code:

;; The parts of this code:
;;
;; * Customization group setup
;; * Category Index maintenance
;; * JournalWiki maintenance

;;;_ + Contributors

;; Gary Vaughan (gary AT gnu DOT org) started the Journal in 2003.
;; The original date for his 0.0.3 version is Fri, 2 May 2003
;;
;; Ole Arndt (ole AT sugarshark DOT com) was the maintainer from April
;; 2004 to September 2004.  He added Category support.
;;
;; Hoan Ton-That (hoan AT ton-that DOT org) contributed multiple
;; journal support.
;;
;; Jose A. Ortega Ruiz (jao AT gnu DOT org) contributed
;; journal-splitting support, which allows the number of entries to
;; keep on a page to be specified.  Old entries are moved to another
;; page.
;;
;; Yamagata Yoriyuki (yoriyuki AT mbg DOT ocn DOT ne DOT jp)
;; contributed a patch that fixed some breakage in
;; `emacs-wiki-journal-category-alist'.  He also contributed a 15-line
;; patch that allows for more than 10 overflow journal files to be
;; used.
;;
;; Yu Li (liyu2000 AT hotmail DOT com) began the use of date tags in
;; category indexes and individual journal entries.

;;;_ + Todo

;; Only add category icons if the icon file exists
;; Search for category icon using emacs-wiki-image-regexp
;; Read in the CategoryWiki using emacs-wiki-read-name
;; Split the JournalWiki up by year and generate annual chronological indices

;;;_* Prerequisites
(require 'cl)
(require 'emacs-wiki)
(defvar emacs-wiki-journal-loaded nil)

;;;_* Options

(defun emacs-wiki-journal-option-customized (sym val)
  (set sym val)
  (when emacs-wiki-journal-loaded
    (emacs-wiki-journal-update-wiki-project)))

(defgroup emacs-wiki-journal nil
  "Options controlling the behaviour of emacs-wiki journaling.
See `emacs-wiki-journal-add-entry' for more information."
  :group 'emacs-wiki)

(defvar emacs-wiki-journal-project-default-name "JournalWiki"
  "Default name of Journal project.
This is used by `emacs-wiki-journal-update-wiki-project' to make
sure that any old entries are removed correctly.")

(defcustom emacs-wiki-journal-project
  emacs-wiki-journal-project-default-name
  "The name of this project, used when referencing it from other
emacs-wiki projects."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-directory "~/Blog"
  "The directory that contains your journal entries."
  :type 'directory
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-publishing-directory
  "~/website/blog"
  "The directory where all journal entries are published to."
  :type 'directory
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-server-prefix
  "../blog/"
  "The location of the publishing directory with respect to the
locations of the publishing directories of other emacs-wiki
projects."
  :type 'directory
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-wiki "MyJournal"
  "Default prefix of name of the file to which journal entries
are added."
  :type 'string
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-default-category "CategoryMisc"
  "The default category used when adding entries."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-use-other-window nil
  "If non-nil, emacs-wiki-journal will open in another window."
  :type 'boolean
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-icons-subdirectory "images"
  "Default base url to a directory containing category icons."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-time-format "%a, %e %b. %2y"
  "Format for the date string of journal entries.
See `format-time-string' for more information."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-category-regexp
  "^Category\\([A-Z][a-z]+\\)+$"
  "Each of the category index Wiki files start with this prefix."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-time-format-category nil
  "Format for the date/time string of category page entries.
If `nil', `emacs-wiki-journal-time-format' is used.

See `format-time-string' for more information."
  :type '(choice (string :tag "Time string format: ")
                 (const :tag "nil" nil))
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-title nil
  "Title of the main journal page. If `nil' no title is inserted."
  :type '(choice (string :tag "Title string: ")
                 (const :tag "No title" nil))
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-old-title-format-1 "Entries from %s to %s"
  "Title of the old journal pages. If `nil', no title is inserted."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-old-title-format-2 "Entries of %s"
  "Title of the old journal pages if it contains the entries of only
one day. If `nil', no title is inserted."
  :type 'string
  :group 'emacs-wiki-journal)


(defcustom emacs-wiki-journal-entries-per-page 4
  "Maximum number of journal entries per page."
  :type 'integer
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-more-entries-link "(more entries...)"
  "Text used for the more entries link at the end of a journal page."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-index-title-threshold t
  "*If nil, filenames are always used in the index.
This is faster, but less informative.

If a positive integer, only that many bytes will be scanned for a
#title directive.  Otherwise, the entire wiki file is scanned for
a #title directive."
  :type '(choice (integer :tag "Bytes to scan: ")
                 (const :tag "Scan for title" t)
                 (const :tag "Do not scan for title" nil))
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-date-format "%4Y-%2m-%2e"
  "Format for the string used in the date tags.
Unlike `emacs-wiki-journal-time-format', this should be
consistent for entries made on the same day.

See `format-time-string' for more information."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-date-tag-template
  "<!-- date: %s -->"
  "The tag inserted after each journal entry.
It is placed after the category field, before the point.

It also is used on the category index page, inserted after an
entry.

The \"%s\" text will be replaced with the value of
`emacs-wiki-journal-date-format'."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-prepare-welcome-page 
  'emacs-wiki-journal-copy-current
  "The method to create the welcome page."
  :type 'function
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-self-link-name "link this article"
  "The name of the self link in the article"
  :type 'string
  :group 'emacs-wiki-journal)

;;;_* Internal Variables

(defvar emacs-wiki-journal-current-page nil
  "The current journal page")

(defvar emacs-wiki-journal-current-page-number nil
  "The number of journal pages")

;;;_* Internal Functions

;; From planner.el

(defun emacs-wiki-journal-expand-file-name (page)
  "Return the file name of the given EmacsWiki journal page"
  (expand-file-name page emacs-wiki-journal-directory))

(defun emacs-wiki-journal-find-file (page &optional command)
  "Open the emacs-wiki PAGE by name.
If COMMAND is non-nil, it is the function used to visit the file."
  (make-directory emacs-wiki-journal-directory t)
  (funcall (or command 'find-file)
           (emacs-wiki-journal-expand-file-name page)))

(defun emacs-wiki-journal-find-file-noselect (page &optional command)
  "Open the emacs-wiki PAGE by name, without selecting its buffer.
If COMMAND is non-nil, it is the function used to visit the file."
  (make-directory emacs-wiki-journal-directory t)
  (funcall (or command 'find-file-noselect)
           (emacs-wiki-journal-expand-file-name page)))

(defun emacs-wiki-journal-category-alist (&optional no-check-p)
  "Return possible category index Wikis in `emacs-wiki-journal-directory'.
If NO-CHECK-P is non-nil, then don't check for changes in the
directories to decide whether to re-read the cached alist, just
re-read the disk."
  (let ((file-alist
         (cadr (assoc emacs-wiki-journal-project 
		      emacs-wiki-file-alist)))
        (category-alist nil))
    (save-match-data
      (mapc
       (lambda (file)
         (if (string-match emacs-wiki-journal-category-regexp
                           (car file))
             (setq category-alist (cons file category-alist))))
       file-alist))
    category-alist))

(defun emacs-wiki-journal-prompt-for-category-wiki ()
  "Prompt for a category index file."
  (let ((file-alist (emacs-wiki-journal-category-alist))
        (emacs-wiki-default-page emacs-wiki-journal-default-category))
    (emacs-wiki-read-name file-alist "Category: ")))

(defalias 'emacs-wiki-journal-make-link 'emacs-wiki-make-link)

(defun emacs-wiki-journal-add-category-entry
  (wiki target-url &optional link-description)
  "Find category index file and add an entry for today."
  (emacs-wiki-journal-find-file wiki)
  (undo-boundary)
  (goto-char (point-min))

  ;; Skip to first heading
  (when (re-search-forward "^\\* " nil t)
        (beginning-of-line)
        (forward-line 1))
  (let ((insert-date-entry
         (format emacs-wiki-journal-date-tag-template
                 (format-time-string
                  emacs-wiki-journal-date-format)))
        (date-string (format-time-string
                      (or emacs-wiki-journal-time-format-category
                          emacs-wiki-journal-time-format)))
        latest-date latest-date-entry)

    ;; Skip to first entry
    (re-search-forward "^ ?- " nil t)
    (beginning-of-line)
    (save-excursion
      (setq latest-date
            (and (re-search-forward
                  (format emacs-wiki-journal-date-tag-template
                          "\\(.+\\)")
                  nil t)
                 (match-string 1))))
    (setq latest-date-entry
          (format emacs-wiki-journal-date-tag-template
                  latest-date))

    ;; Insert new entry
    (insert (concat " - " date-string " "
                    (emacs-wiki-journal-make-link target-url
                                                  link-description)
                    "\n"))
    (unless (string= latest-date-entry insert-date-entry)
      (insert (concat insert-date-entry "\n"
                      (if latest-date
                          (concat "\n** " latest-date "\n\n")
                        ""))))))

(defun emacs-wiki-journal-1+-string (value)
  "Increment an ascii encoded number."
  (int-to-string (1+ (string-to-int value))))

(defun emacs-wiki-journal-prepare-journal ()
  "Check if maximum number of journal entries reached
and react accordingly."
  (emacs-wiki-journal-set-current-page)
  (when (and (> emacs-wiki-journal-entries-per-page 0)
             (>= (emacs-wiki-journal-entries)
                emacs-wiki-journal-entries-per-page))
    (emacs-wiki-journal-update-old-journal-title)
    (emacs-wiki-journal-create-new-page)))

(defun emacs-wiki-journal-set-current-page ()
  "set the current page and page number"
  (unless (and emacs-wiki-journal-current-page 
	       emacs-wiki-journal-current-page-number
	       (or
		(get-buffer emacs-wiki-journal-current-page)
		(file-exists-p (emacs-wiki-journal-expand-file-name
			       emacs-wiki-journal-current-page))))
    (let ((last (emacs-wiki-journal-last-page)))
      (setq emacs-wiki-journal-current-page (car last))
      (setq emacs-wiki-journal-current-page-number (cdr last)))))

(defun emacs-wiki-journal-create-new-page ()
  "Create the new journal page"
  (let* ((new-number (1+ emacs-wiki-journal-current-page-number))
	 (new-page (format "%s%d" emacs-wiki-journal-wiki new-number)))

    (emacs-wiki-journal-find-file new-page)    
    (setq emacs-wiki-journal-current-page-number new-number)
    (setq emacs-wiki-journal-current-page new-page)))

(defun emacs-wiki-journal-entries ()
  "Return the number of journal entries in the current journal file."
  (let ((jf (emacs-wiki-journal-expand-file-name 
	     emacs-wiki-journal-current-page)))
    (if (or (file-exists-p jf) 
	    (get-buffer emacs-wiki-journal-current-page))
      (with-current-buffer (find-file-noselect jf)
        (save-excursion
          (labels
              ((count-ent (n)
                          (if (re-search-forward "^\\*\\* " nil t)
                              (count-ent (1+ n))
                            n)))
            (goto-char (point-min))
            (count-ent 0))))
      0)))

(defun emacs-wiki-journal-nextday()
  (goto-char (point-min))
  (and (re-search-forward
	(format emacs-wiki-journal-date-tag-template
		"\\(.+\\)")
	nil t)
       (match-string 1)))

(defun emacs-wiki-journal-lastday()
  (goto-char (point-max))
  (and (re-search-backward
	(format emacs-wiki-journal-date-tag-template
		"\\(.+\\)")
	nil t)
       (match-string 1)))

(defun emacs-wiki-journal-mktime(s)
  (if s
      (let* ((date (split-string s "-"))
	     (time (encode-time 0 0 0
				(string-to-int (nth 2 date))
				(string-to-int (nth 1 date))
				(string-to-int (nth 0 date)))))
	(format-time-string emacs-wiki-journal-time-format time))))

(defun emacs-wiki-journal-update-old-journal-title (&optional page)
  "Update the title of an old journal page."
  (when (and emacs-wiki-journal-old-title-format-1 
	     emacs-wiki-journal-old-title-format-2)
    (with-current-buffer (emacs-wiki-journal-find-file-noselect 
			  (if page page emacs-wiki-journal-current-page))
      (goto-char (point-min))
      (when (re-search-forward "^#title " nil t)
	(beginning-of-line)
	(let ((kill-whole-line t)) (kill-line)))
      (let* ((to (emacs-wiki-journal-nextday))
	     (from (emacs-wiki-journal-lastday)))
	(goto-char (point-min))
	(insert "#title "
		(if (not (string= from to))
		    (format emacs-wiki-journal-old-title-format-1
			    (emacs-wiki-journal-mktime from)
			    (emacs-wiki-journal-mktime to))
		  (format emacs-wiki-journal-old-title-format-2
			  (emacs-wiki-journal-mktime from))))
	(insert "\n"))
      (save-buffer (current-buffer))
      (kill-buffer (current-buffer)))))

(defun emacs-wiki-journal-pages ()
  "Return the current number of journal wiki pages."
  (cdr (emacs-wiki-journal-last-page)))

(defun emacs-wiki-journal-last-page ()
  "Return a pair with the last journal page name and number."
   (let ((files
 	 (directory-files emacs-wiki-journal-directory
 			  nil
 			  (concat emacs-wiki-journal-wiki
 				  "[0-9]+\\'")
 			  'true))
 	(final 1))
     (mapc (lambda (file)
             (let* ((num-string 
                     (substring file (length emacs-wiki-journal-wiki)))
                    (num (string-to-number num-string)))
               (setq final (max num final))))
           files)
     (cons (format "%s%d" emacs-wiki-journal-wiki final) final)))

(defun emacs-wiki-journal-more-entries-anchor ()
  "Return a wiki anchor to last stored journal page."
  (if (> emacs-wiki-journal-current-page-number 1)
      (emacs-wiki-journal-make-link
       (format "%s%d"
	       emacs-wiki-journal-wiki
	       (1- emacs-wiki-journal-current-page-number))
       emacs-wiki-journal-more-entries-link)))


(defun emacs-wiki-journal-publish-index ()
  "Replacement for `emacs-wiki-publish-index'."
  (interactive)
  (if (not (string-equal emacs-wiki-project
                         emacs-wiki-journal-project))
      (emacs-wiki-publish-index)
    (emacs-wiki-generate-index)))

;;;_* User Functions

(defun emacs-wiki-journal-get-title (page) 
  (save-excursion
    (save-window-excursion
      (emacs-wiki-find-file page)
      (goto-char (point-min))
      (or
       (when (re-search-forward "^#title\\s-+\\(.+\\)$" nil t)
	 (match-string-no-properties 1))
       page))))

;;;###autoload
(defun emacs-wiki-journal-add (category-wiki journal-entry-heading)
  "Add a journal entry under category with a heading"
  (interactive)
  (let* ((anchor-base "p")
         (anchor-regexp (concat "^#"
                                (regexp-quote anchor-base)
                                "\\([0-9][0-9]*\\)"))
         (anchor-ord "0")
	 (self-link nil))

    (emacs-wiki-journal-set-current-page)

    (emacs-wiki-journal-find-file
     emacs-wiki-journal-current-page
     (when emacs-wiki-journal-use-other-window
       'find-file-other-window))

    (goto-char (point-min))

    (when emacs-wiki-journal-title
      (unless (re-search-forward "^#title " nil t)
        (insert "#title " emacs-wiki-journal-title "\n")
        (forward-line)))

    (save-excursion
      (if (re-search-forward anchor-regexp nil t)
          (setq anchor-ord
                (emacs-wiki-journal-1+-string
                 (buffer-substring (match-beginning 1)
                                   (match-end 1))))
        (when (and (> emacs-wiki-journal-entries-per-page 0)
                   (> emacs-wiki-journal-current-page-number 1)
                   (zerop (emacs-wiki-journal-entries)))
          (insert "\n\n" (emacs-wiki-journal-more-entries-anchor)
                  "\n"))))

    (setq self-link        
	  (concat emacs-wiki-journal-current-page
		  "#" anchor-base anchor-ord))
    (save-excursion
      (emacs-wiki-journal-add-category-entry
       category-wiki
       self-link
       journal-entry-heading))

    ;; skip # lines and blanks at the start of the buffer
    (while (and (looking-at "^\\(#\\|\n\\)")
                (equal 0 (forward-line 1))))

    ;; skip to first heading
    (re-search-forward "^\\* " nil t)
    (beginning-of-line)

    (let* ((time-string (format-time-string
                         emacs-wiki-journal-time-format))
           (date-string
            (format emacs-wiki-journal-date-tag-template
                    (format-time-string
                     emacs-wiki-journal-date-format)))
           (icon-file-name
            (concat emacs-wiki-journal-icons-subdirectory "/"
                    category-wiki ".png"))
           (icon-link (if (file-exists-p icon-file-name)
                          (concat (emacs-wiki-journal-make-link
                            icon-file-name) " ")
                        nil)))

      ;; only add a new date if different from the top entry
      (if (not (looking-at (regexp-quote (concat "* " time-string))))
           (insert (concat "* " time-string "\n\n"))
          (forward-line 1))
       ;; add the anchor first so that user is taken above entry
       (insert (concat "#" anchor-base anchor-ord "\n"))

      (open-line 1)
      (insert
       (concat
        "** " journal-entry-heading "\n"
        (if icon-link
            (concat icon-link "\n"))
        "*** " 
	"[[" category-wiki "][" (emacs-wiki-journal-get-title category-wiki)
	"]], [[" self-link "][" emacs-wiki-journal-self-link-name  "]]\n"
        date-string "\n\n\n")))

    (emacs-wiki-journal-find-file emacs-wiki-journal-current-page)
    ;; move to insertion point
    (forward-line -1)))

(defun emacs-wiki-journal-copy-current ()
  "Copy the current page to the emacs-wiki-journal-wiki"
  (save-window-excursion
    (save-excursion
      (emacs-wiki-journal-find-file emacs-wiki-journal-current-page)
      (write-region
       (point-min)
       (point-max)
       (emacs-wiki-journal-expand-file-name 
	emacs-wiki-journal-wiki)))))
    
(defun emacs-wiki-journal-publish-function (file output-path)
  (emacs-wiki-journal-set-current-page)
  (let ((journal-wiki-file (emacs-wiki-journal-expand-file-name 
			    emacs-wiki-journal-wiki))
	(current-file (emacs-wiki-journal-expand-file-name 
		       emacs-wiki-journal-current-page)))
    (if (equal file current-file)
	(progn
	  (emacs-wiki-journal-copy-current)
	  (emacs-wiki-publish-files `(,journal-wiki-file) t)))
    (emacs-wiki-publish-current file output-path)))
       
;;;###autoload
(defun emacs-wiki-journal-add-entry ()
  "Find journal file and add an entry and category index for today."
  (interactive)
  (let ((category (emacs-wiki-journal-prompt-for-category-wiki))
        (heading (read-from-minibuffer "Journal Heading: ")))
    (emacs-wiki-journal-prepare-journal)
    (emacs-wiki-journal-add category heading)))

;;;###autoload
(defun emacs-wiki-journal-add-entry-other-window ()
  "Find category index file in another window and add an entry
for today."
  (interactive)
  (let ((emacs-wiki-journal-use-other-window t))
    (emacs-wiki-journal-add-entry)))

;;;_* Initialization
(defun emacs-wiki-journal-update-wiki-project ()
  "Update the \"emacs-wiki-journal\" project in `emacs-wiki-projects'."
  ;; Remove the entry associated with Journal
  (setq emacs-wiki-projects
        (delq (assoc emacs-wiki-journal-project emacs-wiki-projects)
              emacs-wiki-projects))
  ;; Remove an entry that uses the default Journal project name
  (setq emacs-wiki-projects
        (delq (assoc emacs-wiki-journal-project-default-name
                     emacs-wiki-projects)
              emacs-wiki-projects))
  ;; Assign new contents to Journal entry
  (add-to-list 'emacs-wiki-projects
               `(,emacs-wiki-journal-project
                 . ((emacs-wiki-directories
                     . (,emacs-wiki-journal-directory))
                    (emacs-wiki-home-page
                     . ,emacs-wiki-journal-wiki)
                    (emacs-wiki-publishing-directory
                     . ,emacs-wiki-journal-publishing-directory)
                    (emacs-wiki-project-server-prefix
                     . ,emacs-wiki-journal-server-prefix)
                    (emacs-wiki-index-title-threshold
                     . ,emacs-wiki-journal-index-title-threshold)
		    (emacs-wiki-publish-function 
		     . emacs-wiki-journal-publish-function))))
  (emacs-wiki-update-project-interwikis))

(setq emacs-wiki-journal-loaded t)
(emacs-wiki-journal-update-wiki-project)
(provide 'emacs-wiki-journal)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; emacs-wiki-journal.el ends here
