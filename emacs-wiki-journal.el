;;; emacs-wiki-journal.el --- Maintain a weblog  with emacs-wiki

;; Copyright (C) 2003, 2004 Gary V. Vaughan (gary AT gnu DOT org)
;; Copyright (C) 2004 Ole Arndt (ole AT sugarshark DOT com)
;; Copyright (C) 2004 Hoan Ton-That (hoan AT ton-that DOT org)
;; Copyright (C) 2004 Jose A. Ortega Ruiz (jao AT gnu DOT org)
;; Copyright (C) 2004, 2005 Michael Olson
;; Copyright (C) 2004 Yamagata Yoriyuki
;; Copyright (C) 2004, 2005 Yu Li
;; Copyright (C) 2005, 2006, 2007, 2008 Sucha (suchaaa AT gmail DOT com)

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-journal.el
;; Version: 2.66
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Michael Olson (mwolson AT gnu DOT org)
;; Description: Maintain weblog in a local Emacs Wiki
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
;; maintain a weblog with generating rss feed, calendar and archive
;; links.

;;;_ + Startup


;; For new user
;;
;; Please put this in your .emacs file:
;;
;;   (add-to-list 'load-path "~/path/to/emacs-wiki-journal/")
;;   (require 'emacs-wiki-journal)
;;
;; then M-x customize-group, enter emacs-wiki-journal to setup 
;; your config


;; For original emacs-wiki-journal user
;;
;; You can backup your config file, then delete all your original
;; emacs-wiki-journal variables, M-x customize-group, enter 
;; emacs-wiki-journal to re-setup emacs-wiki-journal.
;;
;; Another choice
;;
;; First ensure to comment these below, just put a ";" beginning 
;; of the line.
;; 
;; ;'(emacs-wiki-journal-date-format "%Y-%m-%dT%T%z")
;; ;'(emacs-wiki-journal-date-tag-template "<!-- date: %s -->")
;;
;; Then add these variables to your .emacs file:
;; 
;; (custom-set-variables
;;  (emacs-wiki-journal-welcome-page-title "Welcome")
;;  (emacs-wiki-journal-self-link-name "Permalink")   ; for each entry
;;  (emacs-wiki-journal-generate-rss-file t)          ; nil for needless
;;  (emacs-wiki-journal-rss-intiial-content           ; adapt for your need 
;;   "<?xml version=\"1.0\" encoding=\"utf-8\"?>
;; <?xml-stylesheet href=\"../styles/rss_style.css\" type=\"text/css\"?>
;; <rss version=\"2.0\"
;;      xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
;;      xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
;;      xmlns:admin=\"http://webns.net/mvcb/\"
;;      >
;;   <channel>
;;     <title>Sucha's Blog</title>
;;     <link>http://suchang.net/blog/index.html</link>
;;     <description>linux, emacs, programming, live and essay</description>
;;     <pubDate>%s</pubDate>
;;     <generator>emacs-wiki-journal</generator>
;;     <language>zh-CN</language>
;;     <dc:creator>suchaaa@gmail.com (sucha)</dc:creator> \n\n" )
;;  (emacs-wiki-journal-rss-file-name "~/path/to/your/publish/rss.xml")
;;  (emacs-wiki-journal-rss-link-prefix "http://scuhang.net/blog/") ;your blog addr
;;  (emacs-wiki-journal-maintainer "sucha"))
;;
;; 
;; If you need publish calendar, archive links, please add these to
;; your original publishing header or footer.
;;
;;   (emacs-wiki-publishing-header . 
;;     "    <-- THE ORIGINAL TEXT -->
;;
;;          <lisp>(emacs-wiki-journal-insert-calendar)</lisp>
;;
;;          <lisp>(emacs-wiki-journal-generate-archive-links)</lisp>
;;
;;      <!-- Page published by Emacs Wiki begins here --> ")
;; 
;; You can also make changes for your need.


;; Other document
;;
;; And I also keep a document in http://scuhang.net/cs/EmacsWikiJournal.html,
;; all written in Chinese, and you can also find a sample config file in
;; http://suchang.net/code/emacs-wiki-conf.el.html. I use emacs-wiki projects
;; for emacs-wiki-journal there.


;; Usage 
;;
;; Now, you can add an entry to your journal with M-x
;; emacs-wiki-journal-add-entry, give it a WikiName for a category to
;; index the entry under (usually beginning "Category") and a header
;; line for the new entry.  A suitable header for today's entry will
;; added to emacs-wiki-journal-wiki where you add the body for the
;; entry and a cross reference will be added to the category index.
;; If you have a PNG icon in the emacs-wiki-category-icons-url then
;; the icon named after the CategoryWiki will be inserted in the
;; journal header. If you enable generate rss file and set the previous
;; part in your emacs-wiki-publish-header or footer, you can also get 
;; an rss feed for your latest month, a calendar for the related month,
;; and archive links for all your blogs files. Enjoy for this, :)

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
;;
;; I also strongly suggest you to use this auto-publish code from sacha
;; instead of C-c C-p, it will run after you make a save:
;;
;; (defun sacha-emacs-wiki-auto-publish()
;;   (when (derived-mode-p 'emacs-wiki-mode)
;;     (unless emacs-wiki-publishing-p
;;      (let ((emacs-wiki-publishing-p t)
;;	    (emacs-wiki-after-wiki-publish-hook nil))
;;	(emacs-wiki-journal-publish-this-page)
;; ;    (emacs-wiki-publish-index) ; if you need auto-publish-index, remove the leading ';'
;;      ))))
;;
;; (add-hook 'emacs-wiki-mode-hook
;;	  (lambda () (add-hook 'after-save-hook
;;			       'sacha-emacs-wiki-auto-publish
;;			       nil t)))
;;
;; emacs-wiki-journal have set C-c C-p to emacs-wiki-journal-publish-this-page
;; by defualt, for generating rss, calendar and archive links. So, if you want
;; to publish WikiIndex, you should have to M-x emacs-wiki-journal-publish-index
;; for emacs-wiki-journal's  WikiIndex.
;;
;; It also suit for emacs-wiki other project.

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
;;
;; Sucha (suchaaa AT gmail DOT com) make a big hacking, and use emacs-
;; wiki-journal as weblog, it can genrate rss feed, calendar and archive 
;; links.



;;;_* Prerequisites
(require 'cl)
(require 'emacs-wiki)
(require 'calendar)
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
  "The directory where all journal entries are published to. Please don't
   insert the last '/'."
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
  "A static page where archive files are added to, see
`emacs-wiki-journal-wiki-max-entries'."
  :type 'string
  :set 'emacs-wiki-journal-option-customized
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-wiki-max-entries 11
  "Max entries in the statice default page."
  :type 'number
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

(defcustom emacs-wiki-journal-time-format nil
  "Format for the date string of journal entries.
   See `format-time-string' for more information. 
   If `nil' means use self definited function showed below."
  :type '(choice (string :tag "Time string format: ")
		 (const :tag "nil" nil))
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-user-time-format
  'emacs-wiki-journal-user-definite-time-format
  "The user's function to format time string. 
   If emacs-wiki-journal-time-format set `nil', 
   this function will be active."
  :type 'function
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-category-regexp
  "^Category\\([A-Z][a-z]+\\)+$"
  "Each of the category index Wiki files start with this prefix."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-time-format-category nil
  "Format for the date/time string of category page entries.
   If `nil', `emacs-wiki-journal-time-format' will be used.
   See `format-time-string' for more information."
  :type '(choice (string :tag "Time string format: ")
                 (const :tag "nil" nil))
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-user-time-format-category
  'emacs-wiki-journal-user-definite-format-category
  "The user's method to format category time string. If the
   emacs-wiki-journal-time-format-category set `nil', this 
   function will be acting."
  :type 'function
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


(defcustom emacs-wiki-journal-welcome-page-title "Welcome"
  "The \"static\" page -- welcome page title."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-self-link-name "Permalink"
  "The self-link name for the related article."
  :type 'string
  :group 'emacs-wiki-journal)


;;;_* Internal Variables

(defvar emacs-wiki-journal-current-page nil
  "The current journal page")

;; use the fuul ISO 8601 format, a important sign to 
;; search or seek in files. also create rss and calendar.

(defvar emacs-wiki-journal-date-format "%Y-%m-%dT%T%z")
(defvar emacs-wiki-journal-date-tag-template "<!-- date: %s -->")
(defvar emacs-wiki-journal-calendar-content nil)


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
  ;;  (let ((file-alist
  ;;         (cadr (assoc emacs-wiki-journal-project
  ;;		      emacs-wiki-file-alist)))
  ;;        (category-alist nil))
  ;;    (save-match-data
  ;;      (mapc
  ;;       (lambda (file)
  ;;         (if (string-match emacs-wiki-journal-category-regexp
  ;;                           (car file))
  ;;             (setq category-alist (cons file category-alist))))
  ;;       file-alist))
  ;;    category-alist))
  ;; If there are possible category index Wikis in the `emacs-wiki-journal-directory',
  ;; read it
  (directory-files emacs-wiki-journal-directory 
		   nil
		   emacs-wiki-journal-category-regexp
		   'true))

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

  ;; If current month's file didn't exist, first to create it
  (if (not (file-exists-p (emacs-wiki-journal-expand-file-name
			   emacs-wiki-journal-current-page)))
      (write-file (emacs-wiki-journal-expand-file-name
		   emacs-wiki-journal-current-page)))
  
  ;; Skip to first heading
  (when (re-search-forward "^\\* " nil t)
    (beginning-of-line)
    (forward-line 1))
  (let ((insert-date-entry
         (format emacs-wiki-journal-date-tag-template
                 (format-time-string
                  emacs-wiki-journal-date-format)))
        (date-string
	 (if emacs-wiki-journal-time-format-category
	     (format-time-string 
	      emacs-wiki-journal-time-format-category)
	   (emacs-wiki-journal-user-definite-format-category)))
	(after-save-hook nil)
        (month-title-format "%Y.%m")
        latest-date
        last-month-title)

    ;; Find last month title, format as '2008.04'
    (save-excursion
      (setq last-month-title
            (and (re-search-forward "^** \\(.+\\)" nil t)
                 (match-string 1))))

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

    ;; P.S: latest-date format is %Y-%m-%dT%T%z
    (let 
         ;; get the last entrie month in category
        ((last-entry-month
          (if latest-date (format "%s.%s"
                                  (substring latest-date 0 4)
                                  (substring latest-date 5 7)))))

      ;; if last-month-title or emacs-wiki-journal-category-last-entry-month
      ;; is nil, as new buffer, do not insert first
      (when (or last-month-title last-entry-month)

        ;; if last-month-title isn't the last-category-month, insert it
        ;; for update old category styles to new one
        (if last-entry-month
            (unless (or (string= last-month-title last-entry-month)
                        (string= last-entry-month (format-time-string month-title-format)))
              (insert (concat "\n** " last-entry-month "\n\n"))))

        ;; if month tag in the beginning, skip it
        (goto-char (point-min))
        (when (and (string= last-month-title last-entry-month)
                   (string= last-month-title (format-time-string month-title-format)))
          (re-search-forward "^** [0-9][0-9][0-9][0-9].[0-9][0-9]" nil t)
          (forward-line 2))
        )

      ;; Insert new entry include date tag
      (insert (concat " - " date-string " "
                      (emacs-wiki-journal-make-link target-url
                                                    link-description)
                      "\n"
                      insert-date-entry "\n"))

      ;; update to new category style
      ;; insert current month tag if there's none
      (unless (string= last-month-title (format-time-string month-title-format))
        (goto-char (point-min))
        (insert (concat "\n** " (format-time-string month-title-format) "\n\n")))
      )
    
    ;; Save-buffer then return to the current-month buffer
    (let ((after-save-hook nil))
      (save-buffer)			; no auto publish
      (emacs-wiki-journal-publish-this-page)
      (kill-buffer (buffer-name)))
    ))

;; use to calculate the anchor
(defun emacs-wiki-journal-1+-string (value)
  "Increment an ascii encoded number."
  (int-to-string (1+ (string-to-number value))))

(defun emacs-wiki-journal-set-current-page ()
  "Return the current month page."
  (unless (and emacs-wiki-journal-current-page
	       (or
		(get-buffer emacs-wiki-journal-current-page)
		(file-exists-p (emacs-wiki-journal-expand-file-name
				emacs-wiki-journal-current-page))))
    (setq emacs-wiki-journal-current-page (format-time-string "%Y-%m"))))

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

;; the latest page also the latest blog month.
(defun emacs-wiki-journal-last-page ()
  "Return the latest page file-name, a string."
  (car (directory-files emacs-wiki-journal-directory
			nil
			"^[0-9][0-9][0-9][0-9]-[0-9][0-9]$"
			'true)))

(defun emacs-wiki-journal-publish-index ()
  "Replacement for `emacs-wiki-publish-index'."
  (interactive)
  (emacs-wiki-journal-generate-calendar) ; first to generate calendar
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
    
    ;; set the current month page
    (emacs-wiki-journal-set-current-page)

    (emacs-wiki-journal-find-file
     emacs-wiki-journal-current-page
     (when emacs-wiki-journal-use-other-window
       'find-file-other-window))

    (goto-char (point-min))

    (unless (re-search-forward "^#title " nil t)
      (insert "#title Archive for "
	      (format "%s, %s"
		      (calendar-month-name
		       (string-to-number (format-time-string "%m")))
		      (format-time-string "%Y"))
	      "\n\n")
      (forward-line))

    (save-excursion
      (if (re-search-forward anchor-regexp nil t)
	  (setq anchor-ord
		(emacs-wiki-journal-1+-string
		 (buffer-substring (match-beginning 1)
				   (match-end 1))))))
    (setq self-link
	  (concat emacs-wiki-journal-current-page
		  "#" anchor-base anchor-ord))

    ;; skip # lines and blanks at the start of the buffer
    (while (and (looking-at "^\\(#\\|\n\\)")
		(equal 0 (forward-line 1))))

    ;; skip to first heading
					;    (re-search-forward "^\\* " nil t)
    (re-search-forward "^#p\\([0-9][0-9]*\\)" nil t)
    (beginning-of-line)

    (let* ((time-string 
	    (if emacs-wiki-journal-time-format
		(format-time-string emacs-wiki-journal-time-format)
	      (emacs-wiki-journal-user-definite-time-format)))
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

      ;; add the anchor first so that user is taken above entry
      (insert (concat "#" anchor-base anchor-ord "\n"))
      ;; always insert a new date
      (insert (concat "* " time-string "\n\n\n"))
      (forward-line -1)

      (open-line 1)
      (insert
       (concat
	"** " journal-entry-heading "\n\n\n\n"
	(if icon-link
	    (concat icon-link "\n"))
	"*** " 
	"[[" category-wiki "][" (emacs-wiki-journal-get-title category-wiki)
	"]] / "
	"[[" self-link "][" emacs-wiki-journal-self-link-name  "]] \n\n"
	date-string "\n\n")))

    (emacs-wiki-journal-find-file emacs-wiki-journal-current-page)

    ;; save the above prepare for publishing the category file
    (let ((after-save-hook nil))
      (save-buffer)			; no auto publish
      (forward-line -6))		; move to insertion point

    ;; prepared all the above, we generate the category entries
    (save-window-excursion
      (save-excursion
	(emacs-wiki-journal-add-category-entry
	 category-wiki
	 self-link
	 journal-entry-heading)))
    ))

(defun emacs-wiki-journal-publish-function (file output-path)
  (emacs-wiki-journal-set-current-page)
  (let ((journal-wiki-file (emacs-wiki-journal-expand-file-name 
			    emacs-wiki-journal-wiki))
	(current-file (emacs-wiki-journal-expand-file-name 
		       emacs-wiki-journal-current-page)))
    (if (equal file current-file)
	(progn
	  (emacs-wiki-journal-prepare-welcome-page)
	  (emacs-wiki-publish-files `(, journal-wiki-file) t)
	  ))
    (emacs-wiki-publish-current file output-path)))
       
;;;###autoload
(defun emacs-wiki-journal-add-entry ()
  "Find journal file and add an entry and category index for today."
  (interactive)
  (let ((category (emacs-wiki-journal-prompt-for-category-wiki))
	(heading (read-from-minibuffer "Journal Heading: ")))
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

;; The main addtion code by sucha
;; rss generator section

(defcustom  emacs-wiki-journal-generate-rss-file t
  "Whether to generate the rss file for the latest month in
   auto-publish code."
  :type 'boolean
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-rss-initial-content
  "<?xml version=\"1.0\" encoding=\"utf-8\"?>
<?xml-stylesheet href=\"../styles/rss_style.css\" type=\"text/css\"?>
<rss version=\"2.0\"
     xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"
     xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
     xmlns:admin=\"http://webns.net/mvcb/\"
     >
  <channel>
    <title>Sucha's Blog</title>
    <link>http://suchang.net/blog/index.html</link>
    <description>linux, emacs, programming, live and essay</description>
    <pubDate>%s</pubDate>
    <generator>emacs-wiki-journal</generator>
    <language>zh-CN</language>
    <dc:creator>suchaaa@gmail.com (sucha)</dc:creator>\n\n"

  "Rss initial file content."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-rss-file-name
  "~/workport/homesite/publish/blog/rss.xml"
  "The RSS feed file name, include the path."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-rss-link-prefix
  "http://suchang.net/blog/"
  "Your blog site address. NEED last '/'."
  :type 'string
  :group 'emacs-wiki-journal)

(defcustom emacs-wiki-journal-maintainer "sucha"
  "Your post name, or we call the ID, :)"
  :type 'string
  :group 'emacs-wiki-journal)


(defun emacs-wiki-journal-generate-rss ()
  "Generate the rss feed from the journal latest month every time
   you modified it."
  (interactive)
  (when (equal (buffer-name) (emacs-wiki-journal-last-page))
    (save-window-excursion
      (save-excursion
	(find-file emacs-wiki-journal-rss-file-name)
	(erase-buffer)
	(insert (format emacs-wiki-journal-rss-initial-content
			(format-time-string "%Y-%m-%dT%T%z")))

	(emacs-wiki-journal-find-file (emacs-wiki-journal-last-page))
	(goto-char (point-min))
	(emacs-wiki-journal-rss-collect-items)

	(find-file emacs-wiki-journal-rss-file-name)
	(insert "  </channel>\n</rss>")
	(save-buffer 0)
	(kill-buffer (buffer-name))
	(message "Generate rss file done.")))))

(defun emacs-wiki-journal-rss-collect-items ()
  "Collect items' information from journal's latest month."
  (emacs-wiki-journal-find-file (emacs-wiki-journal-last-page))
  (when (setq insert-page-anchor
	      (and (re-search-forward "^#p\\([0-9][0-9]*\\)" nil t)
		   (match-string 1)))
    (setq insert-rss-title
	  (and (setq last-title-point
		     (re-search-forward "^\\*\\* \\(.+\\)" nil t))
	       (match-string 1)))
    (setq insert-rss-category
	  (and (re-search-forward
		"^*** \\[\\[\\([A-Z].+\\)\\]\\[[A-Z].+\\]\\] \\/" nil t)
	       (match-string 1)))
    (setq insert-rss-date
	  (and (setq last-date-point
		     (re-search-forward "<!-- date: \\(.+\\) -->" nil t))
	       (match-string 1)))

    ;; collect discriptions
    (save-excursion
      (copy-region-as-kill
       (and (goto-char last-title-point)
	    (forward-line 1) 
	    (point))
       (and (goto-char last-date-point)
	    (forward-line -2)
	    (point))))

    ;; Construct a markup disctriptions for rss.
    ;; if you do not need the HTMLed description(actually, 
    ;; W3C claimed do not HTMLed the description in rss),
    ;; comment the (emacs-wiki-replace-markup)
    (with-temp-buffer
      (setq insert-rss-description
	    (let ((emacs-wiki-publishing-header "")
		  (emacs-wiki-publishing-footer ""))
	      (yank)
	      (emacs-wiki-replace-markup)
	      (buffer-string))))

    (emacs-wiki-journal-rss-insert-items)))

(defun emacs-wiki-journal-rss-insert-items ()
  "Insert the rss items in the rss file."
  (find-file emacs-wiki-journal-rss-file-name)
  (goto-char (point-max))
  (insert  "    <item>\n"
	   "      <title>"insert-rss-title"</title>\n"
	   "      <guid>"emacs-wiki-journal-rss-link-prefix
	   (emacs-wiki-journal-last-page)".html#p"
	   insert-page-anchor"</guid>\n"
	   "      <pubDate>"insert-rss-date"</pubDate>\n"
	   "      <dc:creator>"emacs-wiki-journal-maintainer
	   "</dc:creator>\n"
	   "      <category>"insert-rss-category"</category>\n"
	   "      <description><![CDATA["
	   insert-rss-description"]]></description>\n"
	   "    </item>\n\n")
  (emacs-wiki-journal-rss-collect-items))


;; generate archives links
(defun emacs-wiki-journal-generate-archive-links ()
  "Get the directory archives then insert the archive links."
  (insert "<div class=\"archive_links\"><ul>")
  (ignore
   (mapcar
    (lambda (file-name)
      (insert
       (format "<li><a href=\"%s.html\">%s %s</a></li>"
	       file-name
	       (calendar-month-name
		(string-to-number (substring file-name 5 7)))
	       (substring file-name 0 4))))
    (directory-files emacs-wiki-journal-directory
		     nil
		     "^[0-9][0-9][0-9][0-9]-[0-9][0-9]$" 'true)))
  (insert "</ul></div>"))


;; calendar generator section
(defun emacs-wiki-journal-generate-calendar ()
  "Generate the calendar base on the file name . If the
   current file is category file, create the latest month's calendar."
  (interactive)
  (setq emacs-wiki-journal-calendar-content nil)
  (save-excursion
    (ignore
     (mapc (lambda (file-name)
	     (when (equal (buffer-name) file-name)
	       (setq last-point 0)
	       (setq emacs-wiki-journal-calendar-item-list nil)
	       (emacs-wiki-journal-calendar-collect-items file-name)
	       (emacs-wiki-journal-calendar-construct-insert file-name)))
	   (directory-files emacs-wiki-journal-directory
			    nil
			    "^[0-9][0-9][0-9][0-9]-[0-9][0-9]$"
			    'true)))))

(defun emacs-wiki-journal-calendar-collect-items (file)
  "Collect files items for creating date link."
  (goto-char last-point)
  (when (setq cur-anchor
	      (and (re-search-forward "^#p\\([0-9][0-9]*\\)" nil t)
		   (match-string 1)))
    (setq cur-title
	  (and (re-search-forward "^\\*\\* \\(.+\\)" nil t)
	       (match-string 1)))
    (setq cur-day (string-to-number
		   (substring (and (setq last-point 
					 (re-search-forward
					  "<!-- date: \\(.+\\) -->" nil t))
				   (match-string 1)) 8 10)))

    (setq emacs-wiki-journal-calendar-item-list
	  (cons (list cur-anchor cur-title cur-day)
		emacs-wiki-journal-calendar-item-list))
    (emacs-wiki-journal-calendar-collect-items file)))

(defun emacs-wiki-journal-calendar-construct-insert (file-name)
  "Construct the insert content for each entry's day."
  (let*
      ((cur-month (string-to-number (substring file-name 5 7)))
       (cur-year (string-to-number (substring file-name 0 4)))
       (start-day (calendar-day-of-week (list cur-month 1 cur-year)))
       (last-day-of-month (calendar-last-day-of-month
			   cur-month cur-year))
       (items-form (nth 0 emacs-wiki-journal-calendar-item-list))
       (snum 1))

    (setq emacs-wiki-journal-calendar-content
	  (with-output-to-string
	    (princ "<div class=\"calendar\"><table><caption>")
	    (princ (calendar-month-name cur-month))
	    (princ " ")
	    (princ cur-year)
	    (princ "</caption><thead><tr><th>Su</th><th>Mo</th><th>Tu</th><th>")
	    (princ "We</th><th>Th</th><th>Fr</th><th>Sa</th></tr></thead><tr>")

	    (dotimes (day start-day)
	      (if (< day start-day)
		  (princ "<td></td>")))
    
	    (dotimes (day last-day-of-month)
	      (if (= (mod (+ day start-day) 7) 0)
		  (princ "</tr><tr>"))

	      (cond
	       ((not (equal (1+ day) (nth 2 items-form)))
		(princ "<td>")
		(princ (1+ day))
		(princ "</td>" ))

	       ((equal (nth 2 items-form)
		       (nth 2 (nth snum emacs-wiki-journal-calendar-item-list)))
		(setq day (1- day))
		(setq items-form (nth snum emacs-wiki-journal-calendar-item-list))
		(setq snum (1+ snum)))

	       (t 
		(princ "<td><a title=\"")
		(princ (nth 1 items-form))
		(princ "\" href=\"")
		(princ file-name)
		(princ ".html#p")
		(princ (nth 0 items-form))
		(princ "\">")
		(princ (1+ day))
		(princ "</a></td>")
		(setq items-form
		      (nth snum emacs-wiki-journal-calendar-item-list))
		(setq snum (1+ snum)))))

	    (princ "</tr></table></div>")))))

(defun emacs-wiki-journal-insert-calendar ()
  "Insert the calendar content."
  (if emacs-wiki-journal-calendar-content
      (insert emacs-wiki-journal-calendar-content)
    (let (emacs-wiki-journal-buffer-appearance)
      (save-window-excursion
	(save-excursion
	  (setq emacs-wiki-journal-buffer-appearance
		(get-buffer (emacs-wiki-journal-last-page)))
                (emacs-wiki-journal-find-file (emacs-wiki-journal-last-page))
                (save-excursion
                  (setq last-point 0)
                  (setq emacs-wiki-journal-calendar-item-list nil)
                  (emacs-wiki-journal-calendar-collect-items
                   (emacs-wiki-journal-last-page))
                  (emacs-wiki-journal-calendar-construct-insert
                   (emacs-wiki-journal-last-page)))
                (if (not emacs-wiki-journal-buffer-appearance)
                    (kill-buffer (buffer-name)))))
      (insert emacs-wiki-journal-calendar-content))))

;; time-format for entries' title and categorys' title
(defun emacs-wiki-journal-user-definite-time-format ()
  "If emacs-wiki-journal-time-format set to nil, it will 
  display your own time string format in entries."
  (format "%s %s, %s"
	  (calendar-month-name
	   (string-to-number (format-time-string "%m")))
	  (format-time-string "%-e")
	  (format-time-string "%Y")))

(defun emacs-wiki-journal-user-definite-format-category ()
  "If emacs-wiki-journal-time-format-category set to nil, it 
   will display your own time string format in category."
  (format "%s %s, %s"
	  (calendar-month-name
	   (string-to-number (format-time-string "%m")))
	  (format-time-string "%-e")
	  (format-time-string "%Y")))

;; for welcome page
(defun emacs-wiki-journal-prepare-welcome-page ()
  "Copy max-entries to default page from archive files."
  (interactive)
  ;; clean then insert title
  (save-window-excursion
    (save-excursion
      (let ((after-save-hook nil))
	(emacs-wiki-journal-find-file  emacs-wiki-journal-wiki)
	(erase-buffer)
	(insert (format "#title %s\n"  emacs-wiki-journal-welcome-page-title))
	(save-buffer)
	(kill-buffer (buffer-name)))))
  ;; copy entries from archive files
  (let ((files (directory-files emacs-wiki-journal-directory
				nil
				"^[0-9][0-9][0-9][0-9]-[0-9][0-9]$"
				'true))
	(entries emacs-wiki-journal-wiki-max-entries)
	(current-file buffer-file-name))

    (save-window-excursion ;; circle
      (save-excursion
	(while (and (> entries 0)
		    (not (equal files nil))
		    (setq file (car files)))

	  ;; when '(cdr files)' to be nil, the left can be publish
	  (setq files (cdr files))
	  (emacs-wiki-journal-find-file file)
	  (goto-char (point-min))

	  (while (and (> entries 0)
		      (re-search-forward "^#p\\([0-9][0-9]*\\)" nil t))
	    (write-region
	     (and (forward-line 1)	; do not need the '#p0'
		  (point))
	     (and (re-search-forward "<!-- date: \\(.+\\) -->" nil t)
		  (forward-line 2)
		  (point))
	     (emacs-wiki-journal-expand-file-name emacs-wiki-journal-wiki)
	     't)
	    (setq entries (1- entries)))
	  ;; in case of current-buffer is the file we edit
	  (if (not (equal current-file buffer-file-name))
	      (kill-buffer (buffer-name))))))))

(defun emacs-wiki-journal-publish-this-page ()
  "Force publication of the current page, also consider of the 
   emacs-wiki-journal-wiki page."
  (interactive)
  (emacs-wiki-journal-generate-calendar)
  (when (equal buffer-file-name (emacs-wiki-journal-expand-file-name
                                 (emacs-wiki-journal-last-page)))
    (if emacs-wiki-journal-generate-rss-file
        (emacs-wiki-journal-generate-rss))
    (emacs-wiki-journal-prepare-welcome-page)
    (emacs-wiki-publish-files (list (emacs-wiki-journal-expand-file-name
                                     emacs-wiki-journal-wiki)) t))
  (emacs-wiki-publish-files (list buffer-file-name) t))

(defun emacs-wiki-journal-publish ()
  "Replacement for `emacs-wiki-publish', also consider of the
emacs-wiki-journal-wiki page."
  (interactive)
  (emacs-wiki-journal-generate-calendar)
  (when (equal buffer-file-name (emacs-wiki-journal-expand-file-name
				 (emacs-wiki-journal-last-page)))
    (if emacs-wiki-journal-generate-rss-file
	(emacs-wiki-journal-generate-rss))
    (emacs-wiki-journal-prepare-welcome-page)
    (emacs-wiki-publish-files (list (emacs-wiki-journal-expand-file-name
				     emacs-wiki-journal-wiki)) t))
  (emacs-wiki-publish-files (list buffer-file-name) t))

(define-key emacs-wiki-mode-map		; re-bind the publish key
  [(control c) (control p)] 'emacs-wiki-journal-publish)

;; end of sucha's section

(setq emacs-wiki-journal-loaded t)
(emacs-wiki-journal-update-wiki-project)
(provide 'emacs-wiki-journal)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; emacs-wiki-journal.el ends here