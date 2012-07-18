;; emacs-wiki-blog.el - Emacs-wiki addon for maintaining a we(blog)
;; $Id: emacs-wiki-blog.el 39 2005-03-25 20:13:00Z ganesh $

;; Copyright (C) 2003, 2004, 2005 Ganesh Swami

;; Author: Ganesh Swami <ganesh@iamganesh.com>
;; Version: $Revision :$
;; Created: December 10 2003
;; 

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; this software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.


;;; Commentary:

;; How to use: 

;; 1. Where ever you want the frontpage news to appear, put in this gem:

;; <lisp>
;; (emacs-wiki-blog-front-page date num text)
;; </lisp>

;; where date is a date in the month that you want generated. Can very
;; well be this month's.


;;; Code



(require 'emacs-wiki)
(require 'calendar)



;; Customization
;(setq ewb-publishing-footer "")
;(setq ewb-publishing-header "")



;; Regular expression to match the weblog files
(defconst *weblog-file-regexp* "^[0-9][0-9][0-9][0-9]\.[0-9][0-9]\.[0-9][0-9]$")


(defun weblog-mdy-lessp (mdy1 mdy2)
  (let ((m1 (first mdy1))
        (d1 (second mdy1))
        (y1 (third mdy1))
        (m2 (first mdy2))
        (d2 (second mdy2))
        (y2 (third mdy2)))
    (or (< y1 y2)
        (and (eq y1 y2)
             (or (< m1 m2)
                 (and (eq m1 m2)
                      (< d1 d2)))))))



(defun weblog-month-file-name (month year &optional file-type)
  ;;  (concat (weblog-format-2d (% year 100))
  (concat (format "%d" year) 
          (weblog-format-2d month)
          (or file-type ".txt")))

(defun weblog-day-file (month day year)
  "Return the name of the file for the given day"
  (concat (format "%d" year ) "." 
          (weblog-format-2d month) "." 
          (weblog-format-2d day)
          ))

(defun weblog-day-file-exists (month day year)
  "Return true if the html file exists for a particular day"
  (let* ((file (weblog-day-file month day year))
         (other-file (concat (file-name-sans-extension file) ".txt")))
    (and (or (file-attributes (weblog-file file))
             (file-attributes (weblog-file other-file)))
         file)))

(defun weblog-mdy-in-future-p (month day year)
  (= 1 2)
)
;; (defun weblog-mdy-in-future-p (month day year)
;;   "Say whether a (month day year) list is in the future"
;;   (let* ((time (decode-time (current-time)))
;;          (cur-day (nth 3 time))
;;          (cur-month (nth 4 time))
;;          (cur-year (nth 5 time)))
;;     (weblog-mdy-lessp
;;      (list cur-month cur-day cur-year) (list month day year))))


(defun weblog-file-mdy (file-name)
  "Return (list month day year) for a filename of \"yyyy.mm.dd\""
  (ignore-errors
    (let* ((name 
            (file-name-nondirectory file-name))
           (yy (car (read-from-string (substring name 0 4))))
           (mm (car (read-from-string (substring name 5 7))))
           (dd (car (read-from-string (substring name 8 10)))))
      (when (and (eq 10 (length name))
                 (integerp yy)
                 (integerp mm)
                 (integerp dd))
        ;;notneed, coded already.
        ;;        (setq yy (if (< yy 70) (+ 2000 yy) (+ 1900 yy)))
        (list mm dd yy)))))


(defun weblog-format-2d (n)
  "Format an integer as two characters with a leading zero"
  (let* ((s (format "%d" n)))
    (if (eq 1 (length s)) (setq s (concat "0" s)))
    s))


(defun weblog-month-file-name (month year &optional file-type)
  ;;  (concat (weblog-format-2d (% year 100))
  (concat (format "%d" year) 
          (weblog-format-2d month)
          (or file-type ".txt")))


(defun emacs-wiki-real-page-title (file)
 (with-temp-buffer  
  (insert-file-contents file)
  (save-excursion
    (goto-char 0)
   (if (re-search-forward "#title \\(.*\\)" nil t) 
    (match-string 1) (emacs-wiki-page-title file)))))

(defcustom ewb-rss-initial-contents
  "<?xml version=\"1.0\"?>
<rss version=\"2.0\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\"
xmlns:content=\"http://purl.org/rss/1.0/modules/content/\"
>

<channel>
        <title>Ganesh Swami</title>
        <link>http://www.sfu.ca/~gswamina/</link>
        <description>Stuff for me, stuff that matters</description>
        <language>en-us</language>
        <copyright>Copyright 2002-2004, Ganesh Swami</copyright>
        <generator>Emacs Wiki Blog (ewb) 0.5</generator>
        <docs>http://blogs.law.harvard.edu/tech/rss</docs>
        <ttl>600</ttl>
        <image>
                <url>http://www.sfu.ca/~gswamina/images/g.png</url>
                <title>Ganesh Swami</title>
                <link>http://www.sfu.ca/~gswamina/</link>
        </image>
        <skipHours>
                <hour>14</hour>
                <hour>15</hour>
                <hour>16</hour>
                <hour>17</hour>
                <hour>18</hour>
                <hour>19</hour>
                <hour>20</hour>
                <hour>21</hour>
        </skipHours>
"
  "Initial contents for RSS file."
  :type 'string
  :group 'planner)

(defcustom ewb-rss-base-url
  "http://www.sfu.ca/~gswamina/"
  "Base URL for blog entries. Should include trailing /."
  :type 'string
  :group 'planner)

(defcustom ewb-rss-file-name
  "/tmp/blog.rdf"
  "Name for the main planner-rss file."
  :type 'string
  :group 'planner)

(defcustom ewb-rss-category-feeds
  nil
  "List of (REGEXP FILENAME INITIAL-CONTENTS).
Blog entries that match this alist are duplicated in the given blogs.
If INITIAL-CONTENTS is non-nil, it is used to initialize the file if
the file is not found or is corrupted."
  :type '(repeat (group regexp file string)))


;; The Code

(defun ewb-publish-rss-entry-template (bobj)
  "Construct the feed block for this entry"

  ;; do the items
  ;; <item>
  ;; 	<title></title>
  ;; 	<link></link>
  ;; 	<description></description>
  ;; 	<pubDate></pubDate>
  ;;    <guid isPermaLink=\"true\"></guid>
  ;;    <content:encoded> </content:encoded>
  ;; </item>

  (let*
      ((rawdate (plist-get bobj 'rawdate)) ;mdy
       (date (format "%s, %s %s %s %s:%s:%s GMT" 
		     (substring (calendar-day-name  rawdate) 0 3)
		     (weblog-format-2d (nth 0 rawdate))
		     (weblog-format-2d (nth 1 rawdate))
		     (nth 2 rawdate)
		     (random 12) (random 60) (random 60)))
	(txt (plist-get bobj 'text)))
    (concat 
     "<item>"
     "    <title>" (emacs-wiki-escape-html-string (plist-get bobj 'title)) "</title>"
     "    <link>"  (plist-get bobj 'published-file)  "</link>"
     "    <pubDate>" date "</pubDate>"
     "    <guid isPermaLink=\"true\">" (plist-get bobj 'published-file) "</guid>"
     "    <dc:subject>" (plist-get bobj 'category)  "</dc:subject>"
     "    <description><![CDATA[" 
     (if (> (length txt) 150)
	 (concat (substring txt 0 150) "....")
       (emacs-wiki-blog-wash txt))
     "   ]]> </description>"

     "
    <content:encoded><![CDATA[" (private-ewb-rss-process-text (plist-get bobj 'text)) "]]></content:encoded>"
    "</item>\n")
    ))

(defun emacs-wiki-blog-wash (x)
  "Wash the HTML text to plain text"

  (with-temp-buffer
    (let ((emacs-wiki-publishing-header "")
	  (emacs-wiki-publishing-footer ""))
      (insert x)
      (cd (file-name-directory "~/Wiki/WelcomePage"))
      (emacs-wiki-maybe)
      (emacs-wiki-replace-markup)
      (html2text))
    (buffer-string)))

;; Entry point
(defun ewb-publish-rss (&optional file numofentries)
  "The toplevel function to process RSS feeds"
  (if (null numofentries) (setq numofentries 10))
  (if (null file) (setq file emacs-wiki-home-page))

  (save-excursion
    (save-window-excursion
      (find-file ewb-rss-file-name)
      (erase-buffer)

      (insert ewb-rss-initial-contents)

      (setq files (sort  (directory-files (file-name-directory (emacs-wiki-page-file file)) nil *weblog-file-regexp*)  
			 '(lambda (a b) (weblog-mdy-lessp (weblog-file-mdy b) (weblog-file-mdy a))
			    )))
      (setq cnt 0)

      (mapcar '(lambda (x)
		 (if (< cnt numofentries)
		     (progn
		       (setq cnt (1+ cnt))
		       (insert (ewb-publish-rss-entry-template (emacs-wiki-blog-section (emacs-wiki-page-file x))))
		       )))		       
	      files)

      (insert "
        </channel>
</rss>\n\n")
      (save-buffer)
      )))

(defun private-ewb-rss-process-text (x)
  (with-temp-buffer
          (let ((emacs-wiki-publishing-header "")
                (emacs-wiki-publishing-footer ""))
	    (insert x)
	    (cd (file-name-directory "~/Wiki/WelcomePage"))
	    (emacs-wiki-maybe)
	    (emacs-wiki-replace-markup))
    (buffer-string)))




(defun emacs-wiki-blog-generate-calendar ()
  "Generate a graphic calendar for the current month with links
  to active blog entries"

  (let* ((time (decode-time (current-time)))
	 (cur-day (nth 3 time))
	 (cur-month (nth 4 time))
	 (cur-year (nth 5 time))
	 (day 0)
	 (start-day (calendar-day-of-week (list cur-month 1 cur-year)))
	 (last-day-of-month (calendar-last-day-of-month cur-month cur-year)))

    (with-output-to-string

      (princ "<div id=\"wp-calendar\">")
      (princ "<table align=\"center\">\n")
      (princ "<caption>\n")
      (princ 

       (concat 
;	(format "<a href=\"archive_%s_%s.html\">%s</a> | " cur-year (weblog-format-2d (- cur-month 2))
;		(substring (calendar-month-name (- cur-month 2)) 0 3))
;	(format "<a href=\"archive_%s_%s.html\">%s</a> | " cur-year (weblog-format-2d (- cur-month 1))
;		(substring (calendar-month-name (- cur-month 1)) 0 3))
	(format "<a href=\"archive_%s_%s.html\">%s</a>" cur-year (weblog-format-2d cur-month)
		(substring (calendar-month-name cur-month) 0 3) ))

       )    
      (princ "</caption>\n<thead><tr>\n  ")
      (dotimes (i 7)
	(let ((column-day (mod (+ i start-day) 7)))
	  (princ "<th>" )
	  (princ (substring (calendar-day-name column-day 3 t) 0 2))
	  (princ "</th>")))

      (princ "</tr></thead><tr>")


      (dotimes (i last-day-of-month)
	(when (>= day 7)
	  (princ "</tr><tr>")
	  (setq day 0))
	       
	(princ "<td>")
	(let* ((sfile (concat "2005." (weblog-format-2d cur-month) "." (weblog-format-2d (1+ i))))
	       (file (emacs-wiki-page-file sfile)))
	  (if (null file)
	      (princ (1+ i))
	    (progn
	      (princ (format 
		      "<a class=\"active\" title=\"%s\" href=\"%s.html\">%d</a>"
		      (emacs-wiki-real-page-title sfile) sfile (1+ i))))))
	(setq day (1+ day))
	(princ "</td>")
	)

	(princ "</tr></table>\n</div>")
      )))


(defun emacs-wiki-blog-section (file)
  "Section the blog entry into title, entry and category and
  return it as a property list. The properties are rawdate, date, day, title, category and text"
  (with-temp-buffer
    (insert-file-contents file)
    (save-excursion
      (goto-char 0)
      (let 
	  (
	   (rawdate (weblog-file-mdy file))
	   (date (let ((mday (weblog-file-mdy file)))
		   (concat 
		    (weblog-format-2d (nth 1 mday)) " " 
		    (substring (calendar-month-name (nth 0 mday)) 0 3)
		    " "
		    (format "%s" (nth 2 mday)))))

	   (title (if (re-search-forward "#title \\(.*\\)" nil t)
		      (match-string 1) (emacs-wiki-page-title file)))
	   (category (if (re-search-forward "#category \\(.*\\)" nil t)
			 (match-string 1) (concat "CategoryGeneral")))
	   (entry (buffer-substring-no-properties (point) (point-max)))
	   (day (calendar-day-name  (weblog-file-mdy file)))
	   (published-name (emacs-wiki-published-name x))
	   (published-file (emacs-wiki-published-file x))
	   )
	(list 'date date 'day day 'title title 'category category 'published-name published-name 
	      'published-file published-file 'rawdate rawdate
	      'text entry))
      )))
	   
	



(defun private-ewb-generate-archives-page (month year)
  "Generate the archives page for the (month year)"

  (let*
      ((files (sort  (directory-files (file-name-directory 
				       (emacs-wiki-page-file emacs-wiki-home-page)) nil 
				       (concat (format "%d" year) "." (weblog-format-2d  month)))  
		     '(lambda (a b) (weblog-mdy-lessp (weblog-file-mdy b) (weblog-file-mdy a))
			)))
       (mybuf))

    (mapcar '(lambda (x)
	       (with-temp-buffer
		 (insert-file-contents (emacs-wiki-page-file x) t)
		 (cd (file-name-directory (emacs-wiki-page-file x)))
		 (emacs-wiki-maybe)

		 (setq mybuf (concat mybuf
				     (private-emacs-wiki-blog-text
				      (emacs-wiki-blog-section (emacs-wiki-page-file x)))))
		 (if (buffer-modified-p)
		     (set-buffer-modified-p nil))))
	    files)

    (message "Done archive for %s %d" (calendar-month-name month) year)

    (when files
      (with-temp-file (concat "~/public_html/blog/archive_" (format "%d" year) "_" (weblog-format-2d  month) ".html")
	(insert (concat "#title Archive for " (calendar-month-name month) ", " (format "%d" year) "\n" ))
	(insert mybuf)
	(cd (file-name-directory (emacs-wiki-page-file emacs-wiki-home-page)))
	(emacs-wiki-replace-markup)
	))))


;; Entry point function
(defun emacs-wiki-blog-generate-archives ()
  (let ((start-year 2005))
    (dotimes (lyear 2)
      (dotimes (i 12)
	(let* ((year (- start-year lyear))
	       (month (- 12 i)))
	  (private-ewb-generate-archives-page month year))))))


(defun emacs-wiki-blog-generate-archives-link-and-pages (&optional numofmonths)
  "Generate archive links for the last numofmonths"
  
  (when (null numofmonths)
    (setq numofmonths 10))

  (let ((start-year 2005)
	(blog-dir (file-name-directory (emacs-wiki-page-file emacs-wiki-home-page)))
	(mybuf "<ul>"))
    
    (dotimes (lyear 3)
	     (dotimes (i 12)
	       (let* ((year (- start-year lyear))
		      (month (- 12 i))
		      (files (directory-files blog-dir nil (concat (format "%d" year) "." (weblog-format-2d month) "."))))
		      
		 (when files
		   (setq mybuf (concat mybuf 
				       "<li><a href=\"archive_"
				       (format "%d_%s" year (weblog-format-2d month))
				       (format ".html\">%s %d</a></li>" (calendar-month-name month) year)
				       ))))))

    (concat mybuf "\n</ul>\n")))


;; Entry point function
(defun emacs-wiki-blog-last-n-entries (&optional numofentries)
  "Generate the last n entries"
  (when (null numofentries)
    (setq numofentries 10))

    (let* ((time (decode-time (current-time)))
          (cur-day (nth 3 time))
          (cur-month (nth 4 time))
          (cur-year (nth 5 time))
	  (start-date (concat (weblog-format-2d cur-day) "." (weblog-format-2d cur-month) "." (format "%d" cur-year)))

	  (mybuf " ")

	  (files (sort  (directory-files (file-name-directory (emacs-wiki-page-file emacs-wiki-home-page)) nil *weblog-file-regexp*)  
			'(lambda (a b) (weblog-mdy-lessp (weblog-file-mdy b) (weblog-file-mdy a))
			   ))))

     (cd (file-name-directory (emacs-wiki-page-file emacs-wiki-home-page)))
     (setq cnt 0)

     (mapcar '(lambda (x)
		(if (< cnt numofentries)
		    (progn
		      (setq cnt (1+ cnt))
		      (with-temp-buffer
			(insert-file-contents (emacs-wiki-page-file x) t)
			(cd (file-name-directory (emacs-wiki-page-file x)))
			(emacs-wiki-maybe)

			(setq mybuf (concat mybuf
					    (private-emacs-wiki-blog-text
					     (emacs-wiki-blog-section (emacs-wiki-page-file x)))))
			
			(if (buffer-modified-p)
			    (set-buffer-modified-p nil))
			))))
	     files)
     (concat mybuf " ")
))


(defun private-emacs-wiki-blog-text (bobj)
  "Return the XHTML equivalent for this blog entry"
  (concat 
   "<!-- start of entry -->\n"
   "<div class=\"post\">"
   "<p class=\"time\">" (plist-get bobj 'day) " " (plist-get bobj 'date) "</p>"
   "<p class=\"title\">" (plist-get bobj 'title) "</p>"
   (plist-get bobj 'text)
   "<p class=\"category\">[" (plist-get bobj 'category) "]"
   (format " [<a rel=\"bookmark\" title=\"Permanent Link : %s\" href=\"%s\">#</a>]"
	   (plist-get bobj 'title) (plist-get bobj 'published-name))
   "</p>"
   "</div>"
   "<!-- end of entry -->\n"
   ))


;; Entry point function

(defun emacs-wiki-blog-generate-category-archives ()
  "Generate category archives."

  (let*
      ((archive-plist (list "CategoryGeneral" "\n" "CategoryBar" "<p></p>"))
       (files (sort  (directory-files (file-name-directory (emacs-wiki-page-file emacs-wiki-home-page)) nil *weblog-file-regexp*)  
		     '(lambda (a b) (weblog-mdy-lessp (weblog-file-mdy b) (weblog-file-mdy a))
			))))
    (mapcar '(lambda(x)
	       (let* ((obj (emacs-wiki-blog-section (emacs-wiki-page-file x)))
		      (pcat (plist-get obj 'category)))

		 (setq ocat (get 'archive-plist pcat))
		 ;(setq 'archive-plist (plist-put (symbol-plist 'archive-plist) 'LOL "YES"))
		      
		 (put 'archive-plist "CategoryGeneral" '"foo")
		 )

		      ) files)


))