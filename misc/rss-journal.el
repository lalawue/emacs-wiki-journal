(defcustom emacs-wiki-online-target
  "http://lalawudrop.51.net/blog/"
  "Base URL where the files are published."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-rss-filename
  "wiki.rdf"
  "Name for the RSS file."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-rss-title
  "Lalawu's Journal"
  "Title the RSS channel."
  :type 'string
  :group 'emacs-wiki-publish)

(defcustom emacs-wiki-rss-description
  "Lalawu's lives and essay"
  "Description of the RSS channel."
  :type 'string
  :group 'emacs-wiki-publish)

; Redefine publication to include RSS generation
(define-key emacs-wiki-mode-map
  [(control ?c) (control ?p)]
  'my-emacs-wiki-publish)

(defun my-emacs-wiki-publish ()
  "Publishes pages and RSS."
  (interactive)
  (emacs-wiki-publish)
  (emacs-wiki-rss))

(defun emacs-wiki-generate-rss (&optional as-list exclude-private)
  "Generate RSS for all Wiki pages."
  (let ((project emacs-wiki-current-project))
    (with-current-buffer (get-buffer-create "*Wiki RSS*")
      (erase-buffer)
      (if project
          (emacs-wiki-change-project project))
      (insert "<?xml version='1.0'?>
<rdf:RDF 
  xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'
  xmlns='http://purl.org/rss/1.0/'
  xmlns:dc='http://purl.org/dc/elements/1.1/'
>
<channel rdf:about='"
      emacs-wiki-online-target
      emacs-wiki-rss-filename
      "'>
<title>"
      emacs-wiki-rss-title
      "</title>
<link>"
      emacs-wiki-online-target
      (emacs-wiki-published-name emacs-wiki-default-page)
      "</link>
<description>"
      emacs-wiki-rss-description
      "</description>
")
      ; Stolen from http://www.gohome.org/teranisi/EmacsWiki.html
      ; by Yuuichi Teranishi - http://www.gohome.org/teranisi/WelcomePage.html
      (let ((files (sort (mapcar (lambda (pair)
   (list
    (car pair)
    (cdr pair)
    (nth 5 (file-attributes (cdr pair)))))
 (emacs-wiki-file-alist))
 (function
  (lambda (l r)
    (not (emacs-wiki-time-less-p
  (nth 2 l)(nth 2 r)))))))
    file)
(setq f files)
(insert "
<items>
  <rdf:Seq>\n")
(while files
  (unless (and exclude-private
       (emacs-wiki-private-p (caar files)))
    (insert
     "<rdf:li resource='"
     emacs-wiki-online-target
     (emacs-wiki-published-name (caar files)) 
     "'/>\n"))
  (setq files (cdr files)))
(insert "</rdf:Seq></items></channel>")
  (setq files f)
  (while files
    (unless (and exclude-private
 (emacs-wiki-private-p (caar files)))
      (setq name (caar files))
      (insert
       "<item rdf:about='"
       emacs-wiki-online-target
       (emacs-wiki-published-name name)
       "'>\n<title>"
       (emacs-wiki-page-title name)
       "</title>\n"
       "<link>"
       emacs-wiki-online-target
       (emacs-wiki-published-name name)
       "</link>\n"
       "<dc:date>"
       (if as-list "- " "") (format-time-string 
     "%Y-%m-%dT%T"
     (nth 2 (car files)))
       "</dc:date>\n"
       "<dc:creator>"
       emacs-wiki-maintainer
       "</dc:creator>\n"
       "</item>\n"))
    (setq files (cdr files))))
      (insert "</rdf:RDF>")
      (current-buffer))))

(defun emacs-wiki-rss ()
  "Display RSS for all known Journal pages."
  (interactive)
  (message "Generating Journal RSS...")
  (pop-to-buffer (emacs-wiki-generate-rss))
  (goto-char (point-min))
  (let ((backup-inhibited t))
    (write-file
     (expand-file-name emacs-wiki-rss-filename
       emacs-wiki-journal-publishing-directory)))
  (kill-buffer (current-buffer))
  (delete-window)
  (message "Generating Journal RSS...done"))
