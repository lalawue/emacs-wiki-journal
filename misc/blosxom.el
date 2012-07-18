;;; blosxom.el --- Publish a wiki tree for serving by Blosxom

;; Copyright (C) 2004 Gary V. Vaughan

;; Emacs Lisp Archive Entry
;; Filename: emacs-wiki-blosxom.el
;; Version: 0.0.1
;; Date: Wed, 24 March 2004
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Maintainer: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Publish a local Emacs Wiki tree for serving by Blosxom
;; URL: http://tkd.kicks-ass.net/arch/gary@gnu.org--2004/emacs-wiki--gary--1.0
;; Compatibility: Emacs21

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
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

;;; Commentary:

;; I maintain the hypertext parts of my website with John Wiegley's
;; emacs-wiki, now maintained by Michael Olson at
;; http://www.mwolson.org/projects/EmacsWiki.html.  You will need to
;; install a copy of that file before this one is of any use to you.
;;
;; Blosxom wiki publishes a tree of categorised Wiki files to a mirrored
;; tree of blosxom stories to be served by blosxom.cgi.
;;
;; Each Blosxom Wiki file must include `#date yyyy-mm-dd', or optionally
;; the longer `#date yyyy-mm-dd hh:mm', plus whatever normal emacs wiki
;; content is desired.
;;
;; If you want to change `blosxom-directory' and some other variables,
;; either use Customize or use `blosxom-option-customized'.  For
;; example:
;;
;;    (blosxom-option-customized 'blosxom-directory "~/Blosxom")
;;    (blosxom-option-customized 'blosxom-publishing-directory
;;                               "~/public_html/blog")
;;
;; and if you want to modify other emacs-wiki variables for the blosxom
;; project:
;;
;;    (add-to-list 'blosxom-custom-variables
;;                 '(some-emacs-wiki-variable . "some-blosxom-wiki-value"))
;;    (blosxom-option-customized 'blosxom-custom-variables
;;                               blosxom-custom-variables)
;;
;; See `emacs-wiki-update-project' and `blosxom-custom-variables' for more
;; details.

(require 'emacs-wiki)

(defvar blosxom-loaded nil)

(defun blosxom-option-customized (sym val)
  "Set SYM to VAL and update the Blosxom project."
  (set sym val)
  (when blosxom-loaded
    (blosxom-update-wiki-project)))

(defgroup blosxom nil
  "An extension of emacs-wiki for publishing to a blosxom tree."
  :prefix "blosxom-"
  :group 'applications)

(defcustom blosxom-directory "~/BlosxomWiki"
  "A directory of wiki pages to be published for serving by blosxom."
  :require 'blosxom
  :type 'directory
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-publishing-directory emacs-wiki-publishing-directory
  "The directory that the blosxom wiki is published to."
  :type 'directory
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-publishing-file-suffix ".txt"
  "This suffix will be appended to all wiki names when publishing."
  :type 'string
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-publishing-header "<lisp>(emacs-wiki-page-title)</lisp>\n"
  "Text to prepend to a story being published."
  :type 'string
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-publishing-footer ""
  "Text to append to a story being published."
  :type 'string
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-custom-variables nil
  "A list of blosxom specific emacs-wiki variable settings.
You con customize any emacs-wiki variable to be used specially within
blosxom mode buffers, exept for the following, whose values are
derived from the other blosxom mode customized variables:

  `emacs-wiki-directories'
  `emacs-wiki-major-mode'
  `emacs-wiki-markup-tags'
  `emacs-wiki-publishing-markup'
  `emacs-wiki-publishing-file-suffix'
  `emacs-wiki-publishing-header'
  `emacs-wiki-publishing-footer'
  `emacs-wiki-publishing-directory'
  `emacs-wiki-after-wiki-publish-hook'

If you want to customize the derived variables, you can set them from
`blosxom-mode-hook'."
  :type `(repeat
          (choice
           (cons :tag "emacs-wiki-predicate"
                 (const emacs-wiki-predicate) function)
           (cons :tag "emacs-wiki-project-server-prefix"
                 (const emacs-wiki-project-server-prefix) string)
           ,@(mapcar
              (function
               (lambda (sym)
                 (list 'cons :tag (symbol-name sym)
                       (list 'const sym)
                       (get sym 'custom-type))))
              (apropos-internal "\\`emacs-wiki-"
                                (function
                                 (lambda (sym)
                                   (get sym 'custom-type)))))))
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-markup-tags
  '(("blosxom-entries" t t nil blosxom-tag))
  "A list of tag specifications used for marking up blosxom pages.
See the documentation for `emacs-wiki-markup-tags'."
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Highlight tag" :value nil)
                       function))
  :set 'blosxom-option-customized
  :group 'blosxom)

(defcustom blosxom-publishing-markup
  '(["^#\\(date\\)\\s-+\\(.+\\)\n+" 0 blosxom-markup-date-directive])
  "List of additional markup rules to apply when publishing blosxom stories.
These rules are performed first, before any emacs-wiki rules.
See the docs for `emacs-wiki-publishing-markup' for more info."
  :type '(repeat
          (vector :tag "Markup rule"
                  (choice regexp symbol)
                  integer
                  (choice string function symbol)))
  :set 'blosxom-option-customized
  :group 'blosxom)

(defvar blosxom-mode-map
  (let ((map (copy-keymap emacs-wiki-mode-map)))
    (mapcar
     (function (lambda (key-mapping)
                 (define-key map (car key-mapping) (cdr key-mapping))))
     ;; Typically, published pages are restamped in the past, so we
     ;; only want to publish the current page with C-cC-p.
     '(([(control ?c) (control ?p)] . emacs-wiki-publish-this-page)))
    map)
  "Keymap used by Blosxom mode.")

;;; Mode

(defvar blosxom-project "BlosxomWiki")

;;;###autoload
(define-derived-mode blosxom-mode emacs-wiki-mode "Blosxom"
  "An extension to Emacs Wiki that publishes to a blosxom story tree.
\\{blosxom-mode-map}"
  (add-hook 'emacs-wiki-update-project-hook
            'blosxom-update-wiki-project nil t)
  (add-hook 'emacs-wiki-after-file-publish-hook
            'blosxom-set-time nil t))


;;; Maintain (published-file . date) alist

(defvar blosxom-page-date-alist nil)

(defun blosxom-markup-date-directive ()
  "Add a date entry to `blosxom-page-date-alist' for this page."
  (when (string= (match-string 1) "date")
    (let ((date (match-string 2)))
      (save-match-data
        (add-to-list
         'blosxom-page-date-alist
         `(,(emacs-wiki-published-file) . ,date)))))
  "")

(defun blosxom-set-time (file)
  "Reset the modification timestamp for published FILE.
Blosxom uses the modification time of a published file as its publication
date-time.  Adding this function to `emacs-wiki-after-file-publish-hook'
will set the modification time of the published page according to the value
stored in `blosxom-page-date-alist'."
  (let* ((page (emacs-wiki-page-name file))
         (published (emacs-wiki-published-file page))
         (date (cdr (assoc published blosxom-page-date-alist))))
    (when date
      (shell-command
       (format "touch --date='%s' %s" date published)))))

(defun blosxom-update-wiki-project ()
  "Update the \"blosxom\" project in `emacs-wiki-projects'."
  (setq emacs-wiki-projects
        (delq (assoc blosxom-project emacs-wiki-projects)
              emacs-wiki-projects))
  (add-to-list
   'emacs-wiki-projects
   `(,blosxom-project .
     ((emacs-wiki-directories . (,blosxom-directory))
      (emacs-wiki-major-mode  . blosxom-mode)
      (emacs-wiki-markup-tags . ,(append blosxom-markup-tags
                                         emacs-wiki-markup-tags))
      (emacs-wiki-publishing-markup . ,(append blosxom-publishing-markup
                                               emacs-wiki-publishing-markup))
      (emacs-wiki-publishing-file-suffix . ,blosxom-publishing-file-suffix)
      (emacs-wiki-publishing-header . ,blosxom-publishing-header)
      (emacs-wiki-publishing-footer . ,blosxom-publishing-footer)
      (emacs-wiki-publishing-directory . ,blosxom-publishing-directory)
      (emacs-wiki-after-wiki-publish-hook . nil)
      ,@blosxom-custom-variables)))
  (emacs-wiki-update-project-interwikis))

;;; Initialisation

(setq blosxom-loaded t)
(blosxom-update-wiki-project)

(provide 'blosxom)
