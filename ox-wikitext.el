;;; ox-wikitext --- TiddlyWiki WikiText Back-End for Org Export Engine

;; Copyright (C) 2016 Victor Santos
;; Copyright (C) 2022 Konrad Hinsen

;; Author: Victor Santos <victor_santos@fisica.ufc.br>
;; Keywords: outlines, tiddlywiki, wiki, wikitext

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; ox-wikitext.el lets you convert Org-roam files to tiddler files
;; using WikiText markup, via the ox.el export engine.
;;
;; This is a heavily personalized version, specifically for exporting
;; notes from an org-roam directory managed by git.
;;
;; Put this file into your load-path and the following into your ~/.emacs:
;;	 (require 'ox-wikitext)
;;
;; Export Org files to WikiText:
;; M-x org-wikitext-export-as-wikitext RET
;;
;;; Code:

;;; Dependencies

(require 'ox-html)

(eval-when-compile
  (require 'dash)
  (require 's)
  (require 'ox))

;;; Internal functions
(defun vct:tiddlywiki-date ()
  " Return the output of `current-time-string' in a suitable form for TiddlyWiki.
In TiddlyWiki, values of date fields are 17-character strings:
    - 4 digits for the year
    - 2 digits for the month
    - 2 digits for the day
    - 2 digits for the hour
    - 2 digits for the minute
    - 2 digits for the second
    - 3 digits for the millisecond

To avoid problems arising from differences of time zone, TiddlyWiki always uses UTC.

As an example, the created field of this tiddler has the value 20150117190213631."
  (format-time-string "%Y%m%d%H%M%S%3N"))

;;; Variables and options

(defcustom org-wikitext-extension "tid"
  "File extension for the WikiText tiddler."
  :group 'org-export-wikitext
  :type 'string)

(defcustom org-wikitext-coding-system (symbol-name org-html-coding-system)
  "Coding system for the exported file."
  :group 'org-export-general
  :version "24.4"
  :package-version '(Org . "8.0")
  :type 'coding-system)

;; Define the backend
(org-export-define-derived-backend 'wikitext 'html
  :menu-entry
  '(?w "Export to WikiText"
       ((?W "As WikiText buffer" org-wikitext-export-as-wikitext)
        (?w "As WikiText file" org-wikitext-export-to-wikitext)
        ))
  :options-alist
  '(
    (:description "DESCRIPTION" nil nil newline)
    (:keywords "KEYWORDS" nil nil space)
    )
  :translate-alist
  '(
    (bold . org-wikitext-bold)
    (code . org-wikitext-code)
    (headline . org-wikitext-headline)
    (horizontal-rule . org-wikitext-horizontal-rule)
    (italic . org-wikitext-italic)
    (link . org-wikitext-link)
    (paragraph . org-wikitext-paragraph)
    (section . org-wikitext-section)
    (src-block . org-wikitext-src-block)
    (strike-through . org-wikitext-strike-through)
    (template . org-wikitext-template)
    (underline . org-wikitext-underline)
    (verbatim . org-wikitext-verbatim)
    ))

;;; Transcoding functions

;;;; Bold

(defun org-wikitext-bold (bold contents info)
  "Transcode BOLD object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (format "''%s''" contents))

;;;;  Code

(defun org-wikitext-code (code contents info)
  "Transcode CODE object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (let* ((value (org-element-property :value code))
         (lang (org-element-property :language code)))
    (format "``%s``" value)))

;;;; Headline

(defun org-wikitext-headline (headline contents info)
  "Transcode HEADLINE element.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let* ((level (org-export-get-relative-level headline info))
         (title (org-export-data-with-backend
                 (org-element-property :title headline)
                 'wikitext info)))
    (concat
     (format "\n\n<<nh%s \"%s\">>\n\n" level title)
     contents)))

;;;; Horizontal rule

(defun org-wikitext-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (format "\n%s    \n" "------"))

;;;; Italic

(defun org-wikitext-italic (italic contents info)
  "Transcode ITALIC object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (format "//%s//" contents))

;;;; Link

(defun org-wikitext-title-for-id (id)
  (caar
   (org-roam-db-query
    [:select title :from nodes
             :where (= id $s1)
             :limit 1]
    id)))

(defun org-wikitext-link (link desc info)
  "Transcode LINK object.
DESC is the description of the link, or an empty string. INFO is a plist used as a communication channel."
  (let* ((type (org-element-property :type link))
         (raw-path (org-element-property :path link))
         (desc (and (not (string= desc "")) desc))
         (path (cond
                ((member type '("http" "https" "ftp" "mailto" "doi"))
                 (concat type ":" raw-path))
                ((string= type "file")
                 (org-export-file-uri raw-path))
                ((string= type "id") ;; only for org-roam page links
                 (org-wikitext-title-for-id raw-path))
                (t raw-path))))
    (cond
     ;; Link with description
     ((and path desc)
      (if (string= path desc)
          (format "[[%s]]" path)
        (format "[[%s|%s]]" desc path)))
     ;; Link without description
     (path (format "[[%s]]" path))
     ;; Link with only description
     (t desc)
     )
    ))


;;;; Paragraph

(defun org-wikitext-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (format "%s" contents)
  )

;;;; Section

(defun org-wikitext-section (section contents info)
  "Transcode SECTION object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (format "%s" contents))

;;;;  Src-block

(defun org-wikitext-src-block (src-block contents info)
  "Transcode SRC-BLOCK object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (let ((lang (org-element-property :language src-block))
         (content (org-export-format-code-default src-block info)))
    (format "``%s``" content)))

;;;; Strike-through

(defun org-wikitext-strike-through (strike-through contents info)
  "Transcode STRIKE-THROUGH object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (format "~~%s~~" contents))

;; Retrieve initial and last modification time stamps via git.

(defun org-wikitext-time-stamp (time)
  (format-time-string "%Y%m%d%H%M%S%3N" time))

(defun org-wikitext-file-modification-time ()
  (--> (buffer-file-name)
       file-attributes
       file-attribute-modification-time))

(defun org-wikitext-first-revision-time-stamp-from-git ()
  (--> (buffer-file-name)
       (concat "git -C " org-roam-directory " log --format=%at -- " it " | tail -1")
       shell-command-to-string
       string-to-number))

(defun org-wikitext-first-revision-time-stamp ()
  (let ((time-stamp (org-wikitext-first-revision-time-stamp-from-git)))
    (when (equal time-stamp 0)
        (setq time-stamp (org-wikitext-file-modification-time)))
    (org-wikitext-time-stamp time-stamp)))

(defun org-wikitext-last-revision-time-stamp-from-git ()
  (--> (buffer-file-name)
       (concat "git -C " org-roam-directory " log --format=%at -- " it " | head -1")
       shell-command-to-string
       string-to-number))

(defun org-wikitext-last-revision-time-stamp ()
  (let ((time-stamp (org-wikitext-last-revision-time-stamp-from-git)))
    (when (equal time-stamp 0)
        (setq time-stamp (org-wikitext-file-modification-time)))
    (org-wikitext-time-stamp time-stamp)))

;;;; Template

(defun org-wikitext-current-id ()
  (caar
   (org-roam-db-query
    [:select * :from nodes
             :where (= file $s1)
             :limit 1]
    (buffer-file-name))))

(defun org-wikitext-backlinks-for-id (id)
  (->> (org-roam-db-query
         [:select * :from links
                  :where (= dest $s1)]
         id)
       (-map #'cadr)
       -distinct))

(defun org-wikitext-backlink-titles-for-id (id)
  (->> (org-wikitext-backlinks-for-id id)
       (-map #'org-wikitext-title-for-id)
       (-map #'(lambda (s)
                 (if (s-contains? " " s)
                     (concat "[[" s "]]")
                   s)))))

(defun org-wikitext-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string. INFO is a plist holding export options"
  (let* ((title (org-export-data (plist-get info :title) info))
         (tags (org-export-data (plist-get info :filetags) info))
         (refs (org-entry-get (point-min) "ROAM_REFS"))
         (current-id (org-wikitext-current-id)))
    (s-concat
     (format "title: %s\n" title)
     (format "created: %s\n" (org-wikitext-first-revision-time-stamp))
     (format "modified: %s\n" (org-wikitext-last-revision-time-stamp))
     (format "tags: %s\n" tags)
     (format "references: %s\n" (if refs refs ""))
     (format "backlinks: %s\n"
             (let ((backlinks (org-wikitext-backlink-titles-for-id current-id)))
               (if backlinks (mapconcat #'identity backlinks " ") "")))
     "type: text/vnd.tiddlywiki\n\n"
     "\\import [[$:/org-roam/NoteMacros]]\n\n"
     "<<references>>\n\n"
     contents
     "\n\n<<backlinks>>\n")))

;;;; Underline

(defun org-wikitext-underline (underline contents info)
  "Transcode UNDERLINE object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (format "__%s__" contents))

;;;; Verbatim

(defun org-wikitext-verbatim (verbatim contents info)
  "Transcode VERBATIM object.
CONTENTS is the text within bold markup. INFO is a plist used as a communication channel."
  (let* ((value (org-element-property :value verbatim)))
    (format "``%s``" value)))


;;; Export functions

;;;###autoload
(defun org-wikitext-export-as-wikitext
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a WikiText buffer."
  (interactive)
  (org-export-to-buffer 'wikitext "*Org WIKITEXT Export*"
    async subtreep visible-only body-only ext-plist
    (lambda () (set-auto-mode t))))

;;;###autoload
(defun org-wikitext-export-to-wikitext
  (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a WikiText file."
  (interactive)
  (let ((fileName (org-export-output-file-name (concat "." org-wikitext-extension) subtreep)))
    (org-export-to-file 'wikitext fileName async subtreep visible-only body-only ext-plist)
    ))

;;;###autoload
(defun org-wikitext-publish-to-wikitext (plist filename pub-dir)
  "Publish an org file to WikiText.

FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.

Return output file name."
  (let* ((extension ".tid")
         (output-file-name (concat (file-name-as-directory pub-dir)
		                   (file-name-nondirectory (file-name-sans-extension filename))
                                   extension)))
    (when (file-newer-than-file-p filename output-file-name)
      (org-publish-org-to 'wikitext filename extension plist pub-dir))))

(provide 'ox-wikitext)
