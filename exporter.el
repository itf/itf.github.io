#! /bin/sh
":"; exec emacs --no-site-file --script "$0" "$@" # -*-emacs-lisp-*- 
;; The above line is bash trickery  https://stackoverflow.com/questions/6238331/emacs-shell-scripts-how-to-put-initial-options-into-the-script#6259330





;; based on http://pragmaticemacs.com/emacs/export-org-mode-headlines-to-separate-files/
;; export headlines to separate files
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

;;; Begin config
(setq org-export-head--html-postamble 
"<p class=\"author\">Author: Ivan Tadeu Ferreira Antunes Filho</p>
<p class=\"date\">Date: %T</p>
<p class=\"author\">Github:  <a href=\"https://github.com/itf/\">github.com/itf</a></p>
<p class=\"creator\">Made with %c and <a href=\"https://github.com/itf/org-export-head\">Org export head</a> </p>")

(setq org-export-head-tags-page "./tags.html") ; used for the tags to link to a page

;;End config

(defun org-export-head--run-on-temp-copy-buffer (function-to-run &rest args)
  "Runs function on a temp buffer with the contents of the original buffer"
  (save-excursion
    (let ((temp-buffer (generate-new-buffer "tmp")))
      (copy-to-buffer temp-buffer (point-min) (point-max)) 
      (with-current-buffer temp-buffer 
        (org-mode) 
        (outline-show-all) 
        (apply function-to-run args))
      (kill-buffer temp-buffer))))

(defun org-export-head (&optional directory-name backend reexport)
  "Updates the hashes and reexport all changed headings if reexport is nil.
Reexports all headings if reexport is non-nil"
  (interactive)
  (let ((directory-name (or directory-name (read-directory-name "Directory:")))
        (backend (or backend "html")))
    (make-directory directory-name t)
    (org-export-head--run-on-temp-copy-buffer #'org-export-head--modify-buffer-ast directory-name backend reexport)
    (org-export-head--update-hashes)))


(defun org-export-head-reexport (&optional directory-name backend)
  "Reexports all the headings"
  (interactive)
  (org-export-head directory-name backend t))


;;TODO this function should return a list of hashes to update the original buffer
(defun org-export-head--modify-buffer-ast (directory-path backend reexport)
  "Export all subtrees that are *not* tagged with :noexport: to
separate files.

Subtrees that do not have the :EXPORT_FILE_NAME: property set
are exported to a filename derived from the headline text."
  (org-export-head--update-hashes)
  (org-export-expand-include-keyword)

  ;; Create summaries before deleting the posts
  (org-export-head--create-summaries)
  (org-export-head--create-entry-tags) ;; creates tag lists
  ;; Delete content that has already been exported and set it to noreexport
  (if (not reexport)
      (org-export-head--delete-noreexport))

  

  ;;Get headlines, and generate macros (previous post, etc)
  (let* ((headlines-hash-list (org-export-head--get-headlines))
         (headlines-hash (car headlines-hash-list))
         (headlines-list (cdr headlines-hash-list))
         ;;Insert extra things in the headlines-hash to be used for fixing the macros
         ;;To define new headline-level macros, add extra functions here
         (headlines-hash (org-export-head--insert-next-previous-headline headlines-hash headlines-list))
         (headlines-hash (org-export-head--add-title-macro headlines-hash headlines-list))
         ;;Now we get global macros such as the index and the reversed index
         (global-macros (org-export-head--generate-index-alist headlines-list headlines-hash))
         

         ;;Now we get the templates. At the moment it is only the header
         (header (org-export-head--get-content-subtree-match "header"))
         ;;And now the footer, for example, for comments
         (footer (org-export-head--get-content-subtree-match "footer")))


    ;;For each not noexport/noreexport headline apply the template, i.e. copy contents
    (org-export-head--run-on-each-heading 
     #'(lambda ()
         (org-export-head--insert-on-header header)
         (org-export-head--insert-on-footer footer))
     "-noexport-noreexport")

    ;;After applying the template we replace the macros on all places
    (org-export-head--run-on-each-heading 
     #'(lambda ()
         (let* ((headline-name (org-export-head--headline))
                (headline-alist (gethash headline-name headlines-hash nil))
                (macro-alist (append headline-alist global-macros))) ;;in reverse order so that headline properties can overshadow these
           (org-export-head--replace-headline-macros macro-alist)))
     "-noexport-noreexport")

  
    ;;Get the parser tree and the headlines that will become files
    (let*  ((ast (org-element-parse-buffer)))
      
 
        ;;Fix links -- order is important. First external than fuzzy links
        (org-element-map ast 'link
          (lambda (link)
            (let* ((link (or (org-export-head--fix-file-external-link-ast directory-path link) link))
                   (link (or (org-export-head--fix-local-link-ast headlines-hash link) link))))))
      
      ;;Convert the buffer to contain the new AST, 
      ;;this is needed because the exporter expects the content to be in a buffer
      (erase-buffer) 
      (insert (org-element-interpret-data ast))
      
      
      (outline-show-all)
      
      ;;Finally export all the headers
      
      (org-export-head-export-headers directory-path backend))))
  


;;Not everything can be done using the AST, sadly.
;;Org element has no support for adding custom properties to headlines
;;Nor does it have a nice interface to grab the contents without the property drawer
;;Ideally everything would be done using the AST and org-element, since it is 
;;Less prone to writting bugs when using it. 
;;So right now it is only used for fixing links

;;START OF NON AST (non org-element) SESSION
(defun org-export-head--run-on-each-heading(fn match  &rest args)
  "Puts the point on each heading and runs the function. Needed for exporting all headings
   from  http://pragmaticemacs.com/emacs/export-org-mode-headlines-to-separate-files/"
  (save-excursion
    (goto-char (point-min))
    (goto-char (re-search-forward "^*"))
    (set-mark (line-beginning-position))
    (goto-char (point-max))
    (org-map-entries
     (lambda ()
       (apply fn args))
     match 'region-start-level)
    (deactivate-mark)))

(defun org-export-head-export-headers (directory-name backend)
  "Exports each heading to directory-name using backend"
  (if (equal backend "html")
      (org-export-head--run-on-each-heading 
       #'(lambda ()
           (org-set-property
            "EXPORT_FILE_NAME"
            (concat directory-name (org-export-head--escaped-headline)))
           (deactivate-mark)
           (let ((org-html-postamble org-export-head--html-postamble))
                (cl-letf (((symbol-function 'org-export-get-reference) (symbol-function 'org-export-get-reference-custom)))
                  (org-html-export-to-html nil t)))
           (set-buffer-modified-p t)) "-noexport-noreexport"))
  (if (equal backend "pdf")
      (org-export-head--run-on-each-heading 
       #'(lambda ()
           (org-set-property
            "EXPORT_FILE_NAME"
            (concat directory-name (org-export-head--escaped-headline)))
           (deactivate-mark)
           (org-latex-export-to-pdf nil t)
           (set-buffer-modified-p t)) "-noexport-noreexport")))

(defun org-export-head--goto-header(&optional no-new-line)
  "Puts point after property-block if it exists, in an empty line
  by creating a new line, unless no-new-line is non nil and returns point"
  (interactive)
  (org-back-to-heading t)
  (let* ((beg-end (org-get-property-block))
         (end (cdr beg-end)))
    (goto-char (or end (point))))
  (goto-char (point-at-bol 2)) ;;Advance one line
  (if (not no-new-line) 
      (progn
        (newline)
        (goto-char (point-at-bol 0)))) ;;Go back one line
  (point))

(defun org-export-head--goto-footer(&optional no-new-line)
  "Puts point at end of ubtree and returns point"
  (interactive)
  (org-end-of-subtree)
  (if (not no-new-line) 
      (progn
        (newline)))
  (point))


(defun org-export-head--get-content-subtree-at-point()
  (interactive)
  "Gets the content of the subtree at point, performing the necessary includes
to check if the hash has changed"
  (save-excursion
    (deactivate-mark t)
    (let* ((start (org-export-head--goto-header t))
          (end (org-end-of-subtree t))
          (buffer (current-buffer))
          (content (buffer-substring start end))
          (include-re "^[ \t]*#\\+INCLUDE:"))
      (if (string-match include-re content)
          (with-temp-buffer
            (insert content)
            (org-mode)
            (org-export-expand-include-keyword)
            (buffer-string))
      content))))


(defun org-export-head--get-summary-at-point(&optional n-paragraphs n-chars)
  "Gets the summary of the subtree at point"
  (interactive)
  (save-excursion
    (deactivate-mark t)
    (let* ((n-paragraphs (or n-paragraphs 1))
           (n-chars (or n-chars 200))
          (start (org-export-head--goto-header t))
          (endmax (save-excursion (org-end-of-subtree t)))
          (endparagraph
           (save-excursion
             (dotimes (i n-paragraphs)
               (org-forward-paragraph))
             (- (point) 1)))
          (end (min endmax endparagraph (+ start n-chars))))
      (buffer-substring start end))))


(defun org-export-head--create-summaries()
  "Creates summary for all the headings"
  (org-export-head--run-on-each-heading 
   #'(lambda()
       (let* ((summary (org-entry-get-with-inheritance "SUMMARY"))
              (summary (or summary (org-export-head--get-summary-at-point)))
              (summary (replace-regexp-in-string "\n" " " summary)))
         (if summary
             (org-set-property "SUMMARY" summary))))
   "-noexport"))

(defun org-export-head--create-entry-tags()
  "Creates list of tags with lin"
  (org-export-head--run-on-each-heading 
   #'(lambda()
       (let* ((entry-tags (assoc "ALLTAGS" (org-entry-properties)))
             (entry-tags (when entry-tags (delete "" (split-string (cdr entry-tags) ":"))))
             (url org-export-head-tags-page)
             (tags-text-url "")
             (tags-text ""))
         (dolist (tag entry-tags)
           (unless (or (string= tag "reexport") (string= tag "noexport") (string= tag "noreexport"))
             (setq  tags-text-url (concat tags-text-url "[[" url "#" tag "][#"tag"]] "))  
             (setq  tags-text (concat tags-text tag " "))))
         (org-set-property "TAGSURL" tags-text-url)
         (org-set-property "TAGSTEXT" tags-text)))
   "-noexport"))



;;; HASH code
;;Idea from https://emacs.stackexchange.com/a/39376/20165
(defun org-export-head--update-hashes()
  "Updates the hashes of all the headings"
  (org-export-head--run-on-each-heading 
   #'(lambda()
       (let ((new-hash  (format "%s" (org-export-head-get-hash-value-content)))
             (old-hash (org-entry-get-with-inheritance "HASH"))
             (older-hash (org-entry-get-with-inheritance "PREVIOUS-HASH"))) 
         (if (not old-hash)
             (progn
               (org-set-property "CREATION-DATE" (format-time-string "%Y-%m-%d"))))
         ;;If there was a change made
         (if (not (equal new-hash old-hash))
             (progn
               (org-set-property "MODIFICATION-DATE" (format-time-string "%Y-%m-%d"))
               (org-set-property "HASH" new-hash)))
         ;;Setting property is expensive
         (if (not (equal old-hash older-hash))
               (org-set-property "PREVIOUS-HASH" (or old-hash "")))))
   "-noexport"))


(defun org-export-head-get-hash-value-content()
  "Gets the hash of the subtree at point"
  (org-export-head-hash-function (org-export-head--get-content-subtree-at-point)))

(defun org-export-head-hash-function(text)
  "Function to calculate the hash of text.
Can be changed to something such as (length text) to run even faster.
Shouldn't rally affect the time to export unless your file contains over 100 thousand lines of text"
  (md5 text))

;;;END HASH CODE

(defun org-export-head--delete-noreexport()
  "Faster export by deleting things that won't be exported so we don't process them and their links"
  (org-export-head--run-on-each-heading 
   #'(lambda()
       (let ((old-hash (org-entry-get-with-inheritance "PREVIOUS-HASH"))
             (new-hash (org-entry-get-with-inheritance "HASH")))    
         ;;If there was a change made
         (if (equal new-hash old-hash)
             (progn
               (org-toggle-tag "noreexport" 'on)
               ;;faster export by deleting noexport things before processing
               (org-export-head--erase-content-subtree))))) 
   "-noexport-reexport"))

(defun org-export-head--erase-content-subtree()
  (save-excursion
    (let ((start (org-export-head--goto-header t))
          (end (org-end-of-subtree)))
      (delete-region start end))))



(defun org-export-head--get-headlines ()
  "Returns a tuple that contains a hashtable of headline name to Alist of headline properties
As well as a list of the headline names"
  (message "1")
  (flet ((make-hash ()
                   (make-hash-table :test 'equal))
         (add-to-hash (hashtable)
                      (puthash (org-export-head--headline) (org-entry-properties) hashtable)))
    (let ((headlines-hash (make-hash))
          (headlines-list ()))
      (org-export-head--run-on-each-heading 
       #'(lambda()
           
           (add-to-hash headlines-hash)
           (setq headlines-list (cons (org-export-head--headline) headlines-list)))
       "-noexport")
      (cons headlines-hash headlines-list))))


(defun org-export-head--headline ()
  "Gets the headline title if point is at the headline"
  (nth 4 (org-heading-components)))

(defun org-export-head--escaped-headline ()
  (org-export-head--escape (org-export-head--headline)))


(defun org-export-head--replace-headline-macros(macro-alist)
  "Replace macros of the type ###TEXT### They can contain information such as date
or previous and next post.
Any headline property can be used as a macro of this type."
  (save-excursion
    ;;Let's find the end of the headline as a marker, since it can move
    (let ((subtree-end-marker  (save-excursion (org-end-of-subtree) (point-marker)))) 
      ;; End of subtree might change because of macro expansion, so it is recalculated.
      ;; Macros might be substituted for something smaller, so we move the point on to the left at the end.
      (while (re-search-forward "\\#\\#\\#\\([-A-Za-z_]+\\)\\#\\#\\#" (marker-position subtree-end-marker) t)
        (unless (org-in-src-block-p)
          (let* ((macro (match-string-no-properties 1))
                 (macro-subs (cdr (assoc macro macro-alist))))
            (if macro-subs
                (replace-match  macro-subs t t)
              (replace-match ""))
            (backward-char)))))))


(defun org-export-head--get-content-subtree-match(match)
  "Get content of the subtree that matches \"match\"  
Where match is a tag or -tag or combination of them."
  (save-excursion
  (let ((content "")) 
    (org-export-head--run-on-each-heading
     #'(lambda() 
         (setq content (concat content (org-export-head--get-content-subtree-at-point)))) 
     match)
    content)))

(defun org-export-head--insert-on-header (text)
  "Insert text on the header of the subtree, but after the property box"
  (save-excursion
    (org-export-head--goto-header)
    (insert text)))

(defun org-export-head--insert-on-footer (text)
  "Insert text on the footer (end) of the subtree"
  (save-excursion
    (org-export-head--goto-footer)
    (insert text)))

(defun org-export-head--generate-index-alist (headlines-list headlines-hash)
  "Geneates an org list with the index of the website and inserts it in an alist"
  (let ((index "")
        (reverse-index "")
        (index-with-dates "")
        (index-with-summaries "")
        (all-tags "\n")
        (all-tags-reverse "\n")
        (all-tags-with-dates "\n")
        (all-tags-with-summaries "\n")
        (tags ())
        (tags-indexes ()))
    (dolist (headline-name headlines-list)
      (let* ((headline-alist (gethash headline-name headlines-hash nil))
             (entry-tags (assoc "ALLTAGS" headline-alist))
             (entry-tags (when entry-tags (delete "" (split-string (cdr entry-tags) ":"))))
             (creation-date (cdr (assoc "CREATION-DATE" headline-alist)))
             (modification-date (cdr (assoc "MODIFICATION-DATE" headline-alist)))
             (summary (string-trim (cdr (assoc "SUMMARY" headline-alist))))
             (index-entry (concat "- [["headline-name"]["headline-name"]]\n"))
             (index-entry-with-date (concat "- @@html:<b>@@[["headline-name"]["headline-name"]]@@html:</b>@@"
                                       "@@html:<span class=\"page-date\">@@"
                                       " (" creation-date", updated " modification-date ")"
                                       "@@html:</span>@@" "\n" ))
             (index-entry-with-summary 
              (concat  index-entry-with-date 
                       (unless (= (length summary) 0) 
                         (concat "   @@html:<br>@@" summary "\n")))))
        
        (setq index (concat index index-entry))
        (setq reverse-index (concat index-entry reverse-index))
        (setq index-with-dates (concat  index-with-dates index-entry-with-date))
        (setq index-with-summaries (concat  index-with-summaries index-entry-with-summary))

        (dolist (tag entry-tags)
          (if (not (member tag tags))
              (setq tags (cons tag tags)))
          (dolist (suffix '("" "-reverse" "-with-dates" "-with-summaries"))
            ;; Initialize tags lists
            (let ((tag-index-name (upcase (concat tag suffix))))
              (unless (assoc tag-index-name tags-indexes) 
                (setq tags-indexes (cons `(,tag-index-name . "")  tags-indexes)))))

          ;;Add tag indexes to list
          (let* ((tag (upcase tag))
                (tag-reverse (upcase (concat tag "-reverse")))
                (tag-with-dates (upcase (concat tag "-with-dates")))
                (tag-with-summaries (upcase (concat tag "-with-summaries")))
                (tag-assoc (assoc tag tags-indexes))
                (tag-assoc-reverse (assoc tag-reverse tags-indexes))
                (tag-assoc-with-dates (assoc tag-with-dates tags-indexes))
                (tag-assoc-with-summaries (assoc tag-with-summaries tags-indexes))
                (tag-index (cdr tag-assoc))
                (tag-index-reverse (cdr tag-assoc-reverse))
                (tag-index-with-dates (cdr tag-assoc-with-dates))
                (tag-index-with-summaries (cdr tag-assoc-with-summaries)))

            (setf (cdr tag-assoc) (concat tag-index index-entry))
            (setf (cdr tag-assoc-reverse) (concat index-entry tag-index-reverse ))
            (setf (cdr tag-assoc-with-dates) (concat tag-index-with-dates index-entry-with-date))
            (setf (cdr tag-assoc-with-summaries) (concat tag-index-with-summaries index-entry-with-summary))))))
    
    ;; Now we create an index for all tags, to be able to have tag pages
    (sort tags (lambda (a b) (string<  a  b))) ; Sort the tags for the index of all tags
    (dolist (tag tags)
      (unless (or (string= tag "reexport") (string= tag "noexport") (string= tag "noreexport"))
      (let* ((tag-reverse (upcase (concat tag "-reverse")))
            (tag-with-dates (upcase (concat tag "-with-dates")))
            (tag-with-summaries (upcase (concat tag "-with-summaries")))
            (tag-with-summaries-list (cdr (assoc tag-with-summaries tags-indexes)))
            (tag-list (cdr (assoc tag tags-indexes)))
            (tag-reverse-list (cdr (assoc tag-reverse tags-indexes)))
            (tag-with-dates-list (cdr (assoc tag-with-dates tags-indexes))))
        (setq all-tags (concat all-tags "** " tag "\n" tag-list))
        (setq all-tags-reverse (concat all-tags-reverse "** " tag "\n" tag-reverse-list))
        (setq all-tags-with-dates (concat all-tags-with-dates "** " tag "\n" tag-with-dates-list))
        (setq all-tags-with-summaries (concat all-tags-with-summaries "** " tag "\n" tag-with-summaries-list)))))
            

    (append 
     (list (cons "INDEX" index) (cons "INDEX-REVERSE" reverse-index)  (cons "INDEX-WITH-DATES" index-with-dates) (cons "INDEX-WITH-SUMMARIES" index-with-summaries) 
           (cons "ALL-TAGS" all-tags) (cons "ALL-TAGS-REVERSE" all-tags-reverse)  (cons "ALL-TAGS-WITH-DATES" all-tags-with-dates) (cons "ALL-TAGS-WITH-SUMMARIES" all-tags-with-summaries))
     tags-indexes)))

;;END OF NON AST (non org-element) SESSION


(defun org-export-head--fix-local-link-ast (headlines link)
  "Fixes fuzzy links to headlines, so the they point to new files"
  (flet ((get-hash (element set)
                   (gethash element set nil)))
    (when (string= (org-element-property :type link) "fuzzy")
      (let* ((path  (org-element-property :path link))
             (new-path (get-hash path headlines))) 
        (if new-path
          (let ((link-copy (org-element-copy link)))
            (apply #'org-element-adopt-elements link-copy (org-element-contents link))
            (org-element-put-property link-copy :type "file")
            (org-element-put-property link-copy :path (concat (org-export-head--escape path) ".org"))
            (org-element-set-element link link-copy))
          ;; else: need to check if the link is linking to a subheadline
         (save-excursion         
           (org-link-search path)
           ;; If the same heading contains multiple subheadings, each with the same name; it will simply link to the first one
           (let ((link-copy (org-element-copy link)))
            (apply #'org-element-adopt-elements link-copy (org-element-contents link))
            (org-element-put-property link-copy :type "file")
            (org-element-put-property link-copy :path (concat (org-export-head--escape (org-find-top-headline)) ".org"))
            (org-element-put-property link-copy :search-option (concat "#" (org-export-head--escape path)))
            (org-element-set-element link link-copy))
           ))))))


(defun org-export-head--fix-file-external-link-ast (directory-path link)
  "Creates hard links to the external files in the output directory
Only modifies links if file exists"
  (when (and (string= (org-element-property :type link) "file")  (file-exists-p (org-element-property :path link)))
    (let* ((path (org-element-property :path link))
           (link-copy (org-element-copy link))
           (extension (file-name-extension path))
           (img-extensions '("jpg" "tiff" "png" "bmp"))
           (link-description (org-element-contents link))
           ;;Removes ../ from the releative path of the file to force it to be moved to a subfolder
           ;;of the current dir. This causes some file conflits in edge cases
           ;;e.g: ../images and ../../images will map to the same place. This should be rare in normal usage
           (new-relative-path 
            (concat "./" (file-name-extension path) "/" (file-name-nondirectory path)))
           (new-hard-link-path (concat directory-path new-relative-path))
           (new-hard-link-directory (file-name-directory new-hard-link-path)))
      
      ;;Fix the AST
      ;;If image, remove description so it will become a real image instead of a link
      (unless (or (member extension img-extensions))
        (apply #'org-element-adopt-elements link-copy link-description))
      (org-element-put-property link-copy :path new-relative-path)
      (org-element-set-element link  link-copy)
      
      ;;Create hard link folder
      (make-directory new-hard-link-directory t)
      ;;Create hard link, not replacing if it already exists, catching error if file does not exist
      (condition-case nil
          (add-name-to-file path new-hard-link-path nil)
        (error nil)))))


(defun org-export-head--insert-next-previous-headline(headlines-hash headlines-list)
  "Decides what is the next and the previous post and create macro"
  (let* ((temp-list (cons nil headlines-list))
         (len (length headlines-list)))
    (dotimes (i len)
      (let* ((previous (nth 0 temp-list))
             (headline-name (nth 1 temp-list))
            (next (nth 2 temp-list))
            (headline (gethash headline-name headlines-hash nil))
            (new-properties 
             (list (cons "PREVIOUS" (or next "index"))
                   (cons "NEXT" (or previous "index"))))
            (headline (append headline new-properties))) ;; In reverse order, to allow headline properties to shadow this.
        (puthash headline-name headline headlines-hash))
        (setq temp-list (cdr temp-list))))
  headlines-hash)
      
(defun org-export-head--add-title-macro(headlines-hash headlines-list)
  "Creates title macro"
  (let* ((temp-list (cons nil headlines-list))
        (len (length headlines-list)))
    (dotimes (i len)
      (let* ((headline-name (nth 1 temp-list))
            (headline (gethash headline-name headlines-hash nil))
            (new-properties 
             (list (cons "TITLE" headline-name)))
            (headline (append headline new-properties))) ;; In reverse order, to allow headline properties to shadow this.
        (puthash headline-name headline headlines-hash))
        (setq temp-list (cdr temp-list))))
  headlines-hash)


(defun org-export-head--headline-to-file(headline-name)
  "Generate the file name of the headline"
  (concat (org-export-head--escape headline-name) ".org"))


(defun org-export-head--escape(text)
  (when text
    (let* ((text (replace-regexp-in-string " " "_" text))
           (text (replace-regexp-in-string "/" "-" text))
           (text  (replace-regexp-in-string "[^[:alnum:]-_]" "" (downcase text))))
      text)))


;;Nice export headings http://ivanmalison.github.io/dotfiles/#usemyowndefaultnamingschemefororgheadings
(defun imalison:org-get-raw-value (item)
  (when (listp item)
    (let* ((property-list (cadr item)))
      (when property-list (plist-get property-list :raw-value)))))

(defun imalison:generate-name (datum cache)
  (let ((raw-value (imalison:org-get-raw-value datum)))
    (if raw-value
        (org-export-head--escape raw-value)
      ;; This is the default implementation from org
      (let ((type (org-element-type datum)))
        (format "org%s%d"
                (if type
                    (replace-regexp-in-string "-" "" (symbol-name type))
                    "secondarystring")
                (incf (gethash type cache 0)))))))


;(use-package ox)
;  :defer t
;  :config
  (defun org-export-get-reference-custom (datum info)
    "Return a unique reference for DATUM, as a string.
DATUM is either an element or an object.  INFO is the current
export state, as a plist.  Returned reference consists of
alphanumeric characters only."
    (let ((type (org-element-type datum))
          (cache (or (plist-get info :internal-references)
                     (let ((h (make-hash-table :test #'eq)))
                       (plist-put info :internal-references h)
                       h)))
          (reverse-cache (or (plist-get info :taken-internal-references)
                             (let ((h (make-hash-table :test 'equal)))
                               (plist-put info :taken-internal-references h)
                               h))))
      (or (gethash datum cache)
          (let* ((name (imalison:generate-name datum cache))
                 (number (+ 1 (gethash name reverse-cache -1)))
                 (new-name (format "%s%s" name (if (< 0 number) (format "%s%s" "." number) ""))))
            (puthash name number reverse-cache)
            (puthash datum new-name cache)
            new-name))))



(defun org-export-head-other-file (file directory-name &optional reexport)
  "Main function of this script"
  (find-file file)
  (make-directory "../.emacs-saves" t)
  (let ((backup-directory-alist `(("." .  "../.emacs-saves/")))
        (auto-save-file-name-transforms `((".*" "../.emacs-saves/" t))))
    
  (require 'ox)
  (require 'cl)
  (require 'subr-x)
  (org-mode)
  (transient-mark-mode) ;necessary for using org-map-entries
  (outline-show-all) 
  (org-export-head (concat directory-name "/") nil reexport)
  (save-buffer)))

(let ((file  (elt argv 0))
      (dir  (elt argv 1))
      (reexport  (elt argv 2)))
  (if (or (not file) (not dir))
      (message "usage  FILE DIR [export]")
    (message "Exportinf %s to %s" file dir)
    (org-export-head-other-file file dir reexport)))

