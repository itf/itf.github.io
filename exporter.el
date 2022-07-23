#! /bin/sh
":"; exec emacs --no-site-file --script "$0" "$@" # -*-emacs-lisp-*- 
;; The above line is bash trickery  https://stackoverflow.com/questions/6238331/emacs-shell-scripts-how-to-put-initial-options-into-the-script#6259330


(require 'org)


;; based on http://pragmaticemacs.com/emacs/export-org-mode-headlines-to-separate-files/
;; export headlines to separate files
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

;;; Begin config
(setq org-export-head--html-postamble 
"<p class=\"author\">Author: Ivan Tadeu Ferreira Antunes Filho</p>
<p class=\"date\">Date: %T</p>
<p class=\"author\">Github:  <a href=\"https://github.com/itf/\">github.com/itf</a></p>
<p class=\"creator\">Made with %c and <a href=\"https://github.com/itf/org-export-head\">Org export head</a> </p>")

(setq org-export-head-tags-page "Tags") ; used for the tags to link to a page

(setq org-export-head-using-video-links t) ;helper function to add [[video:path]] links
(setq org-export-head-using-inline-js t) ;helper function to add inline-js to babel
(setq org-export-head-link-files-to-export (list "file" "video")) ;;Link types whose paths are gonna be copied with hard links, example[[video:path]]
;useful if using custom links



(setq org-export-head-click-toc-h2 t) ;; Make the header of TOC clickable, so you can write CSS for click to display

(setq org-export-head-resize-images t)
(setq org-export-head-resize-images-width 640)
(setq org-export-head-resize-images-height 640)
(setq org-export-head-resize-images-max-size 300000);0.3mb

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
           (org-export-head--replace-headline-macros macro-alist headlines-list headlines-hash)))
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
                  (if org-export-head-click-toc-h2
                      (cl-letf (((symbol-function 'org-html-toc) (symbol-function 'org-export-head--custom-toc)))
                        (org-html-export-to-html nil t)))))
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
             (url (org-export-head--escape org-export-head-tags-page))
             (tags-text-url "")
             (tags-text "")
             (tags-buttons-front "")
             (tags-buttons ""))
         (dolist (tag entry-tags)
           (unless (or (string= tag "reexport") (string= tag "noexport") (string= tag "noreexport"))
             (setq  tags-text-url (concat tags-text-url "[[file:"  url ".org::#" tag "][#"tag"]] "))  
             (setq  tags-text (concat tags-text tag " "))
             (setq tags-buttons-front (concat tags-buttons-front "@@html:<a href='#" tag "' class='" tag " tagbutton'>" tag "</a>@@ "))
             (setq tags-buttons (concat tags-buttons "@@html:<a href='" url ".html#" tag "' class='" tag " tagbutton'>" tag "</a>@@ "))))
         (org-set-property "TAGSURL" tags-text-url)
         (org-set-property "TAGSFRONT" tags-buttons-front)
         (org-set-property "TAGSBUTTONS" tags-buttons)
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


(defun org-export-head--replace-headline-macros(macro-alist headlines-list headlines-hash)
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



(defun org-export-head--wrap-in-div-class (text classes)
  "Wraps the text in a div with classes equal to the list of classes"
  (let ((classes-str (string-join classes " ")))
  (concat (org-export-head--div-class-start classes) text (org-export-head--div-class-end classes))))

(defun org-export-head--div-class-start (classes)
  "Wraps the text in a div with classes equal to the list of classes"
  (let ((classes-str (string-join classes " ")))
  (concat "@@html:<div class=\""classes-str" post\">@@" )))

(defun org-export-head--div-end ()
  "Wrapes the text in a div with classes equal to the list of classes"
  "@@html:</div>@@")

(defun org-export-head--generate-index-alist (headlines-list headlines-hash)
  "Geneates an org list with all the different index options of the website and inserts it in an alist"
  (let ((index "")
        (reverse-index "")
        (index-with-dates "")
        (index-with-summaries "")
        (all-tags "\n")
        (all-tags-reverse "\n")
        (all-tags-with-dates "\n")
        (all-tags-with-summaries "\n")
        (index-with-summaries-tags "")
        (index-with-tags "")
        (index-tags "")
        (tags ())
        (tags-indexes ())
        (tags-buttons ()))
    (cl-flet ((class-start(tags) (org-export-head--div-class-start tags))
              (class-end() (org-export-head--div-end))) 
      (dolist (headline-name headlines-list)
        (let* ((headline-alist (gethash headline-name headlines-hash nil))
               (entry-tags (assoc "ALLTAGS" headline-alist))
               (entry-tags (when entry-tags  (split-string (cdr entry-tags) ":")))
               (entry-tags (delete "" entry-tags))
               (entry-tags (delete "noexport" entry-tags))
               (entry-tags (delete "reexport" entry-tags))
               (entry-tags (delete "noreexport" entry-tags))
               (creation-date (cdr (assoc "CREATION-DATE" headline-alist)))
               (modification-date (cdr (assoc "MODIFICATION-DATE" headline-alist)))
               (summary (string-trim (cdr (assoc "SUMMARY" headline-alist))))
               (summary-html (concat "" (unless (= (length summary) 0) 
                                          (concat "   @@html:<br>@@" summary))))
               (tags-buttons (cdr (assoc "TAGSFRONT" headline-alist)))
               (headline-link (concat "- " (class-start entry-tags) " @@html:<b>@@[["headline-name"]["headline-name"]]@@html:</b>@@"))
               (index-entry (concat headline-link (class-end)))
               (date (concat "@@html:<span class=\"page-date\">@@"
                             " (" creation-date", updated " modification-date ")"
                             "@@html:</span>@@"  ))
               (index-entry-with-date (concat headline-link  date (class-end)))
               (index-entry-with-summary 
                (concat headline-link  date " " summary-html (class-end)))
               (index-entry-with-summary-tags
                (concat headline-link  date " " tags-buttons summary-html (class-end)))
               (index-entry-with-tags
                (concat headline-link  date " " tags-buttons (class-end))))
          (setq index (concat index index-entry "\n"))
          (setq reverse-index (concat index-entry "\n" reverse-index) )
          (setq index-with-dates (concat  index-with-dates index-entry-with-date "\n"))
          (setq index-with-summaries (concat  index-with-summaries index-entry-with-summary "\n"))
          (when entry-tags
            (setq index-with-summaries-tags (concat index-with-summaries-tags index-entry-with-summary-tags "\n"))
            (setq index-with-tags (concat index-with-tags index-entry-with-tags "\n")))
          
          (dolist (tag entry-tags)
            (if (not (member tag tags))
                (setq tags (cons tag tags)))
            (dolist (suffix '("" "-reverse" "-with-dates" "-with-summaries" "-with-summaries-tags" "-with-tags"))
              ;; Initialize tags lists
              (let ((tag-index-name (upcase (concat tag suffix))))
                (unless (assoc tag-index-name tags-indexes) 
                  (setq tags-indexes (cons `(,tag-index-name . "")  tags-indexes)))))

            ;;Add tag indexes to list
            (let* ((tag (upcase tag))
                   (tag-reverse (upcase (concat tag "-reverse")))
                   (tag-with-dates (upcase (concat tag "-with-dates")))
                   (tag-with-summaries (upcase (concat tag "-with-summaries")))
                   (tag-with-summaries-tags (upcase (concat tag "-with-summaries-tags")))
                   (tag-with-tags (upcase (concat tag "-with-tags")))
                   (tag-assoc (assoc tag tags-indexes))
                   (tag-assoc-reverse (assoc tag-reverse tags-indexes))
                   (tag-assoc-with-dates (assoc tag-with-dates tags-indexes))
                   (tag-assoc-with-summaries (assoc tag-with-summaries tags-indexes))
                   (tag-assoc-with-summaries-tags (assoc tag-with-summaries-tags tags-indexes))
                   (tag-assoc-with-tags (assoc tag-with-tags tags-indexes))
                   (tag-index (cdr tag-assoc))
                   (tag-index-reverse (cdr tag-assoc-reverse))
                   (tag-index-with-dates (cdr tag-assoc-with-dates))
                   (tag-index-with-summaries (cdr tag-assoc-with-summaries))
                   (tag-index-with-summaries-tags (cdr tag-assoc-with-summaries-tags))
                   (tag-index-with-tags (cdr tag-assoc-with-tags)))


              
              (setf (cdr tag-assoc) (concat tag-index index-entry "\n"))
              (setf (cdr tag-assoc-reverse) (concat index-entry "\n" tag-index-reverse ))
              (setf (cdr tag-assoc-with-dates) (concat tag-index-with-dates index-entry-with-date "\n"))
              (setf (cdr tag-assoc-with-summaries) (concat tag-index-with-summaries index-entry-with-summary "\n"))
              (setf (cdr tag-assoc-with-summaries-tags) (concat tag-index-with-summaries-tags index-entry-with-summary-tags "\n"))
              (setf (cdr tag-assoc-with-tags) (concat tag-index-with-tags index-entry-with-tags "\n"))
              (setf (cdr tag-assoc-with-summaries-tags) (concat tag-index-with-summaries-tags index-entry-with-summary-tags "\n"))))))
      
      ;; Now we create an index for all tags, to be able to have tag pages
      (setq tags (cl-sort tags 'string-lessp :key 'downcase)) ;first sort them
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
      

      ;;Kinda bad, generates the tag buttons for each tag. Ideally should be done only if requested
      (dolist (tag tags)
        (unless (or (string= tag "reexport") (string= tag "noexport") (string= tag "noreexport"))
          (setq tags-buttons (cons `(,(upcase (concat tag "-tags")) .  ,(org-export--get-tag-buttons tag))  tags-buttons))))
     

      (setq index-tags (org-export--get-tag-buttons))
      (append 
       (list (cons "INDEX" index) (cons "INDEX-TAGS" index-tags) (cons "INDEX-REVERSE" reverse-index)  (cons "INDEX-WITH-DATES" index-with-dates) (cons "INDEX-WITH-SUMMARIES" index-with-summaries) (cons "INDEX-WITH-SUMMARIES-TAGS" index-with-summaries-tags)
             (cons "INDEX-WITH-TAGS" index-with-tags)
             (cons "ALL-TAGS" all-tags) (cons "ALL-TAGS-REVERSE" all-tags-reverse)  (cons "ALL-TAGS-WITH-DATES" all-tags-with-dates) (cons "ALL-TAGS-WITH-SUMMARIES" all-tags-with-summaries))
       tags-indexes tags-buttons))))

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
  (when (and (member (org-element-property :type link) org-export-head-link-files-to-export)  (file-exists-p (org-element-property :path link)))
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
      
      
      ;;Create hard link folder
      (make-directory new-hard-link-directory t)
      ;;Create hard link, not replacing if it already exists, catching error if file does not exist
      (let ((new-file
             (condition-case nil
                 (add-name-to-file path new-hard-link-path nil)
               (error nil))))
        ;;Fix the AST
        ;;If image, remove description so it will become a real image instead of a link
        (if (not (member extension img-extensions))
            (apply #'org-element-adopt-elements link-copy link-description)
        (if org-export-head-resize-images
            (let ((new-description (org-export-head--resize-image new-hard-link-path org-export-head-resize-images-width org-export-head-resize-images-height org-export-head-resize-images-max-size)))
              (org-element-adopt-elements link-copy (concat "file:"(org-export-head--replace-in-string directory-path "" new-description))))
              ))
        (org-element-put-property link-copy :path new-relative-path)
        (org-element-set-element link  link-copy)
        
        new-file
        ))))

(defun org-export-head--replace-in-string (what with in)
"https://stackoverflow.com/a/17325791/5881930"
  (replace-regexp-in-string (regexp-quote what) with in nil 'literal))

(defun org-export-head--resize-image (path width height max-bytes) 
  "resizes path to width height, keeping proportions, if file larger than max bytes"
  (let* ((swidth (number-to-string width))
         (sheight (number-to-string height))
         (resized-path (concat (file-name-sans-extension path) swidth "x" sheight "." (file-name-extension path)))
         (size-bytes (file-attribute-size (file-attributes path))))
    (if (> size-bytes max-bytes)
        (progn (if (not (file-exists-p resized-path))
                   (call-process "convert" nil t nil path 
                                 "-resize"
                                 (concat  swidth "x" sheight">") 
                                 resized-path))
               resized-path)
      path)))

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
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(setq package-load-list '((htmlize t)))
(package-initialize)

(unless (package-installed-p 'htmlize)
  (package-refresh-contents)
  (package-install 'htmlize))
  (setq org-confirm-babel-evaluate nil)
  (org-mode)
  (transient-mark-mode) ;necessary for using org-map-entries
  (outline-show-all) 
  (org-export-head (concat directory-name "/") nil reexport)
  (save-buffer)))



;;Inpired by https://emacs.stackexchange.com/questions/51251/org-mode-html-export-table-of-contents-without-h2
;;from ox-html.el
(defun org-export-head--custom-toc(depth info &optional scope)
  (let ((toc-entries
	 (mapcar (lambda (headline)
		   (cons (org-html--format-toc-headline headline info)
			 (org-export-get-relative-level headline info)))
		 (org-export-collect-headlines info depth scope))))
    (when toc-entries
      (let ((toc (concat "<nav id=\"table-of-contents\">\n" 
                  "<input id=\"toggle-toc\" style=\"display: none; visibility: hidden;\" type=\"checkbox\">\n"
                  "<label for=\"toggle-toc\">\n <h2> <b> Table of Contents </b> </h2>\n </label>\n"
                  "<div id=\"text-table-of-contents\">"
			 (org-html--toc-text toc-entries)
			 "</div>\n"
                         "</nav> \n")))
        toc))))


;;by jkitcking https://stackoverflow.com/a/27527252/5881930
(defun org-export--get-tag-counts (&optional match)
  (let* ((all-tags '())
        (ignore (or match ""))
        (match (concat (or match "") "-noexport")))
    (org-map-entries
     (lambda ()
       (let ((tag-string (car (last (org-heading-components)))))
     (when tag-string   
       (setq all-tags
         (append all-tags (split-string tag-string ":" t)))))) match)
    ;; now get counts
    (remove nil (loop for tag in (remove-duplicates all-tags
                   :test (lambda (x y) (or (null y) (equal x y)))
                   :from-end t) 
                      collect 
                      (unless (or (string= tag "reexport") (string= tag "noexport") (string= tag "noreexport") (string= tag ignore))
                        (cons tag (cl-count tag all-tags :test 'string=)))))))


(defun org-export--downcase-car(x)
  (downcase (car x)))

(defun org-export--get-tag-buttons  (&optional match num)
  "Gets the tag buttons for the pages that match 'match'"
  (let* ((tags-counts (org-export--get-tag-counts match))
         (tags-counts (cl-sort tags-counts  'string-lessp :key 'org-export--downcase-car)) ;first sort them
        (tags-buttons ""))
    (dolist (tag-tupl tags-counts)
      (let ((tag (car tag-tupl))
            (tagN (number-to-string (cdr tag-tupl))))
        (if num
            (setq tags-buttons (concat tags-buttons "@@html:<a href='#" tag "' class='tagbutton tagindex " tag"'>" tag " "tagN  "</a>@@ "))
          (setq tags-buttons (concat tags-buttons "@@html:<a href='#" tag "' class='tagbutton tagindex " tag "'>" tag "</a>@@ ")))))
    tags-buttons))


(defun org-export-head-refile-subtree-at-point()
  (interactive)
  "Cuts the subtree leaving the header"
  (save-excursion
    (deactivate-mark t)
    (let* ((start (org-export-head--goto-header t))
           (end (org-end-of-subtree t))
          (buffer (current-buffer))
          (content (buffer-substring start end)))
      (let* ((file-name (read-file-name "Move subtree to file:" nil nil 'confirm (concat  (nth 4 (org-heading-components)) ".org"))))
        (with-temp-buffer
          (insert content)
          (write-file file-name))
        (let ((relative-file-name (file-relative-name file-name)))
          (insert (concat "#+INCLUDE \"" relative-file-name "\"\n"))
          (insert (concat "@@comment: [[file:" relative-file-name "]] @@"))
          (delete-region start  end)
      content)))))


;;; START utils
;;Add video links
(if org-export-head-using-video-links 
    (progn
      (defun org-export-head-export-video (path desc format)
        "Format video links for export."
        (cl-case format
          (html (concat "<video controls>
    <source src=\""path"\">
    Sorry, your browser doesn't support embedded videos.
</video>" ))
          (latex (format "\\href{%s}{%s}" path (or desc path)))
          (otherwise path)))
      (org-link-set-parameters "video" :export 'org-export-head-export-video)))

(if org-export-head-using-inline-js
    (progn
      (add-to-list 'org-src-lang-modes '("inline-js" . javascript)) ;; js2 if you're fancy
      (defvar org-babel-default-header-args:inline-js
        '((:results . "html")
          (:exports . "results")))
      (defun org-babel-execute:inline-js (body _params)
        (format "<script type=\"text/javascript\">\n%s\n</script>" body))))

(let ((file  (elt argv 0))
      (dir  (elt argv 1))
      (reexport  (elt argv 2)))
  (if (or (not file) (not dir))
      (message "usage  FILE DIR [export]")
    (message "Exportinf %s to %s" file dir)
    (org-export-head-other-file file dir reexport)))
