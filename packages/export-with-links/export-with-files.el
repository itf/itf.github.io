(require 'ox)


(defun export-with-files-export-parent  (&optional directory-name)
  (interactive)
  (save-excursion
    (outline-up-heading 1)
    (export-with-files-export  directory-name)))


(defun export-with-files-export (&optional directory-name)
  (interactive)
  (let ((directory-name (or directory-name (read-directory-name "Directory:")))
        (export-latex-header (org-entry-get (point) "export_latex_header" t))
        (export-author (org-entry-get (point) "export_author" t))
        (export-options (org-entry-get (point) "export_options" t)))
    (make-directory directory-name t)
    (widen)
    (org-narrow-to-subtree)
    
    ;;Create copy of the 
    (org-export-with-buffer-copy
     (let* ((ast (org-element-parse-buffer)))
       (org-element-map ast 'link
         (lambda (link)
           (export-with-files--fix-file-external-link-ast directory-name link)))
       
       
       ;;Convert the buffer to contain the new AST, 
        ;;this is needed because the exporter expects the content to be in a buffer
       (erase-buffer) 
       (insert (org-element-interpret-data ast))
       
       (outline-show-all)
       (goto-char (point-min))
       (let* ((file-name  (export-with-files--escaped-headline))
              (new-file-name (concat directory-name file-name)))
         ;; Make the buffer file be in the new directory, because
         ;; org-latex-export-to-pdf always export to the working directory of the buffer
         (set-visited-file-name (concat new-file-name ".org"))

         ;; Set export-otions of the parents
         (if export-options
             (org-set-property
              "EXPORT_OPTIONS"
              export-options))
         (if export-author
             (org-set-property
              "EXPORT_AUTHOR"
              export-author))
         (if export-latex-header
             (org-set-property
              "EXPORT_LATEX_HEADER"
              export-latex-header))
         
         ;; Name of the tex file / pdf file
         (org-set-property
          "EXPORT_FILE_NAME"
          file-name)
;;         (deactivate-mark)
        (org-latex-export-to-pdf nil t)))))
  (widen))
     


(defun export-with-files--fix-file-external-link-ast (directory-path link)
  "Creates hard links to the external files in the output directory"
  (when (string= (org-element-property :type link) "file")
    (let* ((path (org-element-property :path link))
           (path (dnd-unescape-uri path))
           (extension (file-name-extension path))
           (link-copy (org-element-copy link))
           (img-extensions '("jpg" "tiff" "png" "bmp"))
           (link-description (org-element-contents link))
           ;; Put files in subdirectories with the extension of the file
           (new-relative-path 
            (concat "./" extension "/" (file-name-nondirectory path)))
           (new-hard-link-path (concat directory-path new-relative-path))
           (new-hard-link-directory (file-name-directory new-hard-link-path)))
      
      ;;Fix the AST
      ;;If image, remove description so it will become a real image instead of a link
     (unless (or (member extension img-extensions) (not link-description))
      (apply #'org-element-adopt-elements link-copy link-description))
     (org-element-put-property link-copy :path new-relative-path)
     (org-element-set-element link  link-copy)
      
      ;;Create hard link folder
      (make-directory new-hard-link-directory t)
      ;;Create hard link, not replacing if it already exists, catching error if file does not exist
      (condition-case nil
          (add-name-to-file path new-hard-link-path nil)
        (error nil)))))



(defun export-with-files--escaped-headline ()
  (export-with-files--escape
   (nth 4 (org-heading-components))))

(defun export-with-files--escape(text)
  (replace-regexp-in-string "[\\?.,!:]" ""
   (replace-regexp-in-string "/" "-" 
    (replace-regexp-in-string " " "_"  text))))
