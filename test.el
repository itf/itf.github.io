#! /bin/sh
":"; exec emacs --no-site-file --script "$0" "$@" # -*-emacs-lisp-*- 


(require 'org)
  (require 'ox)
  (require 'cl)
  (require 'subr-x)
(print (version))
(print (format "I did it. you passed in %s" command-line-args-left))
(print (org-version))
(require 'org)
(print org-latex-pdf-process)
(print org-latex-compiler)
(print org-format-latex-header)
(print org-latex-default-packages-alist)
;; Local Variables:
;; mode: emacs-lisp
;; End:
