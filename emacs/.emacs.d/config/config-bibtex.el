(require 'dash)

(add-hook 'bibtex-mode-hook #'hs-minor-mode)

;;; BibLaTeX Dialect

(defvar my-bibtex-biblatex-entry-type-aliases
  '(("Article"
     ("Review" "Review of Other Work"))
    ("Online"
     ("Electronic" "Online Resource")
     ("WWW" "Online Resource"))
    ("InProceedings"
     ("Conference" "Article in Conference Proceedings"))
    ("Misc"
     ("Artwork" "Work of the Visual Arts")
     ("Audio" "Audio Recording")
     ("Commentary" "Commentary")
     ("Image" "Image, Picture, or Photograph")
     ("Jurisdiction" "Court Decision or Recording")
     ("Legislation" "Law, Bill, or Legislative Proposal")
     ("Legal" "Legal Document")
     ("Letter" "Personal Correspondence")
     ("Movie" "Motion Picture")
     ("Music" "Musical Recording")
     ("Performance" "Work of the Performing Arts")
     ("Software" "Computer Software")
     ("Standard" "National or International Standard")
     ("Video" "Audiovisual Recording")))
  "Entry type aliases specified in BibLaTeX.

Each entry is a pair (ALIASEE . ALIASES) where ALIASEE is the
standard entry name, and ALIASES a list of extra names with
descriptions.")

(defvar my-bibtex-biblatex-entry-type-special-aliases
  '(("Thesis"
     ("MastersThesis" "Master's Thesis")
     ("PhdThesis" "PhD Thesis"))
    ("Report"
     ("TechReport" "Technical Report")))
  "Special entry type aliases specified in BibLaTeX.

Like `my-bibtex-biblatex-entry-type-aliases', except that in the
type alias \"type\" field becomes optional.")

(defvar my-bibtex-biblatex-field-type-aliases
  '(("annotation" "annote")
    ("eprintclass" "primaryclass")
    ("eprinttype" "archiveprefix")
    ("file" "pdf")
    ("institution" "school")
    ("location" "address")
    ("journaltitle" "journal")
    ("sortkey" "key"))
  "Field type aliases specified in BibLaTeX.

Each entry is a pair (ALIASEE . ALIASES) where ALIASEE is the
standard field name, and ALIASES a list of extra names.")

(defun my-bibtex-biblatex-amend-field-group (group)
  "Transform field GROUP and return a new modified group."
  (--mapcat
   (cons it
         (seq-map #'list
                  (cdr (assoc (car it) my-bibtex-biblatex-field-type-aliases))))
   group))

;; Add entry type aliases.
(pcase-dolist (`(,aliasee . ,aliases) my-bibtex-biblatex-entry-type-aliases)
  (dolist (alias aliases)
    (add-to-list 'bibtex-biblatex-entry-alist
                 (append alias
                         (cddr (assoc aliasee bibtex-biblatex-entry-alist))))))

;; Add special entry type aliases.
(pcase-dolist (`(,aliasee . ,aliases)
               my-bibtex-biblatex-entry-type-special-aliases)
  (pcase-let ((`(_ _ ,required nil ,optional)
               (assoc aliasee bibtex-biblatex-entry-alist)))
    (dolist (alias aliases)
      (add-to-list 'bibtex-biblatex-entry-alist
                   (append alias
                           (list (remove '("type") required)
                                 nil
                                 (cons '("type") optional)))))))

;; Add field type aliases.
(setq bibtex-biblatex-entry-alist
      (--map (--map (if (listp it)
                        (my-bibtex-biblatex-amend-field-group it) it)
                    it)
             bibtex-biblatex-entry-alist))

(bibtex-set-dialect 'biblatex)

;;; Reformat setup

(custom-set-variables
 '(bibtex-align-at-equal-sign t)
 '(bibtex-comma-after-last-field t)
 '(bibtex-entry-format t))

;;; Keymap

(define-key bibtex-mode-map [remap indent-region] #'bibtex-reformat)
