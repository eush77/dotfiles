(defun my-bookmark-all-names--hide-org-bookmarks (names)
  "Remove Org bookmarks from the list.

See `org-bookmark-names-plist'."
  (require 'org)
  (let ((org-bookmark-names
         (mapcar #'cadr
                 (seq-partition (symbol-value 'org-bookmark-names-plist)
                                2))))
    (seq-difference names org-bookmark-names)))
(advice-add 'bookmark-all-names
            :filter-return #'my-bookmark-all-names--hide-org-bookmarks)
