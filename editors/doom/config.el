;(setq doom-font (font-spec :name "Josevka Code" :size 16))

(after! org-roam
  (setq org-roam-directory "/mnt/c/Users/camoh/Dropbox/roam"
        org-roam-db-location "~/.org-roam.db"
        org-roam-completion-everywhere t)
  (cl-defmethod org-roam-node-namespace ((node org-roam-node))
    "Return the currently set namespace for the NODE."
    (let ((namespace (cdr (assoc-string "NAMESPACE" (org-roam-node-properties node)))))
      (if (string= namespace (file-name-base (org-roam-node-file node)))
          "" ; or return the current title, e.g. (org-roam-node-title node)
        (format "%s" namespace))))
    (setq org-roam-node-display-template (concat "${namespace:15} ${title:*}" (propertize "${doom-tags:50}" 'face 'org-tag))))
