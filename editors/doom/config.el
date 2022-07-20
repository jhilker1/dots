(after! hydra-posframe
  (hydra-posframe-mode t))

(use-package! lsp-tailwindcss
  :init
  (setq lsp-tailwindcss-add-on-mode t))

(setq user-full-name "Jacob Hilker"
      user-mail-address "jacob.hilker2@gmail.com")

(when (featurep! emoji)
	(emojify-download-emoji))

(setq doom-theme 'doom-gruvbox
      ;; doom-theme 'doom-nord ;; 20242C
      doom-font (font-spec :name "Josevka" :size 17)
      doom-unicode-font (font-spec :name "Josevka")
      doom-variable-pitch-font (font-spec :name "Josevka Book Sans" :size 17))

(set-face-attribute 'default nil :background "#1d2021")
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic))

(after! hydra-posframe
  (hydra-posframe-mode t))

(setq org-directory "~/org")

(after! org
  (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "CANC(c)"))
        org-todo-keyword-faces '(("TODO" . (:foreground "#fb4934" :underline t))
                                 ("NEXT" . (:foreground "#fe8019")))
        org-agenda-files '("gtd/inbox.org" "gtd/orgzly.org" "gtd/todo.org" "gtd/gcal.org")

        org-agenda-start-day nil ;; today
        org-ellipsis "▾"))

(defun jh/org-ui-hook ()
  (variable-pitch-mode 1)
  ;(setq display-line-numbers-type 'nil)
  (setq display-line-numbers nil)
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-title nil :font (font-spec :family "Josevka Book Slab" :size 22) :weight 'bold))

(add-hook! 'org-mode-hook #'jh/org-ui-hook)

(after! org-super-agenda
  (add-hook! 'org-mode-hook #'org-super-agenda-mode))

(after! org-recur
  (add-hook! 'org-mode-hook #'org-recur-mode)
  (add-hook! 'org-agenda-mode-hook #'org-recur-agenda-mode))

(after! doct
  (setq org-capture-templates
            (doct '(("Inbox" :keys "i"
                     :file "~/Dropbox/org/gtd/inbox.org"
                     :template "* TODO %^{TODO Item}"
                     :immediate-finish t)))))

(after! org-roam
  (setq org-roam-directory "~/roam/"
        org-roam-db-location "~/.org-roam.db"
        org-roam-completion-everywhere t)
  (cl-defmethod org-roam-node-namespace ((node org-roam-node))
    "Return the currently set namespace for the NODE."
    (let ((namespace (cdr (assoc-string "NAMESPACE" (org-roam-node-properties node)))))
      (if (string= namespace (file-name-base (org-roam-node-file node)))
          "" ; or return the current title, e.g. (org-roam-node-title node)
        (format "%s" namespace))))

  (setq org-roam-node-display-template (concat "${namespace:15} ${title:*}" (propertize "${doom-tags:50}" 'face 'org-tag))))

(after! vulpea
  (add-hook! 'org-roam-db-autosync-mode #'vulpea-db-autosync-enable))

(defvar jh/org-roam-project-alist nil
    "An alist containing my projects for org-roam.")

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;;         a hookable mode anymore, you're advised to pick something yourself
;;         if you don't care about startup time, use
;;  :hook (after-init . org-roam-ui-mode)
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(after! ox-hugo
  (setq org-hugo-front-matter-format "yaml"
        org-hugo-section "blog"
        org-hugo-paired-shortcodes "cventry mermaid warning"
        org-hugo-auto-set-lastmod t
        org-hugo-suppress-lastmod-period 86400
        org-hugo-special-block-type-properties '(("audio" :raw t)
                                                 ("katex" :raw t)
                                                 ("mark" :trim-pre t :trim-post t)
                                                 ("tikzjax" :raw t)
                                                 ("video" :raw t)
                                                 ("mermaid" :raw t)))
  (add-to-list 'org-export-global-macros '(("srcstart" . "@@hugo:<details><summary class=\"font-bold underline\">$1</summary>@@")
                                           ("srcend" . "@@hugo:</details>@@"))))

(use-package! ox-moderncv)
(use-package! ox-hugocv)
