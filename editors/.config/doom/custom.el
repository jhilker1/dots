(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((eval add-hook 'after-save-hook
           (lambda nil
             (copy-file buffer-file-name "~/Devel/sites/jhilker.gitlab.io/content-org/posts/" t)))
     (eval add-hook 'after-save-hook
           (lambda nil
             (copy-file buffer-file-name "~/Projects/sites/jhilker.gitlab.io/content-org/posts/" t))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:slant italic)))))
