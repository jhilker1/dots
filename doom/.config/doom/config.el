(setq doom-font (font-spec :name "Josevka Code" :size 16))

(after! org
  (add-to-list 'org-babel-default-header-args
               '((:noweb . "yes")
                 (:mkdirp . "yes"))))
