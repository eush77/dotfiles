(my-install-packages 'sdcv)
(require 'sdcv)

(custom-set sdcv-word-pronounce-command "true")

(global-set-key (kbd "C-c C-s") #'sdcv-search-pointer+)
(global-set-key (kbd "C-c C-d") #'sdcv-search-input)
