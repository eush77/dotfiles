(add-to-list 'package-selected-packages 'sdcv)
(autoload 'sdcv-search-pointer+ "sdcv" nil t)
(autoload 'sdcv-search-input "sdcv" nil t)

(custom-set-variables '(sdcv-word-pronounce-command "true"))

(global-set-key (kbd "C-c C-s") #'sdcv-search-pointer+)
(global-set-key (kbd "C-c C-d") #'sdcv-search-input)
