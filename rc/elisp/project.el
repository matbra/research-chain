(require 'json)

(setq debug-on-error t)

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (if list
  (while list
    (message (car list))
    (setq list (cdr list)))
  (message "the list is empty.")))

(setq path-seperator '"/")
(setq settings-file-name '"settings.json")

;;(json-read-file '"../../settings.json")

;; function to find the path to the next (upwards in the directory tree)
;; settings file.
(defun find-next-settings-file ()
;;(interactive)
(setq cur-dir (buffer-file-name))

;;(message cur-dir)

(setq idx-sep  (length cur-dir))
;;(message (number-to-string cur-pos))
(setq b-stop-looping nil)
(while (and (> idx-sep 0) (not b-stop-looping))
  ;; find the last path seperator in the path string
  (setq idx-sep 0)
  (setq read-pos 0)
  
  (while (< read-pos (- (length cur-dir) 1))
    (if (string= (substring cur-dir read-pos (+ read-pos 1)) path-seperator)
	;(message "bang")
	(setq idx-sep read-pos)
;;      (message (number-to-string idx-sep))
      )

    (setq read-pos (1+ read-pos))
    ;(message (number-to-string read-pos))
    )

  ;;(message (concat "/ at " (number-to-string idx-sep)))
  ;;  (message cur-dir)
;;  (message (number-to-string idx-sep))
  (setq cur-pos idx-sep);; (length cur-dir))
  (setq cur-dir (substring cur-dir 0 (1+ idx-sep)))
  ;;(message cur-dir)
;;  (message "bla")
  ;;  (setq last-sep '(search-backward 
  ;;(print cur-pos)
  ;;  (message cur-pos)
  ;;  (setq cur-dir '(substring cur-dir 1 2))
;;  (message (number-to-string idx-sep))
  ;;(setq cur-pos (- cur-pos 1))
(setq candidate-path (concat cur-dir settings-file-name))
  (when (file-exists-p candidate-path)
      (setq b-stop-looping t)))

(if b-stop-looping cur-dir nil))
;      (return find-next-settings-file (candidate-path)) nil)))

(defun read-settings-file (filepath)
  (setq settings (json-read-file filepath)))

(defun print-alist (list)
;;(setq settings)
(while list
  (setq cur-element (car list))
  (setq cur-key (car cur-element))
  (setq cur-value (cdr cur-element))
  (message (format "%10s: %s" cur-key cur-value))
  (setq list (cdr list))
)
)

;;(setq settings ())
;;(load-next-settings-file)
;;(message (load-next-settings-file))
;;(message settings)

(message (find-next-settings-file))

;;(print-alist settings)
;;(print-alist settings)


;(print-alist (cons (assoc 'name settings) '()))

(defun find-all-inbox-entries ()
  ;; get the project directory
  ;;(setq settings (load-next-settings-file))
  (setq dir-project-root (find-next-settings-file))
  (setq dir-inbox (concat dir-project-root "inbox/"))
  (message dir-inbox)
  ;; traverse through the subdirectories
  (message (directory-files-and-attributes dir-inbox))
  )

;;(find-all-inbox-entries)

(defun find-files-and-directories (dirname)
  (let (cur-files '() all-files-and-dirs '())
  (setq cur-files (directory-files-and-attributes dirname))

  ; throw away the dot entries
  ; (hopefully always first and second)
  (setq cur-files (cdr (cdr cur-files)))

  ;; find directories in the current dir
  (let (new-directories '())

  (dolist (cur-file cur-files)
  (if (nth 1 cur-file) (add-to-list 'new-directories (nth 0 cur-file))))

  (if (not new-directories)
      (message "no directories found.")
    (progn
      (while new-directories
      (setq new-files-and-dirs (find-files-and-directories (concat dirname (car new-directories))))
      (dolist (new-file-and-dir new-files-and-dirs)
	(add-to-list 'all-files-and-dirs new-file-and-dir))
      (setq new-directories (cdr new-directories))))))

  ;; process the files in the current directory
  (setq new-files '())
  (dolist (cur-file cur-files)
    (if (not (nth 1 cur-file)) (add-to-list 'new-files (nth 0 cur-file))))

  (dolist (new-file new-files)
    (message (concat "path: " dirname ", file:" new-file)))
;    (add-to-list 'all-files-and-dirs (concat (file-name-as-directory dirname) new-file)))
;    (add-to-list 'all-files-and-dirs (concat 
    (add-to-list 'all-files-and-dirs `((dir . ,dirname) (file . ,new-files)))

;(message "stored everything")


;    (message all-files-and-dirs)
  all-files-and-dirs))





(setq dir-project-root (find-next-settings-file))
(setq dir-inbox (concat dir-project-root "inbox/"))
;(print-elements-of-list (find-files-and-directories dir-inbox))
(setq dirs-and-files (find-files-and-directories dir-inbox))

;(print-elements-of-list (cdr (car dirs-and-files)))





