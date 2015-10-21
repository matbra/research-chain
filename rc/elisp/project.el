;; for the menu i learned quite a lot from buff-menu.el
;; (http://www.mit.edu/afs.new/athena/astaff/source/src-8.2/third/emacs/lisp/buff-menu.el)

(require 'json)
(require 'widget)

(setq path-seperator '"/")
(setq settings-file-name '"settings.json")

;; setup the keymap
(setq rc-menu-mode-map (make-keymap))
(suppress-keymap rc-menu-mode-map t)
(define-key rc-menu-mode-map "q" 'rc-menu-quit)
(define-key rc-menu-mode-map "\C-m" 'rc-menu-entry)


(defun insert-org-image (filename)
  (insert (concat "#+CAPTION: " filename "\n"))
  (insert (concat "[[" filename "]]\n")))


(defun rc-menu-quit ()
  "Quit the menu."
  (interactive)
  (let ((buffer (current-buffer)))
    ;; Switch away from the buffer menu and bury it.
    (switch-to-buffer (other-buffer))
    (bury-buffer buffer)))


(defun rc-menu-entry ()
  "Select this line's buffer in this window."
  (interactive)
  (setq filename-to-insert (current-menu-entry))
  (message filename-to-insert)
  (switch-to-buffer (other-buffer))
  (insert-org-image filename-to-insert))


(defun current-menu-entry ()
  "Return buffer described by this line of buffer menu."
  (get-file-at-line (line-number-at-pos)))


;; simply prints a list
(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (if list
  (while list
    (message (car list))
    (setq list (cdr list)))
  (message "the list is empty.")))


;; function to find the path to the next (upwards in the directory tree)
;; settings file.
(defun find-next-settings-file ()
  (setq cur-dir (buffer-file-name))

  (setq idx-sep (length cur-dir))
  (setq b-stop-looping nil)
  (while (and (> idx-sep 0) (not b-stop-looping))
    ;; find the last path seperator in the path string
    (setq idx-sep 0)
    (setq read-pos 0)
    
    (while (< read-pos (- (length cur-dir) 1))
      (if (string= (substring cur-dir read-pos (+ read-pos 1)) path-seperator)
	  (setq idx-sep read-pos))

      (setq read-pos (1+ read-pos)))

    (setq cur-pos idx-sep)
    (setq cur-dir (substring cur-dir 0 (1+ idx-sep)))
    (setq candidate-path (concat cur-dir settings-file-name))
    (when (file-exists-p candidate-path)
      (setq b-stop-looping t)))

  (if b-stop-looping cur-dir nil))


(defun read-settings-file (filepath)
  (setq settings (json-read-file filepath)))

(defun print-alist (list)
  (while list
    (setq cur-element (car list))
    (setq cur-key (car cur-element))
    (setq cur-value (cdr cur-element))
    (message (format "%10s: %s" cur-key cur-value))
    (setq list (cdr list))))


(defun find-all-inbox-entries ()
  ;; get the project directory
  (setq dir-project-root (find-next-settings-file))
  (setq dir-inbox (concat dir-project-root "inbox/"))
  (message dir-inbox)

  ;; traverse through the subdirectories
  (message (directory-files-and-attributes dir-inbox)))


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

;  (if (not new-directories)
      ;(message "no directories found.")
  (if new-directories
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

;  (dolist (new-file new-files)
;    (message (concat "path: " dirname ", file:" new-file)))
    (add-to-list 'all-files-and-dirs `((dir . ,dirname) (file . ,new-files)))

  all-files-and-dirs))

;(setq dir-project-root (find-next-settings-file))
;(setq dir-inbox (concat dir-project-root "inbox/"))
;(setq dirs-and-files (find-files-and-directories dir-inbox))


(defun create-file-selection-form (files-and-dirs)
  (switch-to-buffer "*Inbox Selection*")
  (kill-all-local-variables)
  (setq buffer-read-only nil)
  (setq-local file-table '())
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)

  ;; write the names of all directories and files into the buffer
  (while files-and-dirs
    (setq cur-entry (car files-and-dirs))
    (message (cdr (assoc 'dir cur-entry)))
    (setq cur-dirname (cdr (assoc 'dir cur-entry)))
    (setq cur-files (cdr (assoc 'file cur-entry)))

    (insert cur-dirname)
    (insert "\n")
    (insert (make-string 20 ?-))
    (insert "\n")

    ;; add the files to the current dir
    (while cur-files
      (setq cur-file (car cur-files))
      (insert cur-file)

      ; append the current file to the file table
      ;(message (line-number-at-pos))
      (setq file-table (cons `(,(line-number-at-pos) ,(concat (file-name-as-directory cur-dirname) cur-file)) file-table))
;      (message file-table)
;      (print-elements-of-list file-table) ;

      (setq cur-files (cdr cur-files))

      (if cur-files
	  (insert "\n")))

    (setq files-and-dirs (cdr files-and-dirs))
    (if files-and-dirs
	(insert (make-string 2 ?\n))))

  (setq buffer-read-only t)

  (interactive "p")

;  (message file-table)
  (use-local-map rc-menu-mode-map))

(defun insert-from-inbox ()
  (interactive)
  (setq dir-inbox (file-name-as-directory (concat (file-name-as-directory (find-next-settings-file)) "inbox")))
  (create-file-selection-form (find-files-and-directories dir-inbox)))

(defun get-file-at-line (line)
  (car (cdr (assoc line file-table))))

