(require 'json)

(setq path-seperator '"/")
(setq settings-file-name '"settings.json")

;;(json-read-file '"../../settings.json")

;; function to find the path to the next (upwards in the directory tree)
;; settings file.
(defun load-next-settings-file ()
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
  (if (file-exists-p candidate-path)

(setq settings (json-read-file candidate-path))
;;(message "bla")
;;(message candidate-path)
;;(message "bla")
;;(setq b-stop-looping t)

;;(message candidate-path)
;;(message candidate-path)
;;(message "bla")
;;(message "bla")

;;(message (file-exists-p candidate-path))
)
)
)

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
(load-next-settings-file)
;;(message (load-next-settings-file))
;;(message settings)

;;(print-alist settings)
(print-alist settings)


(print-alist (cons (assoc 'name settings) '()))

