(add-to-list 'load-path "~/.emacs.d/lisp/")

;;
;; Auto fill mode
;; --------------
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Fill-column
;; -----------
(setq-default fill-column 78)

(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

(setq-default inhibit-splash-screen t)
(cua-mode t)

(setq-default indent-tabs-mode nil)

(setq c-default-style "linux"
	c-basic-offset 4)

(global-set-key [f5]  'compile)
(setq compile-command "make")

;; Duplicate line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)

(global-set-key (kbd "C-d") 'duplicate-line)

;;Windows style undo/redo
(require 'undo-tree)
(global-undo-tree-mode 1)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)

;;Switch between header and source files with F12
(global-set-key [f12] 'ff-find-other-file)

;;Automatic TAGS file update
(defadvice find-tag (around refresh-etags activate)
   "Rerun etags and reload tags if tag not found and redo find-tag.              
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
    ad-do-it
      (error (and (buffer-modified-p)
          (not (ding))
          (y-or-n-p "Buffer is modified, save it? ")
          (save-buffer))
         (er-refresh-etags extension)
         ad-do-it))))

(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently          
    (visit-tags-table default-directory nil)))

;; Make find-tag work same as as i-search
(defun looptag ()
  (interactive)
  (let ((current-prefix-arg 4))
    (call-interactively 'find-tag)
    )
  )

(global-unset-key (kbd "M-,"))
(global-set-key (kbd "M-,") 'looptag)

;;Disable backups
(setq make-backup-files nil)

(ido-mode 1)

;;Support for todo comments in the code
(require 'button-lock)
(require 'fixmee)
(global-fixmee-mode 1)
(global-set-key [f3] 'fixmee-goto-nextmost-urgent)
(global-set-key [f2] 'fixmee-goto-prevmost-urgent)
