(add-to-list 'load-path "~/.emacs.d/lisp/")

(tool-bar-mode 0)
(menu-bar-mode 0)
;;
;; Auto fill mode
;; --------------
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Fill-column
;; -----------
(setq-default fill-column 78)

(setq-default inhibit-splash-screen t)
(cua-mode t)

(setq-default indent-tabs-mode nil)

(setq c-default-style "linux"
	c-basic-offset 8)

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

;;Remove trailing whitespace on c mode during file write
(add-hook 'c-mode-hook
          (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

(global-set-key (kbd "C-x d") 'duplicate-line)

;;Switch between header and source files with F12
(global-set-key [f12] 'ff-find-other-file)

;;Automatic TAGS file update
(defadvice xref-find-definitions (around refresh-etags activate)
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
    (call-interactively 'xref-find-definitions)
    )
  )

(global-unset-key (kbd "M-q"))
(global-set-key (kbd "M-q") 'looptag)

;;Disable backups
(setq make-backup-files nil)
;;Disable autosave
(setq auto-save-default nil)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cua-mode t nil (cua-base))
 '(custom-safe-themes
   '("a81bc918eceaee124247648fc9682caddd713897d7fd1398856a5b61a592cb62" default))
 '(gdb-many-windows t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp) (comp))))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(require 'mu4e)
(setq
 mu4e-headers-skip-duplicates  t
 mu4e-view-show-images t
 mu4e-view-show-addresses t
 mu4e-compose-format-flowed nil
 mu4e-date-format "%y/%m/%d"
 mu4e-headers-date-format "%Y/%m/%d"
 mu4e-change-filenames-when-moving t
 mu4e-attachments-dir "~/Downloads"
 mu4e-maildir       "~/Maildir"   ;; top-level Maildir
 ;; note that these folders below must start with /
 ;; the paths are relative to maildir root
 mu4e-refile-folder "/Archive"
 mu4e-sent-folder   "/Sent Items"
 mu4e-drafts-folder "/Drafts"
 mu4e-trash-folder  "/Trash")

(setq mu4e-alert-interesting-mail-query
      (concat
       "flag:unread"
       " AND NOT flag:trashed"
       " AND NOT maildir:"
       "\"/Junk Mail\""))

 ;; Re-index every 5 minutes.
(setq mu4e-update-interval 300)
(setq mu4e-user-mail-address-list '("niko@byteptr.com"))

(setq user-mail-address "niko@byteptr.com")

;; this setting allows to re-sync and re-index mail
;; by pressing U
(setq mu4e-get-mail-command  "mbsync -a")
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "msmtp")

(add-to-list 'mu4e-bookmarks
  '( :name  "Important messages"
     :query "flag:flagged"
     :key   ?f))

(global-set-key [f9] 'mu4e)
(global-set-key [f8] 'mu4e-headers-toggle-full-search)


 (defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(define-skeleton article-template
  "Insert my article template into an active buffer."
  "Article title: "
  "<!doctype html>
<html lang=\"en\">
    <head>
        <meta charset=\"utf-8\">
        <title>N.V Rosvall's weblog:" str "</title>
        <link rel=\"icon\" type=\"image/png\" href=\"favicon.png\">
        <link rel=\"stylesheet\" href=\"../style.css\">
        <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no\" />
    </head>
    <body>
        <div id=\"content\">
            <h1>" str "</h1>
            <p><i>Last updated: " (insert-current-date)  "</i></p>
            <p>Comments on <a href=\"https://mastodon.social/@nvrosvall\">Mastodon</a></p>
            <p>All articles: <a href=\"index.html\">Index</a></p>

            <hr>
            Copyright &copy; Niko Rosvall
        </div>
    </body>
</html>")

(global-set-key (kbd "\C-ca") 'article-template)
