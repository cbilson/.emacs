(require 'package)

(require 'my-defuns)
(my-install-packages
 '(ace-isearch ace-jump-mode ace-jump-buffer ace-window acutex ag
	       browse-kill-ring
	       cider clojure-mode
	       coffee-mode csharp-mode csv-mode ctags ctags-update
	       dash dired-details dired+
	       easy-kill editorconfig elein elisp-slime-nav
	       emmet-mode ess expand-region
	       feature-mode find-file-in-project flymake
	       geiser gist gitconfig-mode gitignore-mode
	       git-timemachine god-mode goto-chg
	       htmlize
	       idle-highlight-mode ido-ubiquitous iedit
	       js-comint
	       magit markdown-mode multiple-cursors
	       omnisharp org org-magit
	       page-break-lines paren-face pp-c-l
	       rbenv
	       s scpaste slime smartparens smex smooth-scrolling
	       twittering-mode
	       undo-tree
	       wrap-region writegood-mode
	       yaml-mode yasnippet))

;;; auto-modes
(autoload 'powershell-mode "powershell-mode")

(setq auto-mode-alist
      (append auto-mode-alist
              '(("\\.asm$" . nasm-mode)
                ("\\.aspx$" . html-mode)
                ("\\.bat$" . cmd-mode)
                ("\\.cljs$" . clojurescript-mode)
                ("\\.cmd$" . cmd-mode)
                ("\\.config$" . xml-mode)
                ("\\.cs$" . csharp-mode)
                ("\\.csx$" . csharp-mode)
                ("\\.cshtml$" . html-mode)
                ("\\.csman$" . xml-mode)
                ("\\.csproj$" . xml-mode)
                ("\\.fs$" . fsharp-mode)
                ("\\.fsx$" . fsharp-mode)
                ("\\.fsproj$" . xml-mode)
                ("\\.targets$" . xml-mode)
                ("\\.ps1xml$" . xml-mode)
                ("\\.psd1" . powershell-mode)
                ("\\.props$" . xml-mode)
                ("\\.proj$" . xml-mode)
                ("\\.rd$" . xml-mode)
                ("\\.rdsc$" . xml-mode)
                ("\\.rels$" . xml-mode)
                ("\\.m$" . octave-mode)
                ("\\.ps1$" . powershell-mode)
                ("\\.psm1$" . powershell-mode)
                ("\\.R$" . r-mode)
                ("\\.r$" . r-mode)
                ("\\.spark$" . html-mode))))

;;; settings

(require 'recentf)
(require 'uniquify)

(when (package-installed-p 'smooth-scrolling)
  (require 'smooth-scrolling))

(delete-selection-mode +1)
(recentf-mode +1)
(ido-mode)

(require 'yasnippet)
(setq yas-snippet-dirs (file-name-as-directory (concat my-emacs-dir "snippets")))
(yas-global-mode +1)

(setq user-full-name "Chris Bilson")
(setq user-mail-address "cbilson@pobox.com")

(defalias 'yes-or-no-p 'y-or-n-p)

(defvar backups-dir (concat user-emacs-directory "backups"))

(setq apropos-do-all t
      backup-by-copying t
      backup-directory-alist
      `(("." . ,(file-name-as-directory backups-dir)))
      c-basic-offset 2
      column-number-mode t
      delete-old-versions t
      delete-selection-mode t
      echo-keystrokes 0.1
      ediff-window-setup-function 'ediff-setup-windows-plain
      ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      indent-tabs-mode nil
      indicate-empty-lines t
      inhibit-splash-screen t
      kept-new-versions 6
      kept-old-versions 2
      make-backup-files t
      menu-bar-mode nil
      mouse-yank-at-point t
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ;; mouse-wheel-progressive-speed nil
      ;; mouse-wheel-follow-mouse t
      recentf-max-menu-items 15
      recentf-max-saved-items 200
      require-final-newline t
      save-interprogram-paste-before-kill t
      save-place t
      save-place-file (concat user-emacs-directory "places")
      scroll-bar-mode -1
      scroll-step 3
      tab-width 2
      transient-mark-mode t
      uniquify-buffer-name-style 'forward
      use-dialog-box nil
      version-control t
      visible-bell t
      whitespace-style '(face trailing lines-tail tabs)
      x-select-enable-clipboard t
      x-select-enable-primary t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))))

(tool-bar-mode -1)

;; (add-to-list ido-ignore-buffers 'whatever)
;; (add-to-list ido-ignore-directories 'whatever)
;; (add-to-list ido-ignore-files 'whatever)

;; auto-revert
(global-auto-revert-mode +1)
(setq global-auto-revert-non-file-buffers +1
      auto-revert-verbose nil)

(setq-default ispell-program-name "aspell"
              save-place t)

(set-face-foreground 'vertical-border "white")

(when (package-installed-p 'github-theme)
  (require 'github-theme))

(when (package-installed-p 'flymake)
  (require 'flymake))

(when (package-installed-p 'smex)
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(when (package-installed-p 'ace-isearch)
  (global-ace-isearch-mode +1))

;;; Remembering where I was
(savehist-mode t)

;; this looks gross in low-color mode
(when (<= (display-color-cells) 8)
  (defun hl-line-mode () (interactive)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; (setq display-time-world-list
;;       '(("Australia/Sydney" "SY")
;;         ("Asia/Tokyo" "TY")
;;      ("Asia/Singapore" "SG")
;;         ("Europe/Amsterdam" "AM")
;;         ("Europe/Ireland" "DB")
;;         ("America/New_York" "BN")
;;         ("America/Dallas" "SN"
;;       ("America/Los_Angeles" "BY"))))

(when (package-installed-p 'undo-tree)
  (undo-tree-mode +1)

  ;; these are just the standard undo-tree keys, but I keep them here
  ;; for reference
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "M-_") 'undo-tree-redo)
  (global-set-key (kbd "C-x u") 'undo-tree-visualize))

(when (package-installed-p 'multiple-cursors)
  (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c m c") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c m f") 'mc/mark-all-like-this-in-defun)
  (global-set-key (kbd "C-c m l") 'mc/edit-lines)
  (global-set-key (kbd "C-c m m") 'mc/mark-all-dwim))

;;; bindings

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-;") 'comment-dwim)

(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))

(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x O") (lambda () (interactive) (other-window -1)))

(global-set-key (kbd "C-x C-m") 'shell)

(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)

(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'duplicate-start-of-line-or-region)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c l") 'org-store-link)
;; C-c m: Prefix for multiple-cursors
(global-set-key (kbd "C-c n") 'my-cleanup-buffer)
;; C-c o: Prefx for omnisharp
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-c C-j") 'cider-jack-in)
(global-set-key (kbd "C-c C-k") 'compile)

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(winner-mode)
(global-set-key (kbd "C-c w") 'winner-undo)

(global-set-key (kbd "H-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "H-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "H-<up>") 'enlarge-window)
(global-set-key (kbd "H-<down>") 'shrink-window)

(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(package-installed-p 'smartparens)
(require 'smartparens-config)
(smartparens-global-mode +1)
(show-smartparens-global-mode +1)
(cl-concatenate 'list sp-ignore-modes-list
                '())

(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)

(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)
(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

  (sp-with-modes '(markdown-mode gfm-mode rst-mode)
    (sp-local-pair "*" "*" :bind "C-*")
    (sp-local-tag "2" "**" "**")
    (sp-local-tag "s" "```scheme" "```")
    (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

  (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
    (sp-local-tag "i" "\"<" "\">"))

  (sp-with-modes '(html-mode sgml-mode)
    (sp-local-pair "<" ">"))

  (sp-with-modes sp--lisp-modes
    (sp-local-pair "(" nil :bind "C-(")))

;;; god-mode

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(eval-after-load 'god-mode
  '(progn
     (define-key god-local-mode-map (kbd ".") 'repeat)
     (define-key god-local-mode-map (kbd "i") 'god-local-mode)))

(dolist '(dired-mode)
  (lambda (x) (add-to-list 'god-expempt-major-modes x)))

;;; expand-region
(when (package-installed-p 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

;;; programming

(add-hook 'prog-mode-hook 'my-kill-word-key)

(eval-after-load 'paredit
  ;; need a binding that works in the terminal
  '(progn
     (define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp)
     (define-key paredit-mode-map (kbd "M-(") 'paredit-backward-slurp-sexp)))

;;; eshell

(defun eshell/rgrep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (eshell-grep "rgrep" args t))

(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(when (package-installed-p 'idle-highlight)
  (add-hook 'prog-mode-hook 'idle-highlight))

(setq lisp-modes
      '(lisp-mode emacs-lisp-mode common-lisp-mode scheme-mode clojure-mode))

(defvar lisp-mode-map (make-key-map))

(defun cbilson/lisp-mode-hook ()
  (smartparens-strict-mode t)
  (paren-face-mode))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'cbilson/lisp-mode-hook))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")

(add-hook 'cider-connected-hook 'my-clojure-mode-eldoc-hook)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;;; c-mode
(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map (kbd "C-c C-k") 'compile)))

;;; org-mode
(setq org-agenda-show-log t
      initial-scratch-message nil
      initial-major-mode 'org-mode
      ;; org-agenda-files (list (concat my-emacs-dir ))
      org-confirm-babel-evaluate nil
      org-default-notes-file (concat my-emacs-dir "Capture.org")
      org-ditaa-jar-path (concat my-emacs-dir "vendor/ditaa0_9.jar")
      org-log-done t
      org-plantuml-jar-path (concat my-emacs-dir "vendor/plantuml.jar")
      org-source-fontify-natively t
      org-todo-keywords '((sequence "TODO" "WIP" "DONE"))
      org-todo-keyword-faces '(("WIP" . (:foreground "blue" :weight bold))))

(setq org-capture-templates
      '(("I" "Incident" entry
         (file+headline "~/OneDrive/Emacs/Work.org" "Incidents")
         "* RDIncident %?\n Entered on %U\n %i")
        ("i" "Interesting" entry
         (file+datetree (concat my-emacs-dir "Capture.org"))
         "* %?\n %i\n %a")
        ("k" "Knowledge Base" entry
         (file+headline (concat my-emacs-dir "Work.org") "Knowledge Base")
         "* KB %?\n %i\n %a")
        ("l" "Log" entry
         (file+datetree (concat my-emacs-dir "Log.org"))
         "* %?\n %i\n %a")
        ("t" "TODO" entry
         (file+headline (concat my-emacs-dir "Work.org") "TODO")
         "* TODO %?\n Entered: %U\n %i\n %a")))

(eval-after-load 'org-mode
  '(progn
     (require 'writegood-mode)
     (writegood-mode)
     (abbrev-mode 1)

     (define-skeleton skel-org-block-elisp
       "Insert an emacs-lisp block"
       ""
       "#+begin_src emacs-lisp\n"
       _ - \n
       "#+end_src\n")

     (define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

     (define-skeleton skel-header-block
       "Creates my default header"
       "" "#+TITLE: " str "\n"
       "#+AUTHOR: Chris Bilson\n"
       "#+EMAIL: cbilson@pobox.com\n"
       "#+OPTIONS: toc:3 num:nil\n"
       "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" "
       "href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\""
       " />\n")

     (define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

     (require 'ob)
     (defun cbilson/org-babel-after-execute-hook ()
       (condition-case nil
           (org-display-inline-images)
         (error nil)))

     (add-hook 'org-babel-after-execute-hook
               'cbilson/org-babel-after-execute-hook)

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((ditaa . t)
        (dot . t)
        (plantuml . t)
        (python . t)
        (ruby . t)
        (sh . t)))

     (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
     (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
     (add-to-list 'org-src-lang-modes (quote ("clojure" . "clj")))

     (defvar org-babel-default-header-args:clojure
       '((:results . "silent")
         (:tangle . "yes")))

     (defvar org-babel-execute:clojure (body params)
       (lisp-eval-string body)
       "Done!")

     (provide 'ob-clojure)))

;;; XML Mode
(eval-after-load 'nxml-mode
  '(progn
     (add-hook 'nxml-mode-hook 'emmet-mode)))

;;; C#
(when (package-installed-p 'omnisharp)
  (require 'omnisharp)
  (defvar omnisharp-path
    (file-name-as-directory
     (concat my-emacs-dir "OmnisharpServer")))
  (setq omnisharp-server-executable-path
        "c:\\Users\\Chris\\OneDrive\\Emacs\\OmnisharpServer\\Omnisharp.exe"
        ;; (concat omnisharp-path "OmnisharpServer.exe")
        )
  (setq omnisharp--curl-executable-path
        "c:\\ProgramData\\chocolatey\\bin\\curl.exe")

  (add-hook 'csharp-mode-hook 'omnisharp-mode)

  (setq omnisharp-eldoc-support t)

  (global-set-key (kbd "C-c o j") 'omnisharp-start-omnisharp-server)
  (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
  (define-key omnisharp-mode-map (kbd "C-c o f") 'omnisharp-mode)
  )

;;; TeX
(eval-after-load 'tex-mode
  '(progn
     (add-hook 'LaTeX-mode-hook
	       (lambda ()
		 (setq TeX-auto-save t)
		 (setq TeX-parse-self t)
		 (reftex-mode t)
		 (TeX-fold-mode t)))))

;;; Windows Specific

(when (equal system-type 'windows-nt)
  (require 'my-windows-nt-stuff))
