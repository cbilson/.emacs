(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ;; ("marmalade" . "https://marmalade-repo.org/packages/")
        ))

(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

;; grizzl guru-mode
;; move-text
;; operate-on-number ov
;; projectile
;; smartparens smartrep
;; undo-tree
(require 'my-defuns)
(my-install-packages
 '(ace-jump-mode ace-jump-buffer ace-window ag
                 better-defaults browse-kill-ring
                 cider clojure-mode clojurescript-mode
                 coffee-mode csharp-mode csv-mode ctags ctags-update
                 dash dired-details dired+
                 easy-kill editorconfig elein elisp-slime-nav
                 emmet-mode ess expand-region
                 feature-mode find-file-in-project flymake
                 geiser gist gitconfig-mode gitignore-mode github-theme
                 git-timemachine god-mode goto-chg
                 htmlize
                 idle-highlight ido-ubiquitous iedit
                 js-comint
                 magit markdown-mode multiple-cursors
                 org org-magit oz
                 page-break-lines parenface-plus pp-c-l
                 rbenv
                 s scpaste smartparens smex smooth-scrolling
                 troncle twittering-mode
                 wrap-region
                 yasnippet))

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
                ("\\.cs$" . csharp-mode)
                ("\\.csx$" . csharp-mode)
                ("\\.cshtml$" . html-mode)
                ("\\.csman$" . xml-mode)
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

(require 'better-defaults)
(require 'smooth-scrolling)
(require 'recentf)

(delete-selection-mode +1)
(recentf-mode +1)
(ido-mode)

(setq backup-by-copying t
      backup-directory-alist
      `(("."
         . ,(file-name-as-directory (concat user-emacs-directory "backups"))))
      c-basic-offset 2
      delete-old-versions t
      ido-enable-flex-matching t
      ido-everywhere t
      ido-use-filename-at-point 'guess
      ido-use-url-at-point nil
      inhibit-splash-screen t
      kept-new-versions 6
      kept-old-versions 2
      ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
      ;; mouse-wheel-progressive-speed nil
      ;; mouse-wheel-follow-mouse t
      recentf-max-menu-items 15
      recentf-max-saved-items 200
      scroll-step 3
      tab-width 2
      version-control t
      whitespace-style '(face trailing lines-tail tabs))

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

;;; Remembering where I was
(savehist-mode t)

;; this looks gross in low-color mode
(when (<= (display-color-cells) 8)
  (defun hl-line-mode () (interactive)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;; bindings

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
(global-set-key (kbd "C-x C-o") (lambda () (interactive) (other-window 2)))
(global-set-key (kbd "C-x C-r") 'recentf-ido-find-file)

(global-set-key (kbd "C-c d") 'duplicate-start-of-line-or-region)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c n") 'my-cleanup-buffer)
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)

(global-set-key (kbd "C-c C-j") 'cider-jack-in)

(global-set-key (kbd "C-M-h") 'backward-kill-word)

(winner-mode)
(global-set-key (kbd "C-c w") 'winner-undo)

(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(when (package-installed-p 'smartparens)
  (smartparens-global-mode +1)
  (require 'smartparens-config)
  (cl-concatenate 'list sp-ignore-modes-list
               '())

  (define-key sp-keymap (kbd "C-S-e") 'sp-end-of-sexp))

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
(add-hook 'prog-mode-hook 'idle-highlight)
(add-hook 'prog-mode-hook 'hl-line-mode)

;;(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-connected-hook 'my-clojure-mode-eldoc-hook)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)
;;(add-hook 'emacs-lisp-mode-hook 'paredit-mode)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

;;; c-mode
(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map (kbd "C-c C-k") 'compile)))

;;; Windows Specific

(when (equal system-type 'windows-nt)
  (require 'my-windows-nt-stuff))



