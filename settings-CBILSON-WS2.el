
(defvar cbilson/backups-dir (concat user-emacs-directory "backups"))
(defvar cbilson/java-home (substitute-in-file-name "$JavaHome"))
(defvar cbilson/java-cmd "java")

(defvar cbilson/omnisharp-path
  (file-name-as-directory
   (concat cbilson/emacs-dir "OmnisharpServer")))

(defvar cbilson/high-dpi
  (pcase system-name
    ("CBILSON-LAPTOP" t)
    ("MAKO" t)
    (_ nil)))

(defun cbilson/reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update)
  (erc-modified-channels-display))

(defun cbilson/cleanup-buffer ()
  (interactive)
  (delete-trailing-whitespace)
  (untabify (point-min) (point-max))
  (indent-region (point-min) (point-max)))

(defun cbilson/clojure-mode-eldoc-hook ()
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode))

(defun cbilson/kill-word-key ()
  (local-set-key (kbd "C-M-h") 'backward-kill-word))

(defun cbilson/install-packages (packages)
  (dolist (p packages)
    (when (not (package-installed-p p))
      (unwind-protect
          (condition-case ex
              (package-install p)
            ('error
             (message (format "Failed to install package %s: %s"
                              p
                              ex))))))))

;; Duplicate start of line or region,
;; from http://www.emacswiki.org/emacs/DuplicateStartOfLineOrRegion
(defun cbilson/duplicate-start-of-line ()
  (if (bolp)
      (progn
        (end-of-line)
        (duplicate-start-of-line)
        (beginning-of-line))
    (let ((text (buffer-substring (point)
                                  (beginning-of-thing 'line))))
      (forward-line)
      (push-mark)
      (insert text)
      (open-line 1))))

(defun cbilson/duplicate-region ()
  (let* ((end (region-end))
         (text (buffer-substring (region-beginning) end)))
    (goto-char end)
    (insert text)
    (push-mark end)
    (setq deactivate-mark nil)
    (exchange-point-and-mark)))

(defun cbilson/duplicate-start-of-line-or-region ()
  (interactive)
  (if mark-active
      (cbilson/duplicate-region)
    (cbilson/duplicate-start-of-line)))

(defun cbilson/recentf-ido-find-file ()
  "Find a recent file using ido."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(defun cbilson/rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defun cbilson/first-existing-file (files)
  (let ((candidates (mapcar #'substitute-in-file-name files)))
    (find-if #'file-exists-p candidates)))

(defun cbilson/first-existing-dir (files)
  (let ((candidates (mapcar #'substitute-in-file-name files)))
    (file-name-as-directory
     (find-if #'file-exists-p candidates))))

(defun cbilson/join-line-back ()
  (interactive)
  (join-line -1))

(defun cbilson/next-line-more ()
  (interactive)
  (ignore-errors (next-line 5)))

(defun cbilson/previous-line-more ()
  (interactive)
  (ignore-errors (previous-line 5)))

(setq cbilson/packages
  '(
    ;; Themes
    smyx-theme
    darktooth-theme
    material-theme
    afternoon-theme

    ;; Saw this mentioned here: [1]
    ace-isearch

    ;; other ace modes
    ace-jump-mode ace-jump-buffer ace-window

    ;; LaTeX
    acutex

    ;; Fast grep-replacement
    ag

    ;; org-mode
    org org-magit

    ;; misc minor modes
    browse-kill-ring
    dash dired-details
    dired+
    easy-kill
    editorconfig
    expand-region
    htmlize
    flx
    flx-ido
    helm
    helm-moccur
    idle-highlight-mode
    ido-ubiquitous
    iedit
    kurecolor
    loccur
    multiple-cursors
    page-break-lines
    paren-face
    projectile
    rainbow-mode

    ;; Basic Programming
    emmet-mode
    feature-mode
    find-file-in-project
    flymake

    ;; git
    gist
    gitconfig-mode
    gitignore-mode
    git-timemachine
    magit

    ;; Clojure
    cider
    clojure-mode

    ;; scheme
    geiser

    ;; elisp
    elisp-slime-nav

    ;; Web programming
    coffee-mode

    ;; .NET
    fsharp-mode
    csharp-mode
    omnisharp

    ;; Powershell
    ;; powershell
    ;; Using my fork instead

    ;; misc major modes
    csv-mode
    god-mode
    markdown-mode

    js-comint
    yasnippet

    pp-c-l
    rbenv
    s scpaste slime smartparens smex smooth-scrolling
    tuareg-mode
    twittering-mode
    undo-tree
    wrap-region writegood-mode
    yaml-mode

    ;; Tags
    ctags ctags-update))

(cbilson/install-packages cbilson/packages)

(setq user-full-name "Chris Bilson")
(setq user-mail-address "cbilson@pobox.com")

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-splash-screen t
      initial-scratch-message nil
      scroll-step 3
      visible-bell t
      use-dialog-box nil)

(setq
 ;; I want scratch buffers to be in org-mode
 initial-scratch-message nil
 initial-major-mode 'org-mode)

(setq backup-by-copying t
      backup-directory-alist
      `((".*" . ,(file-name-as-directory cbilson/backups-dir)))
      auto-save-file-name-transform
      `((".*" ,(file-name-as-directory cbilson/backups-dir) t))
      create-lock-files nil
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      make-backup-files t)

(setq c-basic-offset 2
      column-number-mode t
      indent-tabs-mode nil
      tab-width 2)

(setq indicate-empty-lines t)

(setq require-final-newline t)

(setq apropos-do-all t)

(setq echo-keystrokes 0.1)

(setq save-interprogram-paste-before-kill t)

(setq version-control t
      whitespace-style '(face trailing lines-tail tabs)
      x-select-enable-clipboard t
      x-select-enable-primary t)

;; auto-revert
(global-auto-revert-mode +1)
(setq global-auto-revert-non-file-buffers +1
      auto-revert-verbose nil)

(setq-default ispell-program-name "aspell" )

(semantic-mode 1)

(add-to-list 'safe-local-variable-values
             '((c-set-offset . 2)
               (c-set-offset . 4)
               (c-basic-offset . 2)
               (c-basic-offset . 4)))

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
                ("\\.t4$" . xml-mode)
                ("\\.m$" . octave-mode)
                ("\\.ps1$" . powershell-mode)
                ("\\.psm1$" . powershell-mode)
                ("\\.R$" . r-mode)
                ("\\.r$" . r-mode)
                ("\\.spark$" . html-mode))))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b"))
        mouse-yank-at-point t
        ;; mouse-wheel-scroll-amount '(1 ((shift) . 1))
        ;; mouse-wheel-progressive-speed nil
        ;; mouse-wheel-follow-mouse t
        )

  (when (package-installed-p 'afternoon-theme)
    (load-theme 'afternoon t))

  ;; Fonts
  ;;(set-face-attribute 'default nil ...)

  (set-face-attribute 'minibuffer-prompt nil :slant 'oblique)
  (set-face-attribute 'mode-line nil
                      :foreground "cornsilk"
                      :background "grey30"
                      :slant 'normal
                      :weight 'bold))

(when (eq window-system 'w32)

  (defvar cbilson/base-font-height
    (if cbilson/high-dpi 100 120))

  ;; Fonts
  (set-face-attribute 'default nil
                      :font "Consolas"
                      :height cbilson/base-font-height)

  (set-face-attribute 'minibuffer-prompt nil
                      :font "Candara"
                      :height (+ cbilson/base-font-height 40))

  (set-face-attribute 'mode-line nil
                      :font "Corbel"
                      :height (+ cbilson/base-font-height 20))

  (set-face-attribute 'mode-line-inactive nil
                      :font "Corbel"
                      :height (+ cbilson/base-font-height 20))

  (defvar cbilson/chocolatey-path
    (file-name-as-directory (getenv "ChocolateyInstall"))
    "Path to chocolatey.")

  (defvar cbilson/chocolatey-bin-path
    (file-name-as-directory (concat cbilson/chocolatey-path "bin"))
    "Path to chocolatey bin.")

  (require 'ispell)
  (require 'flyspell)

  (setq aspell-dir (file-name-as-directory (concat cbilson/emacs-dir "aspell")))
  (setq aspell-bin-dir (file-name-as-directory (concat aspell-dir "bin")))
  (setq ispell-program-name (concat aspell-bin-dir "aspell.exe"))

  (setq ispell-aspell-data-dir (file-name-as-directory (concat aspell-dir "data")))
  (setq ispell-aspell-dict-dir (file-name-as-directory (concat aspell-dir "dict")))


  (add-to-list 'ispell-local-dictionary-alist '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
                                                ("-B")
                                                nil iso-8859-1))

  (setq browse-url-browser-function 'browse-url-default-windows-browser
        delete-by-moving-to-trash t
        ispell-personal-dictionary "~/.ispell"
        path-to-ctags "G:\\bin\\ctags.exe"
        projectile-enable-cachinge t
        projectile-indexing-method 'native)

  ;; lisp
  (eval-after-load 'lisp-mode
    '(progn
       (when (file-exists-p "~/quicklisp/slime-helper.el")
         (load "~/quicklisp/slime-helper.el"))
       ;; Replace "sbcl" with the path to your implementation
       (setq inferior-lisp-program
             "C:\\Program Files\\Steel Bank Common Lisp\\1.2.7\\sbcl.exe")))

  ;; java
  (setq cbilson/java-home
        (let* ((candidates
                (mapcar #'substitute-in-file-name
                        '("$ProgramW6432\\Zulu\\zulu-8"
                          "$ProgramW6432\\Java\\JRE")))
               (java-dir (find-if #'file-exists-p candidates)))
          (when java-dir
            (file-name-as-directory
             (find-if #'file-exists-p candidates)))))

  (setenv "JAVA_HOME" cbilson/java-home)

  (setq cbilson/java-cmd
        (concat (file-name-as-directory (concat cbilson/java-home "bin")) "java.exe"))

  (setenv "JAVA_CMD" cbilson/java-cmd)

  ;; maximize the window
  (w32-send-sys-command 61488)

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper))

(when (eq window-system 'w32)
  (set-file-modes (expand-file-name "~/.emacs.d/server") #o700))

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-;") 'comment-dwim)
(global-set-key (kbd "M-j") 'cbilson/join-line-back)
(global-set-key (kbd "C-S-n") 'cbilson/next-line-more)
(global-set-key (kbd "C-S-p") 'cbilson/previous-line-more)
(global-set-key (kbd "C-x m") 'eshell)
(global-set-key (kbd "C-x C-m") 'shell)
;; (global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-i") 'imenu)
(global-set-key (kbd "C-x C-j") 'dired-jump)
;;(global-set-key (kbd "C-x C-r") 'cbilson/recentf-ido-find-file)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c d") 'cbilson/duplicate-start-of-line-or-region)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c n") 'cbilson/cleanup-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-c C-r") 'revert-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key (kbd "H-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "H-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "H-<up>") 'enlarge-window)
(global-set-key (kbd "H-<down>") 'shrink-window)

(add-hook 'prog-mode-hook 'whitespace-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'cbilson/kill-word-key)

(when (package-installed-p 'idle-highlight)
  (add-hook 'prog-mode-hook 'idle-highlight))

(when (package-installed-p 'ag)
  (setq ag-executable
        (cbilson/first-existing-file
         (list (concat cbilson/chocolatey-bin-path "ag.bat")
               (concat cbilson/chocolatey-bin-path "ag.exe")
               "/usr/local/bin/ag"))))

(eval-after-load 'cc-mode
  '(progn
     (define-key c-mode-map (kbd "C-c C-k") 'compile)))

(when (package-installed-p 'csharp-mode)

  (c-add-style "Microsoft C#"
               '("C#"
                 (c-basic-offset . 4)
                 (c-offsets-alist .
                                  ((arglist-intro . c-lineup-arglist-intro-after-paren)
                                   (arglist-cont . c-lineup-arglist)))))

  (defun cbilson/csharp-mode-hook ()
    (setq c-default-style "Microsoft C#"))

  (add-hook 'csharp-mode-hook 'cbilson/csharp-mode-hook)

  (when (package-installed-p 'omnisharp)
    (require 'omnisharp)
    (setq omnisharp--curl-executable-path
          (if (eq window-system 'w32)
              (concat cbilson/chocolatey-bin-path "curl.exe")
            "curl")
          omnisharp-eldoc-support t
          omnisharp-server-executable-path
          (concat cbilson/omnisharp-path "OmniSharp.exe"))

    ;; omnisharp-mode is really slow for big C# files, so turning off for now
    ;; (add-hook 'csharp-mode-hook 'omnisharp-mode)

    (global-set-key (kbd "C-c o j") 'omnisharp-start-omnisharp-server)
    (define-key omnisharp-mode-map (kbd "M-.") 'omnisharp-go-to-definition)
    (define-key omnisharp-mode-map (kbd "C-c C-k") 'omnisharp-build-in-emacs)
    (define-key omnisharp-mode-map (kbd "C-c o d") 'omnisharp-go-to-definition)
    (define-key omnisharp-mode-map (kbd "C-c o D") 'omnisharp-current-type-documentation)
    (define-key omnisharp-mode-map (kbd "C-c o f") 'omnisharp-mode)
    (define-key omnisharp-mode-map (kbd "C-c o i") 'omnisharp-find-implementations)
    (define-key omnisharp-mode-map (kbd "C-c o I") 'omnisharp-current-type-information)
    (define-key omnisharp-mode-map (kbd "C-c o r") 'omnisharp-rename)
    (define-key omnisharp-mode-map (kbd "C-c o u") 'omnisharp-find-usages)
    (define-key omnisharp-mode-map (kbd "C-c o RET") 'omnisharp-fix-code-issue-at-point)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", n t") 'omnisharp-navigate-to-current-file-member)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", n s") 'omnisharp-navigate-to-solution-member)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", n f") 'omnisharp-navigate-to-solution-file-then-file-member)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", n F") 'omnisharp-navigate-to-solution-file)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd ", n r") 'omnisharp-navigate-to-region)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
    ;; (evil-define-key 'insert omnisharp-mode-map (kbd "<f12>") 'omnisharp-show-last-auto-complete-result)
    ;; (evil-define-key 'normal omnisharp-mode-map (kbd ",.") 'omnisharp-show-overloads-at-point)

    ))

(when (package-installed-p 'cider)
  (global-set-key (kbd "C-c C-j") 'cider-jack-in)

  (eval-after-load 'cider
    '(progn
       (define-key cider-mode-map (kbd "C-c TAB") 'complete-symbol))))

;; don't prompt for compilation command unless prefixed
(setq compilation-read-command nil)

;; hide compilation buffer unless error
(defadvice compilation-start
    (around inhibit-display
            (command &optional mode name-function highlight-regexp))
  (if (not (string-match "^\\(find\\|grep\\)" command))
      (flet ((display-buffer)
         (set-window-point)
         (goto-char))
    (fset 'display-buffer 'ignore)
    (fset 'goto-char 'ignore)
    (fset 'set-window-point 'ignore)
    (save-window-excursion
      ad-do-it))
    ad-do-it))

;;(ad-activate 'compilation-start)
;;(ad-deactivate 'compilation-start)

(defun eshell/rgrep (&rest args)
  "Use Emacs grep facility instead of calling external grep."
  (eshell-grep "rgrep" args t))

(defun eshell/cdg ()
  "Change directory to the project's root."
  (eshell/cd (locate-dominating-file default-directory ".git")))

(global-set-key (kbd "C-c t") 'eshell)

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

(add-hook 'cider-connected-hook 'cbilson/clojure-mode-eldoc-hook)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
(add-hook 'ielm-mode-hook 'elisp-slime-nav-mode)

(define-key emacs-lisp-mode-map (kbd "C-c v") 'eval-buffer)

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(eval-after-load 'tex-mode
  '(progn
     (add-hook 'LaTeX-mode-hook
               (lambda ()
                 (setq TeX-auto-save t)
                 (setq TeX-parse-self t)
                 (reftex-mode t)
                 (TeX-fold-mode t)))))

(setq magit-last-seen-setup-instructions "1.4.0")

(eval-after-load 'org
 '(progn
   (require 'ob)
   (require 'ob-clojure)

   (setq org-agenda-show-log t

         ;; log when something changes to done.
         org-log-done t)

   (define-skeleton skel-header-block
     "Creates my default header"
     "" "#+TITLE: " str "\n"
     "#+AUTHOR: Chris Bilson\n"
     "#+EMAIL: cbilson@pobox.com\n"
     "#+OPTIONS: toc:3 num:nil html-postamble:nil\n"
     "#+STYLE: <link rel=\"stylesheet\" type=\"text/css\" "
     "href=\"http://thomasf.github.io/solarized-css/solarized-light.min.css\""
     " />\n")

   (define-abbrev org-mode-abbrev-table "sheader" "" 'skel-header-block)

   (defun cbilson/org-mode-hook ()
     (abbrev-mode 1))

   (add-hook 'org-mode-hook 'flyspell-mode)
   (add-hook 'org-mode-hook 'cbilson/org-mode-hook)
   (add-hook 'org-mode-hook 'auto-fill-mode)))

(eval-after-load 'org
  '(progn

     (define-key org-mode-map (kbd "C-c C-j") 'org-goto)
     (define-key org-mode-map (kbd "C-c r") 'org-refile)

     (setq org-agenda-files (list cbilson/org-dir))

     (setq org-agenda-show-log t
           ;; org-archive-location "::* Archive"
           org-default-notes-file (concat cbilson/org-dir "Capture.org")
           org-refile-targets '((org-agenda-files . 3))
           org-todo-keywords
           '((sequence "BACKLOG" "TODO" "WIP" "BLOCKED" "|" "DONE" "DELEGATED"))
           ;;org-todo-keyword-faces
           ;;'(("WIP" . (:foreground "light-blue" :weight bold)))
           )

     (setq org-capture-templates
           '(("I" "Incident" entry
              (file+datetree (concat cbilson/org-dir "Capture.org"))
              "* RDIncident %?   :incident:\n  Entered on %U\n %i")
             ("i" "Interesting" entry
              (file+datetree (concat cbilson/org-dir "Capture.org"))
              "* %?  :interesting:\n %i\n %a")
             ("k" "Knowledge Base" entry
              (file+datetree (concat cbilson/org-dir "Capture.org"))
              "* KB %?   :kb:\n %i\n %a")
             ("l" "Log" entry
              (file+datetree (concat cbilson/org-dir "Capture.org"))
              "* %?   :log:\n  Entered: %U\n %i\n %a")
             ("m" "Meeting" entry
              (file+datetree (concat cbilson/org-dir "Capture.org"))
              "* Meeting %?   :meeting:\nEntered: %U\n %i\n %a")
             ("t" "TODO" entry
              (file+datetree (concat cbilson/org-dir "Work.org"))
              "* BACKLOG %?\n  Entered: %U\n %i\n %a")))))

(eval-after-load 'org
 '(progn
   (require 'ob)
   (require 'ob-clojure)

   (setq org-confirm-babel-evaluate nil
         org-ditaa-jar-path (concat cbilson/emacs-dir "vendor/ditaa0_9.jar")
         org-plantuml-jar-path (concat cbilson/emacs-dir "vendor/plantuml.jar")
         org-source-fontify-natively t)

   (add-hook 'org-babel-after-execute-hook
             'cbilson/org-babel-after-execute-hook)

  (define-skeleton skel-org-block-elisp
     "Insert an emacs-lisp block"
     ""
     "#+begin_src emacs-lisp\n"
     _ - \n
     "#+end_src\n")

   (define-abbrev org-mode-abbrev-table "selisp" "" 'skel-org-block-elisp)

   (define-skeleton skel-org-block-powershell
     "Insert a powershell block"
     ""
     "#+begin_src powershell\n"
     _ - \n
     "#+end_src\n")

   (define-abbrev org-mode-abbrev-table "sposh" "" 'skel-org-block-powershell)

   (org-babel-do-load-languages
    'org-babel-load-languages
    '((clojure . t)
      (ditaa . t)
      (dot . t)
      (plantuml . t)
      (python . t)
      (ruby . t)
      (sh . t)))

   (add-to-list 'org-src-lang-modes (quote ("dot" . graphviz-dot)))
   (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
   (add-to-list 'org-src-lang-modes (quote ("clojure" . "clj")))))

(eval-after-load 'nxml-mode
  '(progn
     (add-hook 'nxml-mode-hook 'emmet-mode)))

(when (package-installed-p 'ace-jump-mode)
  (require 'ace-jump-mode)
  (define-key global-map (kbd "C-c j") 'ace-jump-char-mode)
  (define-key global-map (kbd "C-c J") 'ace-jump-word-mode)
  (define-key global-map (kbd "C-c L") 'ace-jump-line-mode))

(defun corext-contains-project-file-p (dir)
  (directory-files dir nil "proj$"))

(defun corext-compile ()
  (interactive)
  (let* ((buffer-dir (file-name-directory (buffer-file-name)))
         (default-directory (locate-dominating-file
                             buffer-dir
                             #'corext-contains-project-file-p))
         (build-cmd (concat (projectile-project-root)
                            ".config/.corext/gen/InPath/build.cmd")))
    (compilation-start build-cmd)))

(defvar corext-default-test-project nil)
(defvar corext-test-project-history nil)
(defvar corext-default-test-name nil)
(defvar corext-test-name-history nil)

(defun corext-read-name (prompt default history-sym)
  (let* ((history (symbol-value history-sym)))
    (read-shell-command
     prompt
     nil
     (if (equal (car history) default)
         (list history . 1)
       history-sym))))

;; (corext-read-name "Foo: " "bar" 'foo-history)
;; (corext-read-name "Test Name: " corext-default-test-name 'corext-test-name-history)

(defun corext-read-test-project (&optional test-project)
  (let* ((test-project (or test-project corext-default-test-project)))
    (corext-read-name "Test Project: "
                      test-project
                      'corext-test-project-history)))

(defun corext-read-test-name (&optional test-name)
  (let* ((test-name (or test-name corext-default-test-name)))
    (corext-read-name "Test Name: "
                      test-name
                      'corext-test-name-history)))

(defun corext-test (&optional test-name)
  (interactive "M")
  (let* ((buffer-dir (file-name-directory (buffer-file-name)))
         (test-base-dir (concat (projectile-project-root)
                                "out/debug-AMD64/Services/DatacenterManager"))
         (test-name
          ())
         (default-directory (locate-dominating-file
                             buffer-dir
                             #'corext-contains-project-file-p))
         (build-cmd (concat (projectile-project-root)
                            ".config/.corext/gen/InPath/build.cmd")))
    (compilation-start build-cmd))
  )

(delete-selection-mode +1)

(require 'easy-kill)
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(when (package-installed-p 'editorconfig)
  (require 'editorconfig)

  (setq edconf-exec-path
        (let ((editorconfig (substitute-in-file-name
                             "$ChocolateyInstall/bin/editorconfig")))
          (if (file-exists-p (concat editorconfig ".exe"))
              (concat editorconfig ".exe")
            (concat editorconfig ".bat"))))

  (add-to-list 'edconf-indentation-alist '(csharp-mode c-basic-offset)))

(when (package-installed-p 'expand-region)
  (global-set-key (kbd "C-=") 'er/expand-region))

(when (and (package-installed-p 'ido)
           (package-installed-p 'flx-ido))
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t
        ido-use-faces nil))

(when (package-installed-p 'helm)
  (require 'helm-config)

  (setq helm-ff-file-name-history-use-recentf t
        helm-ff-search-library-in-sexp t
        helm-imenu-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-move-to-line-cycle-in-source t
        helm-scroll-amount 8
        helm-semantic-fuzzy-match t)

  (helm-autoresize-mode 1)

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-c h o") 'helm-occur))

(setq ido-enable-flex-matching t)

(when (package-installed-p 'iedit)
  (global-set-key (kbd "C-c ;") 'iedit-mode)
  (global-set-key (kbd "C-c C-;") 'iedit-mode-from-isearch))

(when (package-installed-p 'loccur)
  (require 'loccur)
  (global-set-key (kbd "C-o") 'loccur-current))

(when (package-installed-p 'multiple-cursors)
  (global-set-key (kbd "C-c m a") 'mc/edit-beginnings-of-lines)
  (global-set-key (kbd "C-c m c") 'mc/add-cursor-on-click)
  (global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
  (global-set-key (kbd "C-c m f") 'mc/mark-all-like-this-in-defun)
  (global-set-key (kbd "C-c m l") 'mc/edit-lines)
  (global-set-key (kbd "C-c m m") 'mc/mark-all-dwim))

(add-to-list 'load-path
             (file-name-as-directory
              (concat cbilson/lisp-dir "powershell.el")))

(require 'powershell)

(setq powershell-eldoc-def-files nil)

(add-to-list 'safe-local-variable-values
             '((powershell-indent . 4)
               (powershell-indent . 2)
               (powershell-continuation-indent . 2)
               (powershell-continuation-indent . 4)))

(setq powershell-indent 2)
(setq powershell-continuation-indent 2)

(when (package-installed-p 'projectile)
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-indexing-method 'native)

  (define-key projectile-mode-map (kbd "C-c C-k") 'projectile-compile-project))

(require 'saveplace)
(setq-default save-place t
              save-place-file (concat user-emacs-directory "places"))

(eval-after-load 'smartparens
  '(progn
     (smartparens-global-mode +1)
     (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
     (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

     (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
     (define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
     (define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
     (define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

     (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
     (define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
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

     (define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)))

;; If it's installed, require it to get it loaded
(when (package-installed-p 'smartparens)
  (require 'smartparens-config)
  (show-smartparens-global-mode +1))

;;(when (package-installed-p 'smooth-scrolling)
;;  (require 'smooth-scrolling))

(transient-mark-mode +1)

(when (package-installed-p 'undo-tree)
  (undo-tree-mode +1)

  ;; these are just the standard undo-tree keys, but I keep them here
  ;; for reference
  (global-set-key (kbd "C-_") 'undo-tree-undo)
  (global-set-key (kbd "M-_") 'undo-tree-redo)
  (global-set-key (kbd "C-x u") 'undo-tree-visualize))

(winner-mode +1)

(require 'yasnippet)
(setq yas-snippet-dirs
      (file-name-as-directory
       (concat cbilson/emacs-dir "snippets")))

(yas-global-mode +1)

(require 'uniquify)

(setq uniquify-buffer-name-style 'forward)

(when (package-installed-p 'writegood-mode)
  (require 'writegood-mode)

  (set-face-attribute 'writegood-passive-voice-face nil :background "gray22")
  (set-face-attribute 'writegood-duplicates-face nil :background "gray22")

  (add-hook 'org-mode-hook 'writegood-mode)
  (add-hook 'markdown-mode-hook 'writegood-mode)
  (add-hook 'latex-mode-hook 'writegood-mode))
