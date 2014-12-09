;; A lighter-weight init.el for 24

(defvar my-emacs-dir
  (let ((path
         (cond
            ((file-exists-p "~/OneDrive/Emacs")
             "~/OneDrive/Emacs/site-lisp")
            ((file-exists-p "~/SkyDrive/Emacs")
             "~/SkyDrive/Emacs")
            ((file-exists-p "~/emacs")))))
    (when path
      (file-name-as-directory path))))

(defvar my-lisp-dir
  (when my-emacs-dir
    (file-name-as-directory (concat my-emacs-dir "site-lisp"))))

(when my-lisp-dir
  (add-to-list 'load-path my-lisp-dir))

(load (concat my-emacs-dir "init.el"))

