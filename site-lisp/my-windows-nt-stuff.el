
(defvar consolas "-outline-Consolas-normal-r-normal-normal-18-97-96-96-c-*-iso8859-1")
(set-default-font consolas)

(defvar chocolatey-path "Path to chocolatey."
  (file-name-as-directory (getenv "ChocolateyInstall")))

(defvar chocolatey-bin-path "Path to chocolatey bin."
  (concat chocolatey-path "bin"))

(require 'ispell)
(add-to-list 'exec-path "C:\\Tools\\Aspell\\bin")
(add-to-list 'ispell-local-dictionary-alist '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil
                                              ("-B")
                                              nil iso-8859-1))

(setq browse-url-browser-function 'browse-url-default-windows-browser
      delete-by-moving-to-trash t
      ispell-aspell-data-dir "C:\\Tools\\Aspell\\data"
      ispell-aspell-dict-dir "C:\\Tools\\Aspell\\dict"
      ispell-personal-dictionary "~/.ispell"
      ispell-program-name "C:\\Tools\\Aspell\\bin\\aspell.exe"
      projectile-enable-cachinge t
      projectile-indexing-method 'native)

(provide 'my-windows-nt-stuff)
