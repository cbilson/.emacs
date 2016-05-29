;;; ob-powershell.el --- org-babel functions for powershell evaluation

;; Authors: Chris Bilson
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org

;;; Commentary:

;; Org-Babel support for evaluating powershell source code.

;;; Code:
(require 'ob)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("poweshell" . "ps1"))

(defvar org-babel-default-header-args:powershell '())

(defvar org-babel-powershell-command "powershell"
  "Name of command to use for executing powershell code.")

(defun org-babel-execute:powershell (body params)
  "Execute a block of Powershell code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assoc :session params)))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (org-babel-expand-body:generic
		     body params (org-babel-variable-assignments:powershell params)))
	 (session (org-babel-powershell-initiate-session session)))
    (org-babel-reassemble-table
     (org-babel-powershell-evaluate session full-body result-type result-params)
     (org-babel-pick-name
      (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
     (org-babel-pick-name
      (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))))

;; TODO: I think I *can* support sessions in powershell and really want to...
(defun org-babel-prep-session:powershell (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Powershell"))

(defun org-babel-variable-assignments:powershell (params)
  "Return list of powershell statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (org-babel-powershell--var-to-powershell (cdr pair) (car pair)))
   (mapcar #'cdr (org-babel-get-header params :var))))

;; helper functions

(defvar org-babel-powershell--lvl 0)

;; (setq foo 42)
;; (org-babel-powershell--var-to-powershell "foo" 'foo)

(defun org-babel-powershell--var-to-powershell (var &optional varn)
  "Convert an elisp value to a powershell variable.
The elisp value, VAR, is converted to a string of powershell source code
specifying a var of the same value."
  (if varn
      (let ((org-babel-powershell--lvl 0) (lvar (listp var)) prefix)
	(concat "$" (symbol-name varn) "=" (when lvar "\n")
		(org-babel-powershell--var-to-powershell var)
		";\n"))
    (let ((prefix (make-string (* 2 org-babel-powershell--lvl) ?\ )))
      (concat prefix
	      (if (listp var)
		  (let ((org-babel-powershell--lvl (1+ org-babel-powershell--lvl)))
		    (concat "[\n"
			    (mapconcat #'org-babel-powershell--var-to-powershell var "")
			    prefix "]"))
		(format "${%s}" var))
	      (unless (zerop org-babel-powershell--lvl) ",\n")))))

(defvar org-babel-powershell-buffers '(:default . nil))

(defun org-babel-powershell-initiate-session (&optional session params)
  "Return nil because sessions are not supported by powershell."
  nil)

(defvar org-babel-powershell-wrapper-method "{
    my $babel_sub = sub {
        %s
    };
    open my $BOH, qq(>%s) or die qq(Powershell: Could not open output file.$/);
    my $rv = &$babel_sub();
    my $rt = ref $rv;
    select $BOH;
    if (qq(ARRAY) eq $rt) {
        local $\\=$/;
        local $,=qq(\t);
	foreach my $rv ( @$rv ) {
	    my $rt = ref $rv;
	    if (qq(ARRAY) eq $rt) {
		print @$rv;
	    } else {
		print $rv;
	    }
	}
    } else {
	print $rv;
    }
}")

(defvar org-babel-powershell-preface nil)

(defvar org-babel-powershell-pp-wrapper-method
  nil)

;; (org-babel-powershell-evaluate nil "$x = 10" 'value)

(defun org-babel-powershell-evaluate (session ibody &optional result-type result-params)
  "Pass BODY to the Powershell process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Powershell"))
  (let* ((body (concat org-babel-powershell-preface ibody))
	 (tmp-file (org-babel-temp-file "powershell-"))
	 (tmp-babel-file (org-babel-process-file-name
			  tmp-file 'noquote)))
    (let ((results
           (case result-type
             (output
              (with-temp-file tmp-file
                (insert
                 (org-babel-eval org-babel-powershell-command body))
                (buffer-string)))
             (value
              (org-babel-eval org-babel-powershell-command
                              (format org-babel-powershell-wrapper-method
                                      body tmp-babel-file))))))
      (when results
        (org-babel-result-cond result-params
	  (org-babel-eval-read-file tmp-file)
          (org-babel-import-elisp-from-file tmp-file '(16)))))))

(provide 'ob-powershell)



;;; ob-powershell.el ends here

