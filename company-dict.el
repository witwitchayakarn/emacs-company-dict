;;; company-dict.el --- A backend that emulates ac-source-dictionary
;;
;; Copyright (C) 2015-16 Henrik Lissner

;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <henrik@lissner.net>
;; Created: June 21, 2015
;; Modified: March 1, 2019
;; Version: 1.2.8
;; Keywords: company dictionary ac-source-dictionary
;; Homepage: https://github.com/hlissner/emacs-company-dict
;; Package-Requires: ((emacs "24.4") (company "0.8.12") (parent-mode "2.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Code:

(require 'company)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(autoload 'parent-mode-list "parent-mode")

;; For compiler
(defvar yas-minor-mode)
(declare-function yas-expand-snippet "yasnippet")

(defgroup company-dict nil
  "A backend that mimics ac-source-dictionary, with support for annotations and
documentation."
  :prefix "company-dict-"
  :group 'company)

(defcustom company-dict-dir (concat user-emacs-directory "dict/")
  "Directory to look for dictionary files."
  :group 'company-dict
  :type 'directory)

(defcustom company-dict-minor-mode-list '()
  "A list of minor modes to be aware of when looking up dictionaries (if they're active)."
  :group 'company-dict
  :type '(repeat symbol))

(defcustom company-dict-enable-fuzzy nil
  "Whether to allow fuzzy searching for company-dict."
  :group 'company-dict
  :type 'boolean)
(define-obsolete-variable-alias 'company-dict-fuzzy 'company-dict-enable-fuzzy "v1.2.4")

(defcustom company-dict-enable-yasnippet t
  "If non-nil, company-dict autocompletions will be expanded like a `yasnippet'
snippet, but only if yasnippet is loaded and `yas-minor-mode' is enabled in the
current buffer. Otherwise, company-dict will pretend this is set to nil.

Note: yasnippet is optional and not a dependency of company-dict. You'll have to
install and enable it yourself."
  :group 'company-dict
  :type 'boolean)

(defcustom company-dict-max-candidates 20
  "Maximum number of candidates to display in overlay, too many would slow Emacs
due to high computation in rendering."
  :group 'company-dict
  :type 'integer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar company-dict-dicts nil
  "A one large list of all merged candidates from all dicts.")

(defun company-dict--read-file (file-path)
  (when (file-exists-p file-path)
    (unless (file-readable-p file-path)
      (user-error "Dictionary file isn't readable! (%s)" file-path))
    (decode-coding-string
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (setq buffer-file-coding-system 'utf-8)
       (insert-file-contents-literally file-path)
       (buffer-substring-no-properties (point-min) (point-max))) 'utf-8)))

(defun company-dict--get-contents (mode)
  "Read dict files and populate contents."
  (let (file contents result)
    (unless (symbolp mode)
      (error "Expected symbol, got %s" mode))

    (setq file (expand-file-name (symbol-name mode) company-dict-dir)
          contents (company-dict--read-file file))
    (when (stringp contents)
      (setq result
            (cl-loop for line in (split-string contents "\n" t)
                     for (label note meta) = (split-string (string-trim-right line) "\t" t)
                     collect (propertize label :note note :meta meta))))
    result))

(defun company-dict--relevant-modes ()
  (append '(all) (parent-mode-list major-mode) company-dict-minor-mode-list))

(defun company-dict--init ()
  (let (list result)
    (unless company-dict-dicts
      (setq list (mapcan 'company-dict--get-contents (company-dict--relevant-modes)))
      (setq company-dict-dicts (vconcat list))
      (cl-sort company-dict-dicts 'string-lessp))
    result))

(defun company-dict--annotation (data)
  (get-text-property 0 :note data))

(defun company-dict--meta (data)
  (get-text-property 0 :meta data))

(defun company-dict--quickhelp-string (data)
  (get-text-property 0 :meta data))

(defun company-dict--post-completion (data)
  (when (and company-dict-enable-yasnippet
             (featurep 'yasnippet)
             (bound-and-true-p yas-minor-mode))
    (yas-expand-snippet data (- (point) (length data)) (point))))

(defun company-dict--binary-search (value sorted-array)
  (let ((low 0)
        (high (1- (length sorted-array))))

    (do () ((< high low) nil)
      (let* ((middle (floor (+ low high) 2))
             (middle-value (aref sorted-array middle)))
        (cond ((and (not (string-prefix-p value middle-value))
                    (string-lessp value middle-value))
               (setf high (1- middle)))

              ((and (not (string-prefix-p value middle-value))
                    (not (string-lessp value middle-value)))
               (setf low (1+ middle)))

              (t (return middle)))))))

(defun company-dict--get-index-first (value sorted-array)
  (let ((index (company-dict--binary-search value sorted-array))
        (found nil))
    (if (and index (> index 0))
        (progn
          (while (and (> index 0) (not found))
            (let ((prev-index-value (aref sorted-array (1- index))))
              (if (string-prefix-p value prev-index-value)
                  (setf index (1- index))
                (setf found t))))
          index)
      index)))

(defun company-dict--get-prefix-match (value sorted-array max)
  (let ((prefix-value value)
        (first-asterisk-index (string-match-p "*" value)))

    (if (and company-dict-enable-fuzzy first-asterisk-index)
        (setf prefix-value (substring value 0 first-asterisk-index)))

    (let ((index (company-dict--get-index-first prefix-value sorted-array)))
      (if index
          (let ((cur-index index)
                (matched-dict nil)
                (done nil))
            (if (and company-dict-enable-fuzzy first-asterisk-index)
                (setf prefix-value (concat "^" (replace-regexp-in-string "*" ".*" value)))
              (setf prefix-value (concat "^" value)))

            (while (and (< cur-index (length sorted-array)) (not done))
              (if (string-match-p prefix-value (aref sorted-array cur-index))
                  (progn
                    (push (aref sorted-array cur-index) matched-dict)
                    (if (>= (length matched-dict) max)
                        (setf done t))))
              (setf cur-index (1+ cur-index)))
            matched-dict)
        nil))))

;;;###autoload
(defun company-dict-refresh ()
  "Refresh all loaded dictionaries."
  (interactive)
  (setq company-dict-dicts nil)
  (company-dict--init))

;;;###autoload
(defun company-dict (command &optional arg &rest ignored)
  "`company-mode' backend for user-provided dictionaries. Dictionary files are lazy
loaded."
  (interactive (list 'interactive))
  (company-dict--init)
  (let ((dicts company-dict-dicts)
        (max company-dict-max-candidates))
    (cl-case command
      (interactive     (company-begin-backend 'company-dict))
      (prefix          (and dicts (company-grab-symbol)))
      (candidates      (company-dict--get-prefix-match arg dicts max))
      (annotation      (company-dict--annotation arg))
      (meta            (company-dict--meta arg))
      (quickhelp-string (company-dict--quickhelp-string arg))
      (post-completion (company-dict--post-completion arg))
      (sorted          't)
      (no-cache        't))))

(provide 'company-dict)
;;; company-dict.el ends here
