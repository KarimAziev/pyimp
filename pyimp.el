;;; pyimp.el --- Add import statements in Python -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/pyimp
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (pyvenv "1.21"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Add import statements in Python

;;; Code:

(require 'project)
(require 'pyvenv)
(require 'treesit)

(defconst pyimp--builin-imports-code "\
import pkgutil
import sys

installed_modules = [module.name for module in pkgutil.iter_modules()]
builtin_modules = list(sys.builtin_module_names)
all_modules = installed_modules + builtin_modules

emacs_lisp_readable_str = \"(\" + \" \".join(f'\"{item}\"' for item in all_modules) + \")\"

print(emacs_lisp_readable_str)
"
  "Script to list import statements in Python code.")

(defconst pyimp--describe-module-code "\
import %s

print(help(%s))
"
  "Script to describe Python module.")

(defconst pyimp--module-symbols-script "
import %s

all_symbols = dir(%s)
symbols = [symb for symb in all_symbols if not symb.startswith(\"__\")]
emacs_lisp_readable_str = \"(\" + \" \".join(f'\"{item}\"' for item in symbols) + \")\"

print(emacs_lisp_readable_str)
"
  "Template script to extract symbols from specific module.")


(defun pyimp--file-modification-time (file)
  "Return the modification time of FILE.

Argument FILE is the path to the file whose modification time is needed."
  (file-attribute-modification-time (file-attributes file)))

(defvar pyimp-builtins-cached nil)

(defun pyimp--locate-project-root ()
  "Return the project root directory by locating the nearest `__init__.py' file."
  (let ((dir default-directory)
        (res))
    (while
        (when-let* ((last-dir (locate-dominating-file dir
                                                     "__init__.py")))
          (setq res dir))
      (setq dir (file-name-parent-directory dir)))
    (when res
      (file-name-parent-directory res))))


(defun pyimp--extract-symbols-from-module (module)
  "Extract and return non-private symbols from a Python module.

Argument MODULE is the name of the Python module from which to extract symbols."
  (let ((code (format pyimp--module-symbols-script module module)))
    (pyimp--exec-python-code-to-elisp code)))

(defun pyimp--list-builtin-modules ()
  "Return a list of all installed and built-in Python modules."
  (pyimp--exec-python-code-to-elisp pyimp--builin-imports-code))

(defun pyimp--exec-python-code-to-elisp (code)
  "Execute Python CODE and return the trimmed output or nil on error.

Argument CODE is the Python code to be executed as a string."
  (let ((cmd (if (bound-and-true-p python-interpreter)
                 python-interpreter
               "python")))
    (with-temp-buffer
      (let ((status (call-process
                     cmd nil t nil
                     "-c" code)))
        (if (zerop status)
            (progn (goto-char (point-max))
                   (skip-chars-backward "\s\t\n")
                   (backward-sexp)
                   (ignore-errors (read (current-buffer))))
          (message "Pyimp: An error occured: %s" (buffer-string))
          nil)))))

(defun pyimp--minibuffer-get-metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun pyimp--minibuffer-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (pyimp--minibuffer-get-metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun pyimp--minibuffer-get-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (pyimp--minibuffer-get-metadata) 'category)
       all))))

(defun pyimp--get-minibuffer-get-default-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (pyimp--minibuffer-get-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar pyimp--minibuffer-targets-finders
  '(pyimp--minibuffer-ivy-selected-cand
    pyimp--vertico-selected
    pyimp--get-minibuffer-get-default-completion))

(defvar pyimp-modules-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-j") #'pyimp-describe-module)
    (define-key map (kbd "C-c C-o") #'pyimp-abort-minibuffer-and-describe-module)
    map)
  "Keymap for minibuffer commands in Python import modules.")

(declare-function vertico--candidate "ext:vertico")
(declare-function vertico--update "ext:vertico")

(defun pyimp--vertico-selected ()
  "Target the currently selected item in Vertico.
Return the category metadatum as the type of the target."
  (when (bound-and-true-p vertico--input)
    (vertico--update)
    (cons (completion-metadata-get (pyimp--minibuffer-get-metadata) 'category)
          (vertico--candidate))))

(defun pyimp--minibuffer-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'pyimp--minibuffer-targets-finders
     (lambda (fun)
       (when-let* ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    target))

(defun pyimp--minibuffer-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (pyimp--minibuffer-get-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))


(defun pyimp--minibuffer-action-no-exit (action)
  "Call ACTION with minibuffer candidate in its original window."
  (pcase-let ((`(,_category . ,current)
               (pyimp--minibuffer-get-current-candidate)))
    (with-minibuffer-selected-window
      (funcall action current))))

(defun pyimp--exec-async (program on-done on-error &rest args)
  "Execute PROGRAM asynchronously with ARGS, handling completion and errors.

Argument PROGRAM is the shell command to run asynchronously.

Argument ON-DONE is the function to call when the process completes
successfully.

Argument ON-ERROR is the function to call when the process fails.

Remaining arguments ARGS are strings passed as command arguments to PROGRAM."
  (require 'ansi-color)
  (let* ((buff-name (generate-new-buffer-name (concat "*pyimp-" program "*")
                                              program))
         (buff (get-buffer-create buff-name))
         (proc (apply #'start-file-process buff-name buff program args)))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process _)
       (let ((buff (process-buffer process)))
         (with-current-buffer buff
           (if (zerop (process-exit-status process))
               (when on-done
                 (funcall on-done))
             (if on-error
                 (funcall on-error)
               (funcall #'pop-to-buffer (current-buffer))))))))
    (set-process-filter
     proc
     (lambda (proc string)
       (when-let* ((buf (process-buffer proc)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert string)))))))
    proc))

(defun pyimp--get-other-wind ()
  "Return another window or split sensibly if needed."
  (if (minibuffer-selected-window)
      (with-minibuffer-selected-window
        (let ((wind (selected-window)))
          (or
           (window-right wind)
           (window-left wind)
           (split-window-sensibly)
           wind)))
    (let ((wind (selected-window)))
      (or
       (window-right wind)
       (window-left wind)
       (split-window-sensibly)
       wind))))

(defun pyimp-abort-minibuffer-and-describe-module (module)
  "Abort the minibuffer and describe the specified Python module.

Argument MODULE is the name of the Python module to describe."
  (interactive
   (list
    (if (active-minibuffer-window)
        (pcase-let
            ((`(,_category . ,current)
              (pyimp--minibuffer-get-current-candidate)))
          current)
      (let ((default-directory (or
                                (pyimp--locate-project-root)
                                default-directory)))
        (pyimp--completing-read-with-preview "Module: "
                                             (pyimp--get-modules)
                                             nil
                                             pyimp-modules-minibuffer-map)))))
  (if-let* ((wnd (active-minibuffer-window)))
      (progn (select-window wnd)
             (run-with-timer 0 nil #'pyimp-describe-module module t)
             (abort-minibuffers))
    (pyimp-describe-module module t)))


(defun pyimp-describe-module (module &optional select)
  "Display detailed information about a specified Python module.

Argument MODULE is the name of the Python module to describe.

Optional argument SELECT, if non-nil, selects the window displaying the MODULE
description."
  (interactive
   (list
    (if (active-minibuffer-window)
        (pcase-let
            ((`(,_category . ,current)
              (pyimp--minibuffer-get-current-candidate)))
          current)
      (let* ((default-directory (or
                                 (pyimp--locate-project-root)
                                 default-directory)))
        (pyimp--completing-read-with-preview "Module: "
                                             (pyimp--get-modules)
                                             nil
                                             pyimp-modules-minibuffer-map)))))
  (require 'ansi-color)
  (let* ((mini-wind (minibuffer-selected-window))
         (buff-name (concat "*pyimp-help*"))
         (buff (progn
                 (when (get-buffer buff-name)
                   (kill-buffer buff-name))
                 (get-buffer-create buff-name)))
         (code (format pyimp--describe-module-code module module))
         (proc
          (let ((default-directory (or
                                    (pyimp--locate-project-root)
                                    default-directory)))
            (start-process buff-name buff "python" "-c" code))))
    (with-current-buffer buff
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (set-process-sentinel
     proc
     (lambda (process _)
       (let ((buff (process-buffer process)))
         (when (and (buffer-live-p buff)
                    (or (not mini-wind)
                        (eq mini-wind
                            (minibuffer-selected-window))))
           (with-current-buffer buff
             (pyimp--fontify-module-help)
             (goto-char (point-min))
             (let ((wnd (pyimp--get-other-wind)))
               (with-selected-window wnd
                 (pop-to-buffer-same-window buff))
               (when select
                 (select-window wnd))))))))
    (set-process-filter
     proc
     (lambda (proc string)
       (when-let* ((buf (process-buffer proc)))
         (with-current-buffer buf
           (let ((inhibit-read-only t))
             (save-excursion
               (goto-char (point-max))
               (insert string)))))))
    proc))

(defvar Man-ansi-color-basic-faces-vector)
(defvar browse-url-button-regexp)

(defun pyimp--fontify-region (beg end)
  "Fontify the region between BEG and END using Python mode.

Argument BEG is the beginning position of the region to fontify.

Argument END is the ending position of the region to fontify."
  (let* ((str (buffer-substring beg end))
         (fontified-str (with-temp-buffer
                          (delay-mode-hooks
                            (python-mode)
                            (goto-char (point-min))
                            (insert str)
                            (font-lock-ensure)
                            (buffer-string)))))
    (goto-char beg)
    (delete-region beg end)
    (insert fontified-str)))

(defun pyimp--fontify-module-help ()
  "Convert overstriking and underlining to the correct fonts.
Same for the ANSI bold and normal escape sequences."
  (require 'man)
  (require 'browse-url)
  (goto-char (point-min))
  (let ((ansi-color-apply-face-function #'ansi-color-apply-text-property-face)
        (ansi-color-basic-faces-vector Man-ansi-color-basic-faces-vector))
    (ansi-color-apply-on-region (point-min)
                                (point-max)))
  (let ((buffer-undo-list t))
    (if (< (buffer-size)
           (position-bytes (point-max)))
        (progn
          (goto-char (point-min))
          (while (and (search-forward "__\b\b" nil t)
                      (not (eobp)))
            (delete-char -4)
            (put-text-property (point)
                               (1+ (point))
                               'font-lock-face 'Man-underline))
          (goto-char (point-min))
          (while (search-forward "\b\b__" nil t)
            (delete-char -4)
            (put-text-property (1- (point))
                               (point)
                               'font-lock-face 'Man-underline))))
    (goto-char (point-min))
    (while (and (search-forward "_\b" nil t)
                (not (eobp)))
      (delete-char -2)
      (put-text-property (point)
                         (1+ (point)) 'font-lock-face 'Man-underline))
    (goto-char (point-min))
    (while (search-forward "\b_" nil t)
      (delete-char -2)
      (put-text-property (1- (point))
                         (point) 'font-lock-face 'Man-underline))
    (goto-char (point-min))
    (while (re-search-forward "\\(.\\)\\(\b+\\1\\)+" nil t)
      (replace-match "\\1")
      (put-text-property (1- (point))
                         (point) 'font-lock-face 'Man-overstrike))
    (goto-char (point-min))
    (while (re-search-forward "o\b\\+\\|\\+\bo" nil t)
      (replace-match "o")
      (put-text-property (1- (point))
                         (point) 'font-lock-face 'bold))
    (goto-char (point-min))
    (while (re-search-forward "[-|]\\(\b[-|]\\)+" nil t)
      (replace-match "+")
      (put-text-property (1- (point))
                         (point) 'font-lock-face 'bold))
    (goto-char (point-min))
    (while (re-search-forward ".\b" nil t)
      (delete-char -2))
    (goto-char (point-min))
    (unless (string-prefix-p "latin-" current-language-environment t)
      (goto-char (point-min))
      (while (search-forward "­" nil t)
        (replace-match "-")))
    (goto-char (point-min))
    (while (re-search-forward "^\\([[:upper:]][[:upper:]0-9 /-]+\\)$" nil t)
      (put-text-property (match-beginning 0)
                         (match-end 0)
                         'font-lock-face 'Man-overstrike))
    (goto-char (point-min))
    (while (re-search-forward "^[\s\t]+\\(class\\|def\\)[\s]\\([a-z0-9_]+\\)"
                              nil
                              t)
      (put-text-property (match-beginning 1)
                         (match-end 1)
                         'font-lock-face 'font-lock-keyword-face)
      (put-text-property (match-beginning 2)
                         (match-end 2)
                         'font-lock-face 'font-lock-type-face))
    (pcase-dolist
        (`(,section . ,face)
         '(("FUNCTIONS" . font-lock-function-name-face)
           ("CLASSES" . font-lock-function-name-face)
           ("DATA" . font-lock-function-name-face)))
      (goto-char (point-min))
      (when (re-search-forward (concat "^" (regexp-quote section) "\n") nil t)
        (let ((end (save-excursion
                     (re-search-forward "^\\([[:upper:]][[:upper:]0-9 /-]+\\)$"
                                        nil t))))
          (while (re-search-forward "^[\s|]+\\([a-z0-9_.]+\\)[\s]*[(=]" end t 1)
            (put-text-property (match-beginning 1)
                               (match-end 1)
                               'font-lock-face face)))))
    (goto-char (point-max))
    (when (re-search-backward  "^FILE[\n][\s]+\\([^\n]+\\)" nil t
                               1)
      (let ((file (match-string-no-properties 1))
            (beg (match-beginning 1))
            (end (match-end 1)))
        (buttonize-region beg end #'find-file-other-window file)))
    (goto-char (point-max))
    (while (re-search-backward browse-url-button-regexp nil t 1)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (buttonize-region beg end #'browse-url (buffer-substring-no-properties
                                                beg end))))
    (font-lock-update)
    (goto-char (point-max))
    (while (re-search-backward "['`]\\([a-zA-Z_][a-zA-Z0-9_.]+\\)['`]" nil t 1)
      (let ((beg (match-beginning 1))
            (end (match-end 1)))
        (if (string-match-p "self\\." (buffer-substring-no-properties beg end))
            (pyimp--fontify-region beg end)
          (put-text-property beg
                             end
                             'font-lock-face 'font-lock-type-face))))))



(defun pyimp--completing-read-with-preview (prompt collection &optional
                                                   preview-action keymap
                                                   predicate require-match
                                                   initial-input hist def
                                                   inherit-input-method)
  "Read COLLECTION in minibuffer with PROMPT and KEYMAP.
See `completing-read' for PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD."
  (let ((collection (if (stringp (car-safe collection))
                        (copy-tree collection)
                      collection)))
    (minibuffer-with-setup-hook
        (lambda ()
          (when (minibufferp)
            (when keymap
              (let ((map (make-composed-keymap keymap
                                               (current-local-map))))
                (use-local-map map)))
            (when preview-action
              (add-hook 'pre-command-hook (lambda ()
                                            (interactive)
                                            (pyimp--minibuffer-action-no-exit
                                             preview-action))
                        nil t))))
      (completing-read prompt
                       collection
                       predicate
                       require-match initial-input hist
                       def inherit-input-method))))


(defun pyimp--get-modules ()
  "Return a list of Python modules in the project."
  (let* ((project
          (or
           (pyimp--locate-project-root)
           default-directory))
         (files
          (directory-files-recursively
           project
           "\\.py\\'"
           nil
           (lambda (it)
             (or (not pyvenv-virtual-env)
                 (not
                  (file-equal-p it pyvenv-virtual-env))))))
         (sorted-files
          (mapcar (lambda (file)
                    (replace-regexp-in-string
                     "/"
                     "."
                     (if (and pyvenv-virtual-env
                              (file-in-directory-p file pyvenv-virtual-env))
                         (car (last (split-string file  "/site-packages/" t)))
                       (substring-no-properties
                        (expand-file-name (file-name-sans-extension
                                           file))
                        (length project)))))
                  (remove buffer-file-name
                          (sort files
                                (lambda (a b)
                                  (not
                                   (time-less-p
                                    (pyimp--file-modification-time
                                     a)
                                    (pyimp--file-modification-time
                                     b))))))))
         (builtins
          (setq pyimp-builtins-cached (pyimp--list-builtin-modules)))
         (all-modules (append sorted-files builtins)))
    all-modules))

(defun pyimp--read-python-module ()
  "Prompt the user to select a Python module from the project or built-in modules."
  (let* ((project
          (or
           (pyimp--locate-project-root)
           default-directory))
         (files
          (directory-files-recursively
           project
           "\\.py\\'"
           nil
           (lambda (it)
             (or (not pyvenv-virtual-env)
                 (not
                  (file-equal-p it pyvenv-virtual-env))))))
         (sorted-files
          (mapcar (lambda (file)
                    (replace-regexp-in-string
                     "/"
                     "."
                     (if (and pyvenv-virtual-env
                              (file-in-directory-p file pyvenv-virtual-env))
                         (car (last (split-string file  "/site-packages/" t)))
                       (substring-no-properties
                        (expand-file-name (file-name-sans-extension
                                           file))
                        (length project)))))
                  (remove buffer-file-name
                          (sort files
                                (lambda (a b)
                                  (not
                                   (time-less-p
                                    (pyimp--file-modification-time
                                     a)
                                    (pyimp--file-modification-time
                                     b))))))))
         (builtins
          (setq pyimp-builtins-cached (pyimp--list-builtin-modules)))
         (all-modules (append sorted-files builtins)))
    all-modules))



(defun pyimp--insert-import (module &optional sym)
  "Insert a Python import statement for MODULE and optionally SYM if not present.

Argument MODULE is the name of the module to import.

Optional argument SYM is the specific symbol to import from the module."
  (let* ((items (treesit-node-children (treesit-buffer-root-node 'python)))
         (results (delq nil (mapcar
                             (lambda (it)
                               (let ((type (treesit-node-type it)))
                                 (when (equal type (if sym
                                                       "import_from_statement"
                                                     "import_statement"))
                                   (let* ((children
                                           (treesit-node-children it))
                                          (from (seq-find (lambda (c)
                                                            (equal
                                                             (treesit-node-type
                                                              c)
                                                             (if sym "from"
                                                               "import")))
                                                          children))
                                          (rest (cdr (memq from children)))
                                          (path
                                           (treesit-node-text (car rest)))
                                          (names
                                           (when sym
                                             (let ((impel
                                                    (seq-find
                                                     (lambda (c)
                                                       (equal
                                                        (treesit-node-type
                                                         c)
                                                        "import"))
                                                     rest)))
                                               (cdr (memq impel rest))))))
                                     (when (equal path module)
                                       (cons it names))))))
                             items)))
         (item (car results)))
    (cond ((not item)
           (let ((imp (seq-find (lambda (it)
                                  (member (treesit-node-type it)
                                          '("import_from_statement"
                                            "import_statement")))
                                (reverse items)))
                 (pref)
                 (suff))
             (if (not imp)
                 (progn
                   (goto-char (point-min))
                   (if (not (member
                             (treesit-node-type (treesit-node-at (point)))
                             '("string-start" "comment")))
                       (setq suff "\n")
                     (goto-char (treesit-node-end (car items)))
                     (setq pref "\n")))
               (goto-char (treesit-node-end imp))
               (setq pref "\n"))
             (unless (looking-at "\n")
               "\n")
             (insert (concat pref
                             (if sym
                                 (format "from %s import %s" module sym)
                               (format "import %s" module))
                             suff))))
          ((and item (not sym))
           (message "Module %s already imported" module))
          (t
           (let ((names (cdr item)))
             (if (member sym (mapcar #'treesit-node-text names))
                 (message "%s already imported" sym)
               (let ((last-node (car (last names))))
                 (when (equal (treesit-node-type last-node) ")")
                   (let ((dropped (butlast names 1)))
                     (setq last-node (car (last dropped)))))
                 (goto-char (treesit-node-end last-node))
                 (if (equal  ","
                             (treesit-node-text last-node))
                     (insert (concat "\s" sym))
                   (insert ", " sym)))))))))

;;;###autoload
(defun pyimp-import (module symb)
  "Insert or update an import statement for a specified Python MODULE and symbol.

Argument MODULE is the name of the Python module to import from.

Argument SYMB is the symbol to import from the specified module."
  (interactive
   (let* ((default-directory (or
                              (pyimp--locate-project-root)
                              default-directory))
          (mod
           (pyimp--completing-read-with-preview "Module: "
                                                (pyimp--get-modules)
                                                nil
                                                pyimp-modules-minibuffer-map))
          (syms (append (list "*whole module*")
                        (pyimp--extract-symbols-from-module mod))))
     (list mod
           (completing-read "Symbol: " syms))))
  (save-excursion
    (pyimp--insert-import module
                          (unless (equal symb "*whole module*")
                            symb))))

(provide 'pyimp)
;;; pyimp.el ends here