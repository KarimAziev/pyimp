;;; pyimp.el --- Add import statements in Python -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/pyimp
;; Version: 0.2.0
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

;; This library helps to insert Python import statements.

;; Usage:

;; Invoke the command `pyimp-import' to insert an import statement:

;; 1. You will be prompted for a Python module. pyimp searches your project,
;; installed libraries, and built-in modules.
;; 2. After selecting a module, pyimp presents a list of exported symbols from
;; the module. You can choose a single symbol or mark multiple symbols (if you
;; enable minibuffer marking, see `pyimp-allow-minibuffer-marking').
;; 3. Once selected, `pyimp' automatically inserts or updates the appropriate
;; import statement in your Python file.

;; If you ever need to read module documentation or review what's been imported,
;; you can use the provided module description functionality (bound under `C-j'
;; by default).

;;; Code:

(require 'project)
(require 'pyvenv)
(require 'treesit)


(defcustom pyimp-files-sorting-threshold 5000
  "Threshold for enabling sorting in project files.

If the number of project files exceeds this threshold, sorting will be
inhibited.

Set this to nil to always allow sorting, or to a numeric value to
specify the threshold."
  :group 'pyimp
  :type '(radio
          (natnum :tag "Threshold")
          (const :tag "Always allow sorting" nil)))

(defcustom pyimp-extract-module-export-functions '(pyimp-parse-exposed-exports-in-module
                                                   pyimp-eval-python-to-extract-symbols-from-module
                                                   pyimp-extract-non-imported-top-nodes)
  "List of functions to extract exported symbols from a Python module.

Each function in the list should accept a single argument,the module, and return
a list of symbols that are considered exports.

The functions are executed in order until one successfully returns
a non-nil value.

To modify the behavior, add or remove functions from the list
according to the desired export extraction logic."
  :group 'pyimp
  :type 'hook)

(defcustom pyimp-allow-minibuffer-marking nil
  "Permission to mark symbols in the minibuffer during import selection.

Determines whether marking is allowed in the minibuffer during symbol
selection for Python imports. When set to true, users can select multiple
symbols from a module using marking commands in the minibuffer.

If false, only a single symbol can be selected at a time.

See `pyimp-multiple-symbols-minibuffer-map'."
  :group 'pyimp
  :type 'boolean)

(defconst pyimp--all-modules-code "\
import pkgutil
import sys

installed_modules = [
    module.name for module in pkgutil.iter_modules() if module.name.isidentifier()
]
builtin_modules = [name for name in sys.builtin_module_names if name.isidentifier()]

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


(defconst pyimp--module-to-file-name-code "
import importlib.util

spec = importlib.util.find_spec(%s)
if spec is not None:
    print(spec.origin)
"
  "String containing Python code to find a module's file path.")


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


(defun pyimp-eval-python-to-extract-symbols-from-module (module)
  "Extract and return non-private symbols from a Python module.

Argument MODULE is the name of the Python module from which to extract symbols."
  (let* ((code (format pyimp--module-symbols-script module module)))
    (pyimp--exec-python-code-to-elisp code)))

(defun pyimp-extract-exports-from-module (module)
  "Extract exported symbols from the given MODULE."
  (run-hook-with-args-until-success 'pyimp-extract-module-export-functions
                                    module))

(defun pyimp--module-to-file-name (module)
  "Convert a Python MODULE name to its corresponding file path.

Argument MODULE is the name of the Python module to be converted into a file
name."
  (let ((cmd (if (bound-and-true-p python-interpreter)
                 python-interpreter
               "python3")))
    (unless (file-name-absolute-p cmd)
      (setq cmd (executable-find cmd)))
    (with-temp-buffer
      (let ((status (call-process
                     cmd nil t nil
                     "-c" (format pyimp--module-to-file-name-code
                                  (prin1-to-string
                                   module))))
            (file))
        (when (zerop status)
          (setq file (string-trim-right (buffer-string)))
          (when (and file (file-name-absolute-p file)
                     (file-exists-p file))
            file))))))

(defun pyimp--list-builtin-modules ()
  "Return a list of all installed and built-in Python modules."
  (pyimp--exec-python-code-to-elisp pyimp--all-modules-code))

(defun pyimp--exec-python-code-to-elisp (code)
  "Execute Python CODE and return the trimmed output or nil on error.

Argument CODE is the Python code to be executed as a string."
  (let ((cmd (if (bound-and-true-p python-interpreter)
                 python-interpreter
               (executable-find "python3"))))
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

(defvar pyimp-multiple-symbols-minibuffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map
                (kbd "C-<return>")
                #'pyimp-minibuffer-done)
    (define-key map
                (kbd "RET")
                #'pyimp-minibuffer-done)
    (define-key map (kbd "C-M-j")
                #'pyimp-minibuffer-done)
    (define-key map (kbd "C-SPC")
                #'pyimp-minibuffer-mark)
    map))



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

(defvar-local pyimp-modules nil)

(defvar pyimp--files-cache (make-hash-table :test 'equal))

(defun pyimp--get-file-cache (cache-key)
  "Retrieve cached data if file hasn't changed.

Argument CACHE-KEY is a key used to retrieve the cache entry from
`pyimp--files-cache'."
  (let* ((cache (gethash cache-key pyimp--files-cache))
         (cache-tick (and cache (plist-get cache :tick)))
         (tick (file-attribute-modification-time (file-attributes
                                                  cache-key
                                                  'string))))
    (when (equal cache-tick tick)
      (plist-get cache :cache))))

(defun pyimp--set-file-cache (path content)
  "Cache JavaScript file CONTENT with modification time.

Argument PATH is the file path for which to set the cache.

Argument CONTENT is the content to be cached for the specified file."
  (let* ((cache (gethash path pyimp--files-cache))
         (tick (file-attribute-modification-time (file-attributes
                                                  path
                                                  'string))))
    (setq cache (list :tick tick
                      :cache content))
    (puthash path cache
             pyimp--files-cache)
    (plist-get cache :cache)))

(defun pyimp--file-name-to-module-path (file &optional abs-prefix)
  "Convert FILE path to module path, using ABS-PREFIX if provided.

Argument FILE is the name of the file to be converted into a module path.

Optional argument ABS-PREFIX is a prefix used when FILE is an absolute path."
  (let ((module (if (and abs-prefix
                         (file-name-absolute-p file))
                    (substring-no-properties
                     (expand-file-name
                      (file-name-sans-extension
                       file))
                     (length
                      (expand-file-name
                       abs-prefix)))
                  (file-name-sans-extension file))))
    (replace-regexp-in-string "/" "." (replace-regexp-in-string
                                       "^\\|/__init__\\'" "" module))))

(defun pyimp--get-site-packages (lib-dir &optional force)
  "Retrieve and cache Python module paths from a specified directory.

Argument LIB-DIR is the directory path to search for Python
site-packages.

Optional argument FORCE, when non-nil, bypasses the cache and forces a fresh
directory scan."
  (setq lib-dir (file-name-as-directory (expand-file-name lib-dir)))
  (or (and (not force)
           (pyimp--get-file-cache lib-dir))
      (let ((dirs (directory-files lib-dir t
                                   directory-files-no-dot-files-regexp))
            (result))
        (while dirs
          (let ((abs-dir (pop dirs)))
            (cond ((and (file-directory-p abs-dir))
                   (when (file-accessible-directory-p abs-dir)
                     (let
                         ((files
                           (delq nil
                                 (mapcar
                                  (lambda (file)
                                    (unless (and (string=
                                                  (file-name-base file)
                                                  "__init__")
                                                 (equal  (file-name-extension
                                                          file)
                                                         "py"))
                                      (let ((module-path
                                             (pyimp--file-name-to-module-path
                                              file
                                              lib-dir)))
                                        (cons module-path file))))
                                  (directory-files-recursively abs-dir
                                                               "\\.py\\'"
                                                               t
                                                               (lambda
                                                                 (subdirectory)
                                                                 (file-exists-p
                                                                  (expand-file-name
                                                                   "__init__.py"
                                                                   subdirectory))))))))
                       (when files
                         (setq files (push
                                      (cons (pyimp--file-name-to-module-path abs-dir lib-dir)
                                            abs-dir)
                                      files))
                         (setq result (nconc result files))))))
                  ((and (equal (file-name-extension abs-dir) ".py"))
                   (setq result (push abs-dir result))))))
        (pyimp--set-file-cache lib-dir result)
        result)))

(defun pyimp--get-python-libs ()
  "Retrieve Python library directories containing `site-packages'."
  (when-let* ((ppath (executable-find "python3"))
              (dirs (directory-files
                     (expand-file-name "lib"
                                       (file-name-parent-directory
                                        (file-name-parent-directory ppath)))
                     t directory-files-no-dot-files-regexp)))
    (let ((result))
      (dolist (dir dirs)
        (let ((site-packages-dir (expand-file-name "site-packages" dir)))
          (when (file-exists-p site-packages-dir)
            (setq result (nconc result (pyimp--get-site-packages site-packages-dir))))))
      result)))

(defun pyimp--project-files (project-dir)
  "Return an alist of Python files in PROJECT-DIR, excluding virtual envs.

Argument PROJECT-DIR is the directory path where Python files are searched."
  (let* ((files
          (directory-files-recursively
           project-dir
           "\\.py\\'"
           nil
           (lambda (it)
             (or (not pyvenv-virtual-env)
                 (not
                  (file-equal-p it pyvenv-virtual-env))))))
         (sorted-files
          (if (>= (length files) pyimp-files-sorting-threshold)
              files
            (sort files
                  (lambda (a b)
                    (not
                     (time-less-p
                      (pyimp--file-modification-time
                       a)
                      (pyimp--file-modification-time
                       b)))))))
         (alist (mapcar (lambda (file)
                          (cons
                           (pyimp--file-name-to-module-path
                            file project-dir)
                           file))
                        (remove buffer-file-name
                                sorted-files))))
    alist))

(defun pyimp--modules-completions-table ()
  "Return a completion table for Python modules in the current project."
  (let* ((project
          (or
           (pyimp--locate-project-root)
           default-directory))
         (alist (pyimp--project-files project))
         (builtins
          (setq pyimp-builtins-cached
                (mapcar #'list
                        (seq-remove (lambda (it) (assoc-string it alist))
                                    (pyimp--list-builtin-modules)))))
         (installed (pyimp--get-python-libs))
         (all-modules-alist (append alist builtins installed))
         (all-modules (mapcar #'car all-modules-alist))
         (longest (apply #'max (or (mapcar #'length all-modules)
                                   (list 1))))
         (root (treesit-buffer-root-node 'python))
         (imports-from (pyimp--extract-import-from-statements root))
         (module-imports (pyimp--extract-imports-statements root))
         (aliased-imports (pyimp--extract-aliased-imports root))
         (annotfmt (concat
                    (propertize " " 'display
                                `(space :align-to ,(1+
                                                    longest)))
                    " %s"))
         (annotf (lambda (str)
                   (let* ((type
                           (cond ((assoc-string str alist)
                                  "project")
                                 ((assoc-string str builtins)
                                  "builtin")
                                 ((assoc-string str installed)
                                  "library")))
                          (alias-imp)
                          (imported-syms)
                          (imported-capture
                           (cond ((setq alias-imp (assoc-string str aliased-imports))
                                  (format "imported as %s" (cdr alias-imp)))
                                 ((setq imported-syms (assoc-string str imports-from))
                                  (format "imported %d symbols"
                                          (length
                                           (cdr
                                            imported-syms))))
                                 ((assoc-string str module-imports)
                                  (format "imported")))))
                     (format annotfmt (string-join
                                       (delq nil
                                             (list type
                                                   imported-capture))
                                       " "))))))
    (setq pyimp-modules all-modules-alist)
    (lambda (str pred action)
      (if (eq action 'metadata)
          `(metadata
            (annotation-function . ,annotf))
        (complete-with-action action all-modules str pred)))))


(defun pyimp--extract-exposed-exports ()
  "Return string literals from `__all__' assignment in current buffer."
  (when-let* ((root (treesit-buffer-root-node 'python))
              (candidates (treesit-query-capture
                           root
                           '((expression_statement
                              (assignment left: (identifier) @name
                               right: (list) @items)))))
              (groupped (seq-split candidates 2))
              (all-var (seq-find
                        (lambda (it)
                          (let* ((all-node (cdr (assq 'name
                                                      it)))
                                 (parent (treesit-node-parent
                                          (treesit-node-parent (treesit-node-parent
                                                                all-node)))))
                            (and (equal "__all__"
                                        (treesit-node-text all-node))
                                 (equal "module" (treesit-node-type parent)))))
                        (reverse groupped)))
              (items-node (cdr (assq 'items all-var)))
              (strs (delq nil
                          (mapcar (lambda (node)
                                    (when (equal (treesit-node-type node)
                                                 "string")
                                      (when-let* ((str-content
                                                   (car (treesit-filter-child
                                                         node
                                                         (lambda (n)
                                                           (equal
                                                            "string_content"
                                                            (treesit-node-type
                                                             n)))))))
                                        (treesit-node-text str-content t))))
                                  (treesit-node-children
                                   items-node t)))))
    strs))



(defun pyimp--get-top-parent-node (node)
  "Return top-level parent node for NODE."
  (let ((top-level-children (treesit-node-children
                             (treesit-buffer-root-node 'python))))
    (seq-find  (lambda (top-child)
                 (delq nil
                       (flatten-list
                        (treesit-induce-sparse-tree
                         top-child
                         (lambda (it)
                           (treesit-node-eq
                            it
                            node))))))
               top-level-children)))

(defun pyimp-parse-exposed-exports-in-module (module)
  "Extract and return `__all__' exports from a Python MODULE's file.

Argument MODULE is the name of the Python module to extract exposed exports
from."
  (when-let* ((file (pyimp--module-to-file-name module)))
    (let* ((buff (get-file-buffer file)))
      (if (buffer-live-p buff)
          (with-current-buffer buff
            (pyimp--extract-exposed-exports))
        (with-temp-buffer
          (insert-file-contents file)
          (pyimp--extract-exposed-exports))))))

(defun pyimp--extract-non-imports-nodes ()
  "Filter out import and certain string nodes from the root node's children."
  (let ((items (treesit-node-children (treesit-buffer-root-node 'python))))
    (seq-remove
     (lambda (node)
       (let ((node-type (treesit-node-type node)))
         (or (member node-type '("comment" "string_start"
                                 "import_from_statement" "import_statement"))
             (and (member node-type '("expression_statement"))
                  (equal (treesit-node-type (treesit-node-child node 0))
                         "string")))))
     items)))


(defun pyimp--extract-definition-id-text (node)
  "Extract and return identifier text from a syntax tree NODE.

Argument NODE is a tree-sitter node from which the identifier text is extracted."
  (pcase (treesit-node-type node)
    ((or "class_definition" "function_definition")
     (let ((child (treesit-node-child node 0 t)))
       (pcase (treesit-node-type child)
         ("identifier" (treesit-node-text child t)))))
    ("decorated_definition"
     (when-let* ((res
                  (car
                   (seq-drop-while (lambda (item)
                                     (pcase (treesit-node-type item)
                                       ("decorator" item)))
                                   (treesit-node-children node
                                                          t)))))
       (pyimp--extract-definition-id-text res)))
    ("expression_statement"
     (let ((child (treesit-node-child node 0 t)))
       (pcase (treesit-node-type child)
         ("assignment"
          (when-let* ((id (treesit-node-child child 0 t)))
            (pcase (treesit-node-type id)
              ("identifier" (treesit-node-text
                             id t)))))
         ("identifier" (treesit-node-text child t)))))))

(defun pyimp--non-imports-top-nodes-ids ()
  "Return identifiers of top-level nodes excluding import statements."
  (delq nil (mapcar #'pyimp--extract-definition-id-text
                    (pyimp--extract-non-imports-nodes))))

(defun pyimp-extract-non-imported-top-nodes (module)
  "Extract top-level nodes from a MODULE file that aren't imports.

Argument MODULE is the name of the Python module to be processed."
  (when-let* ((file (pyimp--module-to-file-name module)))
    (let* ((buff (get-file-buffer file)))
      (if (buffer-live-p buff)
          (with-current-buffer buff
            (pyimp--non-imports-top-nodes-ids))
        (with-temp-buffer
          (insert-file-contents file)
          (pyimp--non-imports-top-nodes-ids))))))

(defun pyimp--imported-nodes-in-module (module)
  "Extract imported nodes from a MODULE file that aren't imports.

Argument MODULE is the name of the Python module to be processed."
  (when-let* ((file (pyimp--module-to-file-name module)))
    (let* ((buff (get-file-buffer file)))
      (if (buffer-live-p buff)
          (with-current-buffer buff
            (list (pyimp--extract-import-from-statements)
                  (pyimp--extract-aliased-imports)
                  (pyimp--extract-imports-statements)))
        (with-temp-buffer
          (insert-file-contents file)
          (list (pyimp--extract-import-from-statements)
                (pyimp--extract-aliased-imports)
                (pyimp--extract-imports-statements)))))))

(defun pyimp--extract-import-from-statements (&optional root-node)
  "Extract imports of form \"from MODULE import SYM\" from a syntax tree.

Optional argument ROOT-NODE is the root node from which to start the
query; if not provided, the root node of the current buffer is used.

Result is alist of modules and imported symbols.

Symbols is also a list of the imported symbols and possibly the aliased name."
  (let ((statements (treesit-query-capture
                     (or root-node (treesit-buffer-root-node 'python))
                     '((import_from_statement
                        module_name: (dotted_name) @module)))))
    (mapcar (pcase-lambda (`(,_ . ,module))
              (let ((syms)
                    (node module))
                (while (setq node (treesit-node-next-sibling node t))
                  (when-let* ((item
                               (pcase (treesit-node-type node)
                                 ("aliased_import"
                                  (let ((id)
                                        (dotted-name))
                                    (dolist (n (treesit-node-children node t))
                                      (pcase (treesit-node-type n)
                                        ("dotted_name"
                                         (setq dotted-name (treesit-node-text n t)))
                                        ("identifier"
                                         (setq id (treesit-node-text n t)))))
                                    (when (and id dotted-name)
                                      (cons dotted-name id))))
                                 ("dotted_name"
                                  (let ((text (treesit-node-text node t)))
                                    (cons text nil))))))
                    (push item syms)))
                (cons (treesit-node-text module t)
                      (nreverse syms))))
            statements)))

(defun pyimp--extract-aliased-imports (&optional root-node)
  "Extract imports of form \"import MODULE as ALIAS\" from a syntax tree.

Optional argument ROOT-NODE is the root node to start the query from.

Result is alist of modules and aliases."
  (mapcar (lambda (it)
            (cons (treesit-node-text (cdr (assq 'module-name it)) t)
                  (treesit-node-text (cdr (assq 'alias-name it)) t)))
          (seq-split (treesit-query-capture
                      (or root-node
                          (treesit-buffer-root-node 'python))
                      '((import_statement
                         name: (aliased_import name:
                                (dotted_name)
                                @module-name alias:
                                (identifier) @alias-name))))
                     2)))

(defun pyimp--extract-imports-statements (&optional root-node)
  "Extract imports of form \"import MODULE\" from a syntax tree.

Optional argument ROOT-NODE is the root node from which to start the query.

Result is alist of (MODULE . nil)."
  (mapcar (pcase-lambda (`(,_ . ,v))
            (let ((text (treesit-node-text v t)))
              (cons text nil)))
          (treesit-query-capture
           (or root-node (treesit-buffer-root-node))
           '((import_statement
              name: (dotted_name) @module)))))

(defun pyimp-minibuffer-mark ()
  "Throw the symbol \\='mark to exit a catch block."
  (interactive)
  (throw 'action
         `(mark . ,(cdr (pyimp--minibuffer-get-current-candidate)))))


(defun pyimp-minibuffer-done ()
  "Throw the symbol \\='done to exit a catch block."
  (interactive)
  (throw 'done (cdr (pyimp--minibuffer-get-current-candidate))))

(defun pyimp--read-symb (module)
  "Prompt the user to select a symbol to import from a specified module.

Argument MODULE is the name of the module from which symbols are to be
imported."
  (pcase-let*
      ((`(,reimported-syms ,reimported-aliased ,reimports)
        (pyimp--imported-nodes-in-module module))
       (imports-from (pyimp--extract-import-from-statements))
       (imported-syms (cdr (assoc-string module imports-from)))
       (syms (append (list "*whole module*")
                     (pyimp-extract-exports-from-module module)))
       (longest (apply #'max (or (mapcar #'length syms)
                                 (list 1))))
       (annotfmt (concat
                  (propertize " " 'display
                              `(space :align-to ,(1+
                                                  longest)))
                  " %s"))
       (annotf
        (pyimp--make-symbols-annotation-fn annotfmt
                                           imported-syms
                                           reimported-aliased
                                           reimports
                                           reimported-syms)))
    (completing-read (format "Import symbol from %s: " module)
                     (lambda (str pred action)
                       (if (eq action 'metadata)
                           `(metadata
                             (annotation-function . ,annotf))
                         (complete-with-action action syms str pred))))))

(defun pyimp--make-symbols-annotation-fn (annotfmt imported-syms
                                                   reimported-aliased reimports
                                                   reimported-syms &optional
                                                   marked-syms)
  "Create a lambda function to annotate symbols with import status.

Argument ANNOTFMT is a format string used to create the annotation.

Argument IMPORTED-SYMS is an association list of symbols and their
imported aliases in current buffer.

Argument REIMPORTED-ALIASED is an association list of symbols and their
reimported aliases in the module to import from.

Argument REIMPORTS is an association list of symbols that are reimported
in the module to import from.

Argument REIMPORTED-SYMS is a list of reimported symbols with their
modules in the module to import from.

Optional argument MARKED-SYMS is a list of symbols marked for import."
  (lambda (str)
    (let* ((cell (assoc-string str imported-syms))
           (imported-capture
            (cond ((member str marked-syms)
                   "(will be imported)")
                  ((cdr cell)
                   (format "imported as %s" (cdr cell)))
                  (cell (format "imported"))))
           (reimported-sym)
           (reimport-capture
            (cond ((rassoc str reimported-aliased)
                   (format "(reimport from %s)"
                           (car
                            (rassoc str
                                    reimported-aliased))))
                  ((assoc str reimports)
                   "(reimport)")
                  ((setq reimported-sym (seq-find
                                         (pcase-lambda (`(,_mod . ,items))
                                           (or (rassoc str items)
                                               (assoc-string str items)))
                                         reimported-syms))
                   (pcase-let ((`(,mod . ,items)
                                reimported-sym))
                     (if (rassoc str items)
                         (format "(reimport of %s from %s)"
                                 (car (rassoc str items))
                                 mod)
                       (format "(reimport from %s)"
                               mod)))))))
      (format annotfmt (string-join
                        (delq nil
                              (list
                               imported-capture
                               reimport-capture))
                        " ")))))

(defun pyimp--read-multiple-syms (module)
  "Prompt user to select symbols to import from a MODULE, with completion.

Argument MODULE is the name of the module from which to import symbols."
  (pcase-let* ((`(,reimported-syms ,reimported-aliased ,reimports)
                (pyimp--imported-nodes-in-module module))
               (whole-mod-str "*whole module*")
               (choices (append (list whole-mod-str)
                                (pyimp-extract-exports-from-module module)))
               (prompt (format "Import symbol from %s: " module))
               (imports-from (pyimp--extract-import-from-statements))
               (imported-syms (cdr (assoc-string module imports-from)))
               (longest (apply #'max (or (mapcar #'length choices)
                                         (list 1))))
               (annotfmt (concat
                          (propertize " " 'display
                                      `(space :align-to ,(1+
                                                          longest)))
                          "%s"))
               (marked-syms)
               (helpstr)
               (key)
               (key-descr)
               (result))
    (setq key (where-is-internal
               #'pyimp-minibuffer-mark
               pyimp-multiple-symbols-minibuffer-map
               t
               t
               t))
    (setq key-descr (if key (key-description key)
                      "M-x pyimp-minibuffer-mark"))
    (setq helpstr (format " (to import multiple symbols use %s) " key-descr))
    (setq result
          (catch 'done (while
                           (let ((curr
                                  (catch 'action
                                    (minibuffer-with-setup-hook
                                        (lambda ()
                                          (let ((map
                                                 (make-composed-keymap
                                                  pyimp-multiple-symbols-minibuffer-map
                                                  (current-local-map))))
                                            (use-local-map
                                             map)))
                                      (let* ((composed-prompt
                                              (concat prompt
                                                      (if
                                                          (string-suffix-p " "
                                                                           prompt)
                                                          (string-trim-left
                                                           helpstr)
                                                        helpstr)
                                                      (if marked-syms
                                                          (format
                                                           "(%d marked)"
                                                           (length
                                                            marked-syms))
                                                        "")
                                                      " "))
                                             (annotf
                                              (pyimp--make-symbols-annotation-fn annotfmt
                                                                                 imported-syms
                                                                                 reimported-aliased
                                                                                 reimports
                                                                                 reimported-syms
                                                                                 marked-syms)))
                                        (if marked-syms
                                            (setq choices (remove whole-mod-str choices))
                                          (unless (member whole-mod-str choices)
                                            (setq choices (push whole-mod-str choices))))
                                        (completing-read composed-prompt
                                                         (lambda
                                                           (str pred action)
                                                           (if (eq action
                                                                   'metadata)
                                                               `(metadata
                                                                 (annotation-function
                                                                  . ,annotf))
                                                             (complete-with-action
                                                              action choices
                                                              str
                                                              pred)))))))))
                             (pcase (car-safe curr)
                               ('mark
                                (when-let* ((sym (cdr curr)))
                                  (cond ((equal sym whole-mod-str)
                                         (message "Whole module cannot be marked"))
                                        ((member sym marked-syms)
                                         (setq marked-syms (remove sym marked-syms)))
                                        (t
                                         (setq marked-syms
                                               (append marked-syms (list sym)))))
                                  t))
                               ((guard (and (stringp curr)
                                            (not choices)))
                                (setq choices (append choices (list curr)))
                                nil)
                               ((guard (stringp curr))
                                curr))))))
    (or marked-syms
        (when (stringp result) result))))



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
                               (let
                                   ((alias (string-trim (read-string (format
                                                                      "import %s as (empty for no alias): "
                                                                      module)))))
                                 (if (and (not (string-empty-p alias))
                                          (not (string= alias module)))
                                     (format "import %s as %s" module alias)
                                   (format "import %s" module))))
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
  "Prompt user to import a MODULE and symbols, then insert import statement.

Argument MODULE is the name of the module to import.

Argument SYMB is the symbol or list of symbols to import from the module."
  (interactive
   (let* ((default-directory (or
                              (pyimp--locate-project-root)
                              default-directory))
          (mod
           (pyimp--completing-read-with-preview "Module: "
                                                (pyimp--modules-completions-table)
                                                nil
                                                pyimp-modules-minibuffer-map))
          (sym (if pyimp-allow-minibuffer-marking
                   (pyimp--read-multiple-syms mod)
                 (pyimp--read-symb mod))))
     (list mod
           sym)))
  (save-excursion
    (when (and symb (not (listp symb)))
      (setq symb (list symb)))
    (dolist (sym symb)
      (pyimp--insert-import
       module
       (unless (equal sym "*whole module*")
         sym)))))

(provide 'pyimp)
;;; pyimp.el ends here