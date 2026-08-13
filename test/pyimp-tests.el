;;; pyimp-tests.el --- Tests for pyimp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'pyimp)

(defmacro pyimp-tests--with-temp-directory (directory &rest body)
  "Bind DIRECTORY to a temporary directory while evaluating BODY."
  (declare (indent 1) (debug (symbolp body)))
  `(let ((,directory (make-temp-file "pyimp-tests-" t)))
     (unwind-protect
         (progn ,@body)
       (delete-directory ,directory t))))

(defun pyimp-tests--write-file (file &optional contents)
  "Create FILE and its parents, inserting CONTENTS when non-nil."
  (make-directory (file-name-directory file) t)
  (with-temp-file file
    (when contents
      (insert contents)))
  file)

(ert-deftest pyimp-file-name-to-module-path-validates-components ()
  (pyimp-tests--with-temp-directory root
    (should
     (equal (pyimp--file-name-to-module-path
             (expand-file-name "package/module.py" root) root)
            "package.module"))
    (should
     (equal (pyimp--file-name-to-module-path
             (expand-file-name "package/__init__.py" root) root)
            "package"))
    (should-not
     (pyimp--file-name-to-module-path
      (expand-file-name "package/bad-name.py" root) root))
    (should-not
     (pyimp--file-name-to-module-path
      (expand-file-name "package/class.py" root) root))
    (should-not
     (pyimp--file-name-to-module-path
      (expand-file-name "../outside.py" root) root))))

(ert-deftest pyimp-project-files-excludes-ignored-artifact-trees ()
  (pyimp-tests--with-temp-directory root
    (let* ((package-init
            (pyimp-tests--write-file
             (expand-file-name "package/__init__.py" root)))
           (module
            (pyimp-tests--write-file
             (expand-file-name "package/module.py" root)))
           (untracked
            (pyimp-tests--write-file
             (expand-file-name "package/untracked.py" root)))
           (generated
            (pyimp-tests--write-file
             (expand-file-name "package/generated.py" root)))
           (invalid
            (pyimp-tests--write-file
             (expand-file-name "package/bad-name.py" root)))
           (build-copy
            (pyimp-tests--write-file
             (expand-file-name "build/lib/package/module.py" root)))
           (project-inventory
            (list package-init module untracked))
           (buffer-file-name nil))
      (cl-letf (((symbol-function 'project-files)
                 (lambda (_project &optional _directories)
                   project-inventory)))
        (let ((modules
               (pyimp--project-files root 'fake-project (list root))))
          (should (assoc "package.module" modules))
          (should (assoc "package.untracked" modules))
          ;; Ignored/generated files inside a real package remain importable.
          (should (equal (cdr (assoc "package.generated" modules)) generated))
          (should-not (assoc "package.bad-name" modules))
          (should-not (seq-find
                       (lambda (candidate)
                         (equal (cdr candidate) invalid))
                       modules))
          ;; An ignored non-package tree is never scanned merely because it
          ;; contains a copied package deeper down.
          (should-not (assoc "build.lib.package.module" modules))
          (should-not (seq-find
                       (lambda (candidate)
                         (equal (cdr candidate) build-copy))
                       modules)))))))

(ert-deftest pyimp-project-files-honors-vc-inventory ()
  (skip-unless (executable-find "git"))
  (pyimp-tests--with-temp-directory root
    (let* ((default-directory root)
           (package-init
            (pyimp-tests--write-file
             (expand-file-name "package/__init__.py" root)))
           (tracked
            (pyimp-tests--write-file
             (expand-file-name "package/tracked.py" root)))
           (untracked
            (pyimp-tests--write-file
             (expand-file-name "package/untracked.py" root)))
           (generated
            (pyimp-tests--write-file
             (expand-file-name "package/generated.py" root)))
           (_build-copy
            (pyimp-tests--write-file
             (expand-file-name "build/lib/package/tracked.py" root)))
           (_gitignore
            (pyimp-tests--write-file
             (expand-file-name ".gitignore" root)
             "build/\n/package/generated.py\n"))
           (buffer-file-name nil)
           (project-vc-include-untracked t))
      (should (zerop (call-process "git" nil nil nil "init" "--quiet")))
      (should (zerop
               (call-process "git" nil nil nil
                             "add" package-init tracked ".gitignore")))
      (let* ((project (project-current nil root))
             (modules (pyimp--project-files root project)))
        (should project)
        (should (assoc "package.tracked" modules))
        (should (equal (cdr (assoc "package.untracked" modules)) untracked))
        ;; Regular-package supplementation retains intentional generated
        ;; modules even when their files are ignored by version control.
        (should (equal (cdr (assoc "package.generated" modules)) generated))
        (should-not
         (seq-find (lambda (candidate)
                     (string-prefix-p "build." (car candidate)))
                   modules))))))

(ert-deftest pyimp-project-files-omits-current-buffer ()
  (pyimp-tests--with-temp-directory root
    (let* ((package-init
            (pyimp-tests--write-file
             (expand-file-name "package/__init__.py" root)))
           (current
            (pyimp-tests--write-file
             (expand-file-name "package/current.py" root)))
           (other
            (pyimp-tests--write-file
             (expand-file-name "package/other.py" root)))
           (buffer-file-name current))
      (cl-letf (((symbol-function 'project-files)
                 (lambda (_project &optional _directories)
                   (list package-init current other))))
        (let ((modules
               (pyimp--project-files root 'fake-project (list root))))
          (should-not (assoc "package.current" modules))
          (should (assoc "package.other" modules)))))))

(ert-deftest pyimp-project-files-resolves-src-layout ()
  (pyimp-tests--with-temp-directory root
    (let* ((src (file-name-as-directory (expand-file-name "src" root)))
           (package-init
            (pyimp-tests--write-file
             (expand-file-name "acme/__init__.py" src)))
           (module
            (pyimp-tests--write-file
             (expand-file-name "acme/widget.py" src)))
           (buffer-file-name module))
      (cl-letf (((symbol-function 'project-files)
                 (lambda (_project &optional _directories)
                   (list package-init module))))
        (let ((roots (pyimp--project-import-roots root)))
          (should (equal (car roots) src))
          (let ((buffer-file-name nil))
            (let ((modules
                   (pyimp--project-files root 'fake-project roots)))
              (should (assoc "acme.widget" modules))
              (should-not (assoc "src.acme.widget" modules)))))))))

(ert-deftest pyimp-sort-python-files-honors-nil-threshold ()
  (let ((pyimp-files-sorting-threshold nil))
    (cl-letf (((symbol-function 'pyimp--file-modification-time)
               (lambda (_file) '(0 0 0 0))))
      (should (equal (pyimp--sort-python-files '("b.py" "a.py"))
                     '("a.py" "b.py"))))))

(ert-deftest pyimp-sort-python-files-skips-large-inventories ()
  (let ((pyimp-files-sorting-threshold 1))
    (should (equal (pyimp--sort-python-files '("b.py" "a.py"))
                   '("b.py" "a.py")))))

(ert-deftest pyimp-get-site-packages-includes-top-level-modules ()
  (pyimp-tests--with-temp-directory site-packages
    (let ((top-level
           (pyimp-tests--write-file
            (expand-file-name "standalone.py" site-packages)))
          (package-init
           (pyimp-tests--write-file
            (expand-file-name "package/__init__.py" site-packages)))
          (child
           (pyimp-tests--write-file
            (expand-file-name "package/child.py" site-packages)))
          (invalid
           (pyimp-tests--write-file
            (expand-file-name "bad-name.py" site-packages))))
      (let ((modules (pyimp--get-site-packages site-packages t)))
        (should (equal (cdr (assoc "standalone" modules)) top-level))
        (should (file-equal-p (cdr (assoc "package" modules))
                              (file-name-directory package-init)))
        (should (equal (cdr (assoc "package.child" modules)) child))
        (should-not (assoc "package.__init__" modules))
        (should-not (seq-find
                     (lambda (candidate)
                       (equal (cdr candidate) invalid))
                     modules))))))

(ert-deftest pyimp-deduplicate-modules-preserves-source-precedence ()
  (let* ((seen (make-hash-table :test #'equal))
         (project
          (pyimp--deduplicate-modules
           '(("shared" . "/project/shared.py")
             ("project-only" . "/project/only.py"))
           seen))
         (installed
          (pyimp--deduplicate-modules
           '(("shared" . "/site/shared.py")
             ("library-only" . "/site/only.py"))
           seen))
         (builtins
          (pyimp--deduplicate-modules
           '(("shared") ("library-only") ("sys"))
           seen)))
    (should (equal (mapcar #'car project) '("shared" "project-only")))
    (should (equal (mapcar #'car installed) '("library-only")))
    (should (equal (mapcar #'car builtins) '("sys")))))

(provide 'pyimp-tests)
;;; pyimp-tests.el ends here
