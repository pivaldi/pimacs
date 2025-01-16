;;; pimacs/lang-ts/autoload.el -*- lexical-binding: t; -*-

(defun pim-ng-get-app-dir ()
  "Search for the `pim-ng-app-filenames` file traversing up the
directory tree. Return the directory."
  (let ((dir default-directory)
        (parent-dir (file-name-directory (directory-file-name default-directory)))
        (nearest-search-dir 'nil)
        (file-path nil))
    (while (and (not (string= dir parent-dir))
                (not nearest-search-dir))
      (dolist (filename pim-ng-app-filenames)
        (setq file-path (concat dir filename))
        (when (file-readable-p file-path)
          (setq nearest-search-dir dir)))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-search-dir))

;;;###autoload
(defun pim-gn-replace-path-at-point-by-app-relative-path nil
  "Replace the path at point by the path relatively at the ng app dir."
  (interactive)
  (let* ((ng-app-dir (pim-ng-get-app-dir))
         (file-at-point (file-truename (thing-at-point 'filename)))
         (bounds (bounds-of-thing-at-point 'filename))
         (start (car bounds))
         (end (cdr bounds))
         (path-to-app (concat "app/" (file-relative-name file-at-point ng-app-dir)))
         )
    (delete-region start end)
    (insert path-to-app)))

;;;###autoload
(defun pim-ng-complete-filename nil
  "Complete file name from the src directory."
  (interactive)
  (let ((default-directory
         (file-truename
          (concat

           (if (string-prefix-p "." (thing-at-point 'filename t))
               ""
             (concat (pim-ng-get-app-dir) "../"))
           ))))
    (message default-directory)
    (comint-dynamic-complete-filename)))

;;;###autoload
(defun pim-setup-tide-mode ()
  "Initialize tide on `typescript-ts-mode-hook'."
  (interactive)
  (tide-setup)
  (tide-hl-identifier-mode +1)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

;;;###autoload
(defun pi-ts-compile ()
  (interactive)
  (compile (concat
            pim-ts-compile-command
            " " (buffer-file-name))))
