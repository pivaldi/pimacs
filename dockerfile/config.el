;;; pimacs/dockerfile/config.el -*- lexical-binding: t; -*-

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(put 'dockerfile-image-name 'safe-local-variable #'stringp)

;; This prevents the docker command from producing ANSI sequences during the
;; image build process, which results in a more readable output in the
;; compilation buffer. From https://emacs.stackexchange.com/a/55340/11843
(defun plain-pipe-for-process () (setq-local process-connection-type nil))
(add-hook 'compilation-mode-hook 'plain-pipe-for-process)
