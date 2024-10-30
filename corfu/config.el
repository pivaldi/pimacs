;;; pimacs/corfu/config.el -*- lexical-binding: t; -*-

;; Copyright (c) 2024, Philippe Ivaldi <www.piprim.net>
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; Commentary:

;; Corfu config.

;;; Code:

;; Come from https://hieuphay.com/doom-emacs-config/
(use-package! corfu
  :config
  ;; (map! :desc "Show documentation of current candidate." "M-h" corfu-info-documentation)
  (map! :desc "" "M-<SPC>" #'completion-at-point)
  (setq corfu-auto nil)
  ;; Emacs 30 and newer: Disable Ispell completion function. As an alternative,
  ;; try `cape-dict'.
  (setq
   text-mode-ispell-word-completion nil
   corfu-auto-delay 0.5
   corfu-auto-prefix 3
   ;; (setq tab-always-indent 'complete)
   tab-always-indent t)
  (keymap-unset corfu-map "<TAB>")
  (defun pim--corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound.
Source : https://hieuphay.com/doom-emacs-config/"
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'pim--corfu-enable-in-minibuffer))

(provide 'pimacs/corfu)
;; Local variables:
;; coding: utf-8
;; eval: (rename-buffer "pimacs/corfu/config.el")
;; End:
