;;; pimacs/dired/+dependencies.el -*- lexical-binding: t; -*-
;; Copyright (c) 2024, Philippe Ivaldi <www.piprime.fr>

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

;;; Commentary:

;; Install Dependencies needed by Doom/PIMacs php-mode configuration

(use-package!
    async
  :config
  (async-start
   (lambda ()
     (dolist (progdef '(
                        ("phpstan" . (
                                      "composer global require phpstan/phpstan --dev -W --no-progress"
                                      "composer global require phpactor/language-server-phpstan-extension -W --no-progress"))
                        ;; ("behat" . "composer global require phpactor/behat-extension -W --no-progress")
                        ("phpunit" . ("composer global require phpunit/phpunit -W --no-progress"))
                        ("phpctags" . ("composer global require techlivezheng/phpctags -W --no-progress"))
                        ("php-cs-fixer" . ("composer global require friendsofphp/php-cs-fixer -W --no-progress"))
                        ("phpcs". ("composer global require squizlabs/php_codesniffer=* --dev -W --no-progress"))
                        ("prettier" . ("npm install -g @prettier/plugin-php"))))
       (let* ((cmd (car progdef))
              (intall-cmds (cdr progdef))
              (buffername "**PIMacs PHP Dependencies Install**"))
         (unless (executable-find cmd)
           (dolist (installdef intall-cmds)
             (let* ((args (string-split installdef " "))
                    (cmd-install (pop args)))
               (unless (executable-find cmd-install)
                 (error "PIMacs error : PIMacs/lang-php module need \"%s\" to be installed." cmd-install))
               (unless (eq 0 (apply #'call-process cmd-install nil buffername nil args))
                 (unless (get-buffer-window buffername 0)
                   (pop-to-buffer buffername nil t))
                 (error "PIMacs error : failed to install \"%s\" with the command \"%s\"" cmd (string-join args " ")))
               (unless (executable-find cmd)
                 (pop-to-buffer buffername nil t)
                 (error "PIMacs error : Not found \"%s\" in the exec path after installing \"%s\" " cmd cmd))
               ))))))))

;;; Code:
