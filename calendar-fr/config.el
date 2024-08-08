;;; -*- lexical-binding: t; -*-
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

;; Configure the Emacs calendar fr french users.

;;; Code:

;; -----------------------
;; * La date en français *

(use-package!
 calendar
 :defer t
 :config
 (setq calendar-day-abbrev-array
       ["dim" "lun" "mar" "mer" "jeu" "ven" "sam"])
 (setq calendar-day-name-array
       ["dimanche" "lundi" "mardi" "mercredi" "jeudi" "vendredi" "samedi"])
 (setq calendar-month-abbrev-array
       ["jan" "fév" "mar" "avr" "mai" "jun"
        "jul" "aoû" "sep" "oct" "nov" "déc"])
 (setq calendar-month-name-array
       ["janvier" "février" "mars" "avril" "mai" "juin"
        "juillet" "août" "septembre" "octobre" "novembre" "décembre"])

 (setq european-calendar-style t
       display-time-day-and-date t
       display-time-24hr-format t
       calendar-week-start-day 1
       mark-holidays-in-calendar t
       general-holidays nil
       hebrew-holidays nil
       islamic-holidays nil
       bahai-holidays nil
       oriental-holidays nil
       local-holidays
       '((holiday-fixed 5 1 "Fête du travail")
         (holiday-fixed 5 8 "Victoire 1945")
         (holiday-fixed 7 14 "Fête nationale")
         (holiday-fixed 8 15 "Assomption")
         (holiday-fixed 11 1 "Toussaint")
         (holiday-fixed 11 11 "Armistice 1918")
         (holiday-float 5 0 2 "Fête des mères")
         (holiday-float 6 0 3 "Fête des pères")))
)

 (provide 'pimacs/calendar-fr)
;;; calendar-fr.el ends here

 ;; Local variables:
 ;; coding: utf-8
 ;; End:
