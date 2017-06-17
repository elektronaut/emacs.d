;;; core-calendar -- Calendar
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package calendar
  :ensure nil
  :init
  (setq-default calendar-week-start-day 1))

(use-package calendar-norway
  :config
  (setq-default calendar-holidays
                (append
                 calendar-norway-raude-dagar
                 calendar-norway-andre-merkedagar
                 calendar-norway-dst)))

(provide 'core-calendar)
;;; core-calendar ends here
