;;; core-calendar -- Calendar
;;; Author: Inge Jørgensen <inge@elektronaut.no>

;;; Commentary:

;;; Licence: MIT

;;; Code:

(use-package calendar
  :ensure nil
  :init
  (setq-default calendar-week-start-day 1
                calendar-time-display-form '(24-hours ":" minutes))
  (calendar-set-date-style 'european))

(provide 'core-calendar)
;;; core-calendar ends here
