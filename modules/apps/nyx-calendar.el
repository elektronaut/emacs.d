;;; nyx-calendar.el --- Calendar -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package calendar
  :ensure nil
  :custom ((calendar-week-start-day 1)
           (calendar-time-display-form '(24-hours ":" minutes))))

(provide 'nyx-calendar)
;;; nyx-calendar.el ends here
