;;; org-natural-dates.el --- Natural language date parsing for Org capture -*- lexical-binding: t; -*-

(require 'org)
(require 'rx)
(require 'seq)

(declare-function org-agenda-schedule "org-agenda")
(declare-function org-agenda-deadline "org-agenda")

;;; Constants

(defconst org-natural-dates--repeater-regexp
  (rx (seq (group (or "every" "Every") (1+ space))
           (group (or "day" "daily"
                      "week" "weekly"
                      "month" "monthly"
                      "year" "yearly"
                      (seq (1+ digit) (1+ space) (or "days" "weeks" "months" "years"))
                      "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"
                      "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))))
  "Regex for recurring intervals.")

(defconst org-natural-dates--time-regexp
  (rx (seq (optional (seq (or "at" "At" "@") (1+ space)))
           (or (seq (1+ digit) ":" (1+ digit))
               (seq (1+ digit) (optional (seq ":" (1+ digit))) (optional space) (or "am" "pm" "AM" "PM")))))
  "Regex for time expressions.")

(defconst org-natural-dates--date-regexp
  (rx (seq (optional (seq (group (or "on" "On" "in" "In" "due" "Due" "by" "By" "next" "Next" "last" "Last" "deadline" "Deadline")) (1+ space)))
           (optional (seq (or "on" "in" "at" "of") (1+ space)))
           (or "today" "tomorrow" "yesterday"
               (seq (1+ digit) (1+ space) (or "days" "weeks" "months" "years"))
               "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday" "Sunday"
               "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"
               (seq (1+ digit) (optional (or "st" "nd" "rd" "th")) (1+ space)
                    (or "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"
                        "January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December")))))
  "Regex for date expressions.")

;;; Parsing Functions

(defun org-natural-dates--parse-repeater (str)
  "Extract repeater from STR. Returns (repeater . cleaned-str)."
  (let ((repeater nil)
        (clean-str str)
        (case-fold-search t))
    (when (string-match org-natural-dates--repeater-regexp str)
      (let* ((unit-part (match-string 2 str))
             (is-weekday nil)
             (interval (save-match-data
                         (cond
                          ((string-match "^\\(day\\|daily\\)$" unit-part) "+1d")
                          ((string-match "^\\(week\\|weekly\\)$" unit-part) "+1w")
                          ((string-match "^\\(month\\|monthly\\)$" unit-part) "+1m")
                          ((string-match "^\\(year\\|yearly\\)$" unit-part) "+1y")
                          ((string-match "\\([0-9]+\\) \\(days?\\|weeks?\\|months?\\|years?\\)" unit-part)
                           (let ((num (match-string 1 unit-part))
                                 (unit (match-string 2 unit-part)))
                             (concat "+" num (substring unit 0 1))))
                          ;; Weekdays imply weekly recurrence
                          ((string-match "Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun" unit-part)
                           (setq is-weekday t)
                           "+1w")
                          (t "+1d")))))
        (setq repeater interval)
        ;; If it's a weekday, we keep the weekday part (group 2) for the date parser
        ;; Otherwise we remove the whole match
        (if is-weekday
            (setq clean-str (string-trim (replace-match unit-part t nil str)))
          (setq clean-str (string-trim (replace-match "" nil nil str))))))
    (cons repeater clean-str)))

(defun org-natural-dates--parse-time (str)
  "Extract time from STR. Returns (time . cleaned-str)."
  (let ((time nil)
        (clean-str str)
        (case-fold-search t))
    (when (string-match org-natural-dates--time-regexp str)
      (let ((match (match-string 0 str)))
        ;; Extract the actual time part (ignoring "at ")
        (let ((time-part (save-match-data
                           (if (string-match (rx (or "at" "At" "@") (1+ space)) match)
                               (substring match (match-end 0))
                             match))))
          (setq time time-part))
        (setq clean-str (string-trim (replace-match "" nil nil str)))))
    (cons time clean-str)))

(defun org-natural-dates--parse-date (str)
  "Extract date expression from STR. Returns (date type . cleaned-str)."
  (let ((date nil)
        (type :scheduled)
        (clean-str str)
        (case-fold-search t))
    (when (string-match org-natural-dates--date-regexp str)
      (let* ((match (match-string 0 str))
             (preposition (match-string 1 str)))

        ;; Determine type based on preposition
        (when (and preposition
                   (string-match-p (rx (or "due" "Due" "by" "By" "deadline" "Deadline")) preposition))
          (setq type :deadline))

        ;; Extract the actual date part (ignoring "on ", "in ", etc.)
        (let ((date-part (save-match-data
                           (if (string-match (rx (or "on" "On" "in" "In" "due" "Due" "by" "By" "next" "Next" "last" "Last" "deadline" "Deadline") (1+ space)) match)
                               (substring match (match-end 0))
                             match))))
          (setq date date-part))
        ;; Ensure we remove the whole match including "deadline" if it was part of it
        ;; The regex already covers "deadline" in the preposition group
        (setq clean-str (string-trim (replace-match "" nil nil str)))))
    (list date type clean-str)))

;;; Main Function

(defun org-natural-dates-parse (input)
  "Parse natural language INPUT into a property list.
Returns a plist containing :text, :org-timestamp, and other fields."
  (let* ((text input)
         (repeater nil)
         (time nil)
         (date nil)
         (type :scheduled)

         ;; Extract Repeater
         (res-rep (org-natural-dates--parse-repeater text))
         (_ (setq repeater (car res-rep) text (cdr res-rep)))

         ;; Extract Time
         (res-time (org-natural-dates--parse-time text))
         (_ (setq time (car res-time) text (cdr res-time)))

         ;; Extract Date and Type
         (res-date (org-natural-dates--parse-date text))
         (_ (setq date (nth 0 res-date)
                  type (nth 1 res-date)
                  text (nth 2 res-date)))

         ;; Calculate Date/Timestamp
         (base-date (if date date "today"))
         (base-time (if time time ""))
         (full-ts-str (concat base-date " " base-time))

         ;; Use org-read-date to resolve the timestamp.
         (parsed-time (condition-case nil
                          (org-read-date nil t full-ts-str)
                        (error (current-time))))

         ;; Normalize time if present
         (normalized-time (when time
                            (format-time-string "%H:%M" parsed-time)))

         (final-ts (format-time-string
                    (concat "<%Y-%m-%d %a"
                            (if normalized-time " %H:%M" "")
                            (if repeater (concat " " repeater) "")
                            ">")
                    parsed-time)))

    (list :text text
          :repeater repeater
          :time normalized-time
          :date date
          :type type
          :org-timestamp final-ts)))

;;; Interactive Command

(defvar org-natural-dates-last-result nil
  "Stores the last parse result for use in capture templates.")

(defun org-natural-dates-get (key)
  "Get KEY from `org-natural-dates-last-result`.
KEY can be :text, :org-timestamp, :repeater, :time, :date, :type."
  (plist-get org-natural-dates-last-result key))

(defun org-natural-dates-get-timestamp-line ()
  "Return the appropriate timestamp line (SCHEDULED or DEADLINE)."
  (let ((ts (org-natural-dates-get :org-timestamp))
        (type (org-natural-dates-get :type)))
    (if (eq type :deadline)
        (concat "DEADLINE: " ts)
      (concat "SCHEDULED: " ts))))

;;;###autoload
(defun org-natural-dates (&optional string)
  "Prompt for a natural language task and capture it.
The capture template should use %(org-natural-dates-get :text)
and %(org-natural-dates-get-timestamp-line)."
  (interactive)
  (let* ((input (or string (read-string "Task: ")))
         (result (org-natural-dates-parse input)))
    (setq org-natural-dates-last-result result)
    (org-capture)))

;;; Interactive Scheduling Wrappers

;;;###autoload
(defun org-natural-dates-schedule (arg)
  "Schedule the item at point using natural language.
ARG is passed to `org-schedule`. If ARG is provided (e.g. C-u),
it falls back to standard `org-schedule` behavior."
  (interactive "P")
  (if arg
      (org-schedule arg)
    (let* ((input (read-string "Scheduled to: "))
           (result (org-natural-dates-parse input))
           (time (plist-get result :org-timestamp)))
      (org-schedule nil time))))

;;;###autoload
(defun org-natural-dates-deadline (arg)
  "Set a deadline for the item at point using natural language.
ARG is passed to `org-deadline`. If ARG is provided (e.g. C-u),
it falls back to standard `org-deadline` behavior."
  (interactive "P")
  (if arg
      (org-deadline arg)
    (let* ((input (read-string "Deadline to: "))
           (result (org-natural-dates-parse input))
           (time (plist-get result :org-timestamp)))
      (org-deadline nil time))))

;;;###autoload
(defun org-natural-dates-agenda-schedule (arg)
  "Schedule the item at point in Org Agenda using natural language.
ARG is passed to `org-agenda-schedule`. If ARG is provided,
it falls back to standard behavior."
  (interactive "P")
  (if arg
      (org-agenda-schedule arg)
    (let* ((input (read-string "Scheduled to: "))
           (result (org-natural-dates-parse input))
           (time (plist-get result :org-timestamp)))
      (org-agenda-schedule nil time))))

;;;###autoload
(defun org-natural-dates-agenda-deadline (arg)
  "Set a deadline for the item at point in Org Agenda using natural language.
ARG is passed to `org-agenda-deadline`. If ARG is provided,
it falls back to standard behavior."
  (interactive "P")
  (if arg
      (org-agenda-deadline arg)
    (let* ((input (read-string "Deadline to: "))
           (result (org-natural-dates-parse input))
           (time (plist-get result :org-timestamp)))
      (org-agenda-deadline nil time))))

(provide 'org-natural-dates)
