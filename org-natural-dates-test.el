;;; org-natural-dates-test.el --- Tests for org-natural-dates  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'org-natural-dates "./org-natural-dates.el")

;;; Integration tests — full parse pipeline

(defconst org-natural-dates-test-cases
  '(;; Basic relative dates
    ("Call Dave tomorrow"
     "Call Dave" "tomorrow" nil nil)
    ("Buy milk today"
     "Buy milk" "today" nil nil)
    ("Did laundry yesterday"
     "Did laundry" "yesterday" nil nil)
    ("Submit report in 3 days"
     "Submit report" "3 days" nil nil)

    ;; Weekdays — full and abbreviated
    ("Lunch on Friday"
     "Lunch" "Friday" nil nil)
    ("Party next Saturday"
     "Party" "next Saturday" nil nil)
    ("Review last Wednesday"
     "Review" "last Wednesday" nil nil)
    ("Standup on Mon"
     "Standup" "Mon" nil nil)

    ;; Times — various formats
    ("Meeting at 3pm"
     "Meeting" nil "15:00" nil)
    ("Dinner at 19:00"
     "Dinner" nil "19:00" nil)
    ("Call Mom tomorrow at 5pm"
     "Call Mom" "tomorrow" "17:00" nil)
    ("Dentist on Monday at 11"
     "Dentist" "Monday" "11:00" nil)
    ("Lunch on Tuesday at 12"
     "Lunch" "Tuesday" "12:00" nil)
    ("Coffee at 3:30"
     "Coffee" nil "15:30" nil)
    ("Gym at 8"
     "Gym" nil "08:00" nil)
    ("Standup @ 9"
     "Standup" nil "09:00" nil)
    ("Party at 10am"
     "Party" nil "10:00" nil)

    ;; Recurring
    ("Gym every day"
     "Gym" nil nil "+1d")
    ("Weekly review every Friday"
     "Weekly review" "Friday" nil "+1w")
    ("Pay rent every month"
     "Pay rent" nil nil "+1m")
    ("Renew license every year"
     "Renew license" nil nil "+1y")
    ("Bill pay every 2 weeks"
     "Bill pay" nil nil "+2w")
    ("Mow lawn every other week"
     "Mow lawn" nil nil "+2w")
    ("Check smoke detectors every other year"
     "Check smoke detectors" nil nil "+2y")
    ("Clean fridge every other month"
     "Clean fridge" nil nil "+2m")

    ;; Ordinals
    ("Pay taxes on the 15th of every month"
     "Pay taxes" "15" nil "+1m")
    ("File report on the 2nd of every month"
     "File report" "2" nil "+1m")

    ;; Complex combinations
    ("Status update every Monday at 10am"
     "Status update" "Monday" "10:00" "+1w" :scheduled)

    ;; Deadlines
    ("Submit report by Friday"
     "Submit report" "Friday" nil nil :deadline)
    ("Due tomorrow"
     "" "tomorrow" nil nil :deadline)
    ("Project deadline in 3 days"
     "Project" "in 3 days" nil nil :deadline)

    ;; Plain text — no date, time, or repeater
    ("Just a note"
     "Just a note" nil nil nil)
    ("Buy groceries"
     "Buy groceries" nil nil nil))
  "Test cases: (INPUT TEXT DATE TIME REPEATER [TYPE]).")

(ert-deftest org-natural-dates-parsing-test ()
  "Test parsing of natural language strings."
  (dolist (case org-natural-dates-test-cases)
    (let* ((input (car case))
           (expected (cdr case))
           (expected-text (nth 0 expected))
           (expected-date (nth 1 expected))
           (expected-time (nth 2 expected))
           (expected-repeater (nth 3 expected))
           (expected-type (or (nth 4 expected) :scheduled))
           (result (org-natural-dates-parse input)))

      (message "Testing input: %s" input)
      (should (string= (plist-get result :text) expected-text))

      (if expected-date
          (should (string= (plist-get result :date) expected-date))
        (should (null (plist-get result :date))))

      (if expected-repeater
          (should (string= (plist-get result :repeater) expected-repeater))
        (should (null (plist-get result :repeater))))

      (if expected-time
          (should (string= (plist-get result :time) expected-time))
        (should (null (plist-get result :time))))

      (should (eq (plist-get result :type) expected-type))

      ;; Timestamp must be valid org format
      (should (string-match-p (rx "<" (+ (any digit "-")) " " (+ (any alpha)) (+ any) ">")
                              (plist-get result :org-timestamp))))))

;;; Date resolution tests — verify actual calendar dates

(ert-deftest org-natural-dates-relative-date-test ()
  "Test that today/tomorrow/yesterday resolve to correct dates."
  (let ((today (format-time-string "%Y-%m-%d"))
        (tomorrow (format-time-string "%Y-%m-%d"
				      (time-add (current-time) (days-to-time 1))))
        (yesterday (format-time-string "%Y-%m-%d"
				       (time-add (current-time) (days-to-time -1)))))
    (should (string-match-p today
			    (plist-get (org-natural-dates-parse "do something today") :org-timestamp)))
    (should (string-match-p tomorrow
			    (plist-get (org-natural-dates-parse "do something tomorrow") :org-timestamp)))
    (should (string-match-p yesterday
			    (plist-get (org-natural-dates-parse "did something yesterday") :org-timestamp)))))

(ert-deftest org-natural-dates-in-n-days-test ()
  "Test \"in N days/weeks\" resolves to correct offset."
  (let ((in-3d (format-time-string "%Y-%m-%d"
				   (time-add (current-time) (days-to-time 3))))
        (in-2w (format-time-string "%Y-%m-%d"
				   (time-add (current-time) (days-to-time 14)))))
    (should (string-match-p in-3d
			    (plist-get (org-natural-dates-parse "submit report in 3 days") :org-timestamp)))
    (should (string-match-p in-2w
			    (plist-get (org-natural-dates-parse "review in 2 weeks") :org-timestamp)))))

(ert-deftest org-natural-dates-last-day-test ()
  "Test \"last <day>\" resolves to the most recent past occurrence."
  (let* ((today-dow (decoded-time-weekday (decode-time)))
         ;; Pick a day 3 days ago
         (target-dow (mod (- today-dow 3) 7))
         (day-names ["Sunday" "Monday" "Tuesday" "Wednesday"
                     "Thursday" "Friday" "Saturday"])
         (target-name (aref day-names target-dow))
         (expected (format-time-string "%Y-%m-%d"
				       (time-add (current-time) (days-to-time -3)))))
    (should (string-match-p expected
			    (plist-get (org-natural-dates-parse
					(concat "call last " target-name))
				       :org-timestamp)))))

(ert-deftest org-natural-dates-last-same-day-test ()
  "Test \"last <today's day>\" gives 7 days ago, not today."
  (let* ((today-dow (decoded-time-weekday (decode-time)))
         (day-names ["Sunday" "Monday" "Tuesday" "Wednesday"
                     "Thursday" "Friday" "Saturday"])
         (today-name (aref day-names today-dow))
         (week-ago (format-time-string "%Y-%m-%d"
				       (time-add (current-time) (days-to-time -7)))))
    (should (string-match-p week-ago
			    (plist-get (org-natural-dates-parse
					(concat "met last " today-name))
				       :org-timestamp)))))

;;; Case insensitivity tests

(ert-deftest org-natural-dates-case-insensitive-test ()
  "Test that parsing is case-insensitive."
  (let ((r1 (org-natural-dates-parse "call TOMORROW"))
        (r2 (org-natural-dates-parse "call tomorrow")))
    (should (string= (plist-get r1 :date) "TOMORROW"))
    (should (string= (plist-get r1 :org-timestamp)
                     (plist-get r2 :org-timestamp))))

  (let ((r (org-natural-dates-parse "gym EVERY friday")))
    (should (string= (plist-get r :repeater) "+1w"))
    (should (string-match-p "friday" (downcase (plist-get r :date)))))

  (let ((r (org-natural-dates-parse "report BY Friday")))
    (should (eq (plist-get r :type) :deadline))))

;;; Timestamp content tests — verify time/repeater appear in output

(ert-deftest org-natural-dates-timestamp-contents-test ()
  "Test that time and repeater appear inside the org timestamp."
  ;; Time appears in timestamp
  (let ((r (org-natural-dates-parse "meeting at 3pm")))
    (should (string-match-p (rx "15:00") (plist-get r :org-timestamp))))

  ;; Repeater appears in timestamp
  (let ((r (org-natural-dates-parse "standup every day")))
    (should (string-match-p (rx "+1d>") (plist-get r :org-timestamp))))

  ;; Both time and repeater
  (let ((r (org-natural-dates-parse "standup every Monday at 9am")))
    (should (string-match-p (rx "09:00") (plist-get r :org-timestamp)))
    (should (string-match-p (rx "+1w>") (plist-get r :org-timestamp))))

  ;; No time → no HH:MM in timestamp
  (let ((r (org-natural-dates-parse "lunch on Friday")))
    (should-not (string-match-p (rx digit digit ":" digit digit)
                                (plist-get r :org-timestamp)))))

;;; Unit tests for internal functions

(ert-deftest org-natural-dates--date-to-org-test ()
  "Test date-to-org translation."
  (should (string= (org-natural-dates--date-to-org "today") "."))
  (should (string= (org-natural-dates--date-to-org "tomorrow") "+1d"))
  (should (string= (org-natural-dates--date-to-org "yesterday") "-1d"))
  (should (string= (org-natural-dates--date-to-org "3 days") "+3d"))
  (should (string= (org-natural-dates--date-to-org "2 weeks") "+2w"))
  (should (string= (org-natural-dates--date-to-org "6 months") "+6m"))
  (should (string= (org-natural-dates--date-to-org "1 years") "+1y"))
  ;; Bare day names pass through
  (should (string= (org-natural-dates--date-to-org "Friday") "Friday"))
  ;; "next" stripped, bare day name returned
  (should (string= (org-natural-dates--date-to-org "next Monday") "Monday"))
  ;; "last" → negative offset
  (should (string-match-p (rx bos "-" (1+ digit) "d" eos)
                          (org-natural-dates--date-to-org "last Tuesday")))
  ;; nil → "."
  (should (string= (org-natural-dates--date-to-org nil) ".")))

(ert-deftest org-natural-dates--parse-repeater-test ()
  "Test repeater extraction."
  (should (equal (org-natural-dates--parse-repeater "gym every day")
                 '("+1d" . "gym")))
  (should (equal (org-natural-dates--parse-repeater "pay every month")
                 '("+1m" . "pay")))
  (should (equal (org-natural-dates--parse-repeater "clean every other week")
                 '("+2w" . "clean")))
  (should (equal (org-natural-dates--parse-repeater "bill every 3 months")
                 '("+3m" . "bill")))
  ;; Weekday repeater preserves day name for date parser
  (let ((r (org-natural-dates--parse-repeater "standup every Monday")))
    (should (string= (car r) "+1w"))
    (should (string= (cdr r) "standup Monday")))
  ;; No repeater → nil
  (should (equal (org-natural-dates--parse-repeater "just text")
                 '(nil . "just text"))))

(ert-deftest org-natural-dates--parse-time-test ()
  "Test time extraction."
  (should (equal (org-natural-dates--parse-time "meeting at 3pm")
                 '("3pm" . "meeting")))
  (should (equal (org-natural-dates--parse-time "dinner at 19:00")
                 '("19:00" . "dinner")))
  (should (equal (org-natural-dates--parse-time "standup @ 9")
                 '("9am" . "standup")))
  (should (equal (org-natural-dates--parse-time "party 10pm")
                 '("10pm" . "party")))
  ;; No time → nil
  (should (equal (org-natural-dates--parse-time "just text")
                 '(nil . "just text"))))

(ert-deftest org-natural-dates--parse-date-test ()
  "Test date extraction and type detection."
  (should (equal (org-natural-dates--parse-date "lunch on Friday")
                 '("Friday" :scheduled "lunch")))
  (should (equal (org-natural-dates--parse-date "review next Monday")
                 '("next Monday" :scheduled "review")))
  (should (equal (org-natural-dates--parse-date "met last Wednesday")
                 '("last Wednesday" :scheduled "met")))
  (should (equal (org-natural-dates--parse-date "report by Friday")
                 '("Friday" :deadline "report")))
  (should (equal (org-natural-dates--parse-date "due tomorrow")
                 '("tomorrow" :deadline "")))
  ;; No date → nil
  (should (equal (org-natural-dates--parse-date "just text")
                 '(nil :scheduled "just text"))))

(provide 'org-natural-dates-test)

;;; org-natural-dates-test.el ends here
