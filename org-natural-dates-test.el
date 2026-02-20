;;; org-natural-dates-test.el --- Tests for org-natural-dates  -*- lexical-binding: t; -*-

(require 'ert)
(require 'org-natural-dates "./org-natural-dates.el")

;; Test Cases: (INPUT . (TEXT DATE TIME REPEATER))
(defconst org-natural-dates-test-cases
  '(;; Basic relative dates
    ("Call Dave tomorrow"
     "Call Dave" nil nil nil)
    ("Buy milk today"
     "Buy milk" nil nil nil)
    ("Submit report in 3 days"
     "Submit report" nil nil nil)

    ;; Weekdays
    ("Lunch on Friday"
     "Lunch" "Friday" nil nil)
    ("Party next Saturday"
     "Party" "next Saturday" nil nil)

    ;; Times
    ("Meeting at 3pm"
     "Meeting" nil "15:00" nil)
    ("Dinner at 19:00"
     "Dinner" nil "19:00" nil)
    ("Call Mom tomorrow at 5pm"
     "Call Mom" "tomorrow" "17:00" nil)

    ;; Recurring
    ("Gym every day"
     "Gym" nil nil "+1d")
    ("Weekly review every Friday"
     "Weekly review" "Friday" nil "+1w")
    ("Pay rent every month"
     "Pay rent" nil nil "+1m")
    ("Bill pay every 2 weeks"
     "Bill pay" nil nil "+2w")

        ;; Complex combinations
        ("Status update every Monday at 10am"
         "Status update" "Monday" "10:00" "+1w" :scheduled)

        ;; Deadlines
        ("Submit report by Friday"
         "Submit report" "Friday" nil nil :deadline)
        ("Due tomorrow"
         "" "tomorrow" nil nil :deadline)
        ("Project deadline in 3 days"
         "Project" "in 3 days" nil nil :deadline)))

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

          (when expected-repeater
            (should (string= (plist-get result :repeater) expected-repeater)))

          (when expected-time
            (should (string= (plist-get result :time) expected-time)))

          ;; Verify type (scheduled vs deadline)
          (should (eq (plist-get result :type) expected-type))

          ;; We can assert the :org-timestamp is present and valid format
          (should (string-match-p (rx "<" (+ (any digit "-")) " " (+ (any alpha)) (+ any) ">")
                                  (plist-get result :org-timestamp))))))
