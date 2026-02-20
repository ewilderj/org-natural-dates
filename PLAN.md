# Plan: Org Natural Dates

## Objective
Enable natural language date parsing across Org mode workflows, expanding beyond just capture to include interactive scheduling (`org-schedule`) and deadlines (`org-deadline`), both in regular buffers and Org Agenda.

## New Scope & Renaming
The project is now `org-natural-dates` to reflect its broader utility.

## The UX Challenge
Currently, `org-schedule` (C-c C-s) and `org-deadline` (C-c C-d) prompt the user via `org-read-date`.
`org-read-date` handles relative math (`+2d`, `fri`) but fails on richer natural language ("every Friday", "next Tuesday at 5pm", "in 3 days").

We have two main UX integration points:
1.  **The Capture Flow:** Prompt for a full task string ("Call Dave tomorrow at 5pm") and parse out the text, date, time, and type.
2.  **The Scheduling Flow (New):** When the user invokes `org-schedule` or `org-deadline`, they should be able to type a natural language date (e.g. "every Friday at 3pm") and have it applied.

## Design

### The Goal: Native Integration
We want the user to press `C-c C-s` (org-schedule) or `C-c C-d` (org-deadline) and just type "next friday at 3pm".

### The Mechanics
`org-schedule` and `org-deadline` both eventually call `org-read-date` to prompt the user.
`org-read-date` accepts an argument `FROM-STRING`. If provided, it parses that string instead of prompting.
However, if we want to intercept the *prompt* itself, we need a wrapper around the interactive call.

**Option A: Command Wrappers (Safest, Recommended)**
Create `org-natural-dates-schedule` and `org-natural-dates-deadline` commands.
These commands prompt the user with "Schedule to: ", parse the natural language string using our engine to extract the date/time/repeater, and then call `org-schedule` non-interactively with the pre-formatted time argument.

*Pros:* Clean, isolated, won't break Org's internals.
*Cons:* User needs to rebind `C-c C-s` and `C-c C-d`.

**Option B: `org-read-date` Advice (Magic, Risky)**
Advise `org-read-date`. If the user types "every Friday", we intercept it and return the correct time structure.

*Pros:* Zero config (no rebinding). Works everywhere `org-read-date` is used (agenda, capture, properties).
*Cons:* `org-read-date` is extremely complex. It returns different things based on `TO-TIME` and `WITH-TIME`. Intercepting the prompt string and short-circuiting the complex return logic is brittle.

### Proposed Architecture (Option A: Wrappers)
We will implement safe command wrappers. This is standard Emacs practice for enhancing built-in behavior without breaking core assumptions.

1.  **`org-natural-dates-parse-date(string)`**: A specialized parsing function that expects *only* a date/time/repeater string (no task text). E.g., "every Friday at 3pm". It returns a formatted Org timestamp string.
2.  **`org-natural-dates-schedule`**:
    - Prompts: `Scheduled to: `
    - Calls `org-natural-dates-parse-date`.
    - Calls `(org-schedule nil parsed-timestamp-string)`.
3.  **`org-natural-dates-deadline`**:
    - Prompts: `Deadline to: `
    - Calls `org-natural-dates-parse-date`.
    - Calls `(org-deadline nil parsed-timestamp-string)`.

For the Agenda, we'd provide similar wrappers (e.g., `org-natural-dates-agenda-schedule`).

## Next Steps for Approval
1. Review the proposed wrapper architecture (Option A).
2. If approved, I will implement the new parsing helper and the command wrappers in `org-natural-dates.el`.
