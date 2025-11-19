# org-scribe-planner

A comprehensive writing planning tool for Emacs Org-mode, inspired by [pacemaker.press](https://pacemaker.press). Calculate, visualize, and manage your writing goals through an interactive interface that integrates seamlessly with Org-mode.

## Recent Updates

### Version 0.2.0
- **Spare Day Management**: Full control over spare days - add, remove individually, or clear all at once
- **Daily Word Count Tracking**: Track actual words written each day with optional notes
- **Improved Error Messages**: Clear, actionable error messages with specific suggestions
- **File Location Display**: Calendar view now shows where your plan file is saved
- **Input Validation**: All numeric and date inputs are validated with helpful feedback
- **Bug Fixes**: Fixed recalculate menu pattern matching for more reliable operation

## Features

### Core Functionality
- **Smart Calculation**: Provide any 2 of 3 variables (total words, daily words, days) and automatically calculate the third
- **Beautiful Visualizations**: Dual calendar system with custom buffer display and org-agenda integration
- **Flexible Spare Days**: Add, remove, and manage breaks, holidays, and weekends with multiple configuration options
- **Plan Modification**: Easily recalculate your plan when circumstances change
- **Progress Tracking**: Update your word count and see if you're ahead or behind schedule
- **Daily Word Count Tracking**: Record actual words written per day with optional notes
- **Milestone Tracking**: Automatic calculation of 25%, 50%, 75%, and 100% completion dates
- **Org-mode Integration**: Save plans as org files with properties and scheduled entries
- **Smart Error Messages**: Clear, actionable error messages with helpful suggestions

### Planning Modes

**1. Know total words and days? Calculate daily word goal**
- Perfect for: NaNoWriMo (50,000 words in 30 days → 1,667 words/day)

**2. Know total words and daily capacity? Calculate days needed**
- Perfect for: "I can write 1,000 words/day, when will I finish 75,000 words?"

**3. Know daily capacity and available days? Calculate achievable goal**
- Perfect for: "I have 20 days and can write 500 words/day, what's realistic?"

## Installation

### Manual Installation

1. Clone this repository or download `org-scribe-planner.el`:

```bash
git clone https://codeberg.org/jcastp/org-scribe-planner.git
```

2. Add to your Emacs configuration:

```elisp
;; Add the directory to load-path
(add-to-list 'load-path "/path/to/org-scribe-planner")

;; Load the package
(require 'org-scribe-planner)

;; Optional: Set custom directory for writing projects
(setq org-scribe-planner-directory "~/Documents/writing-projects")

;; Optional: Enable automatic org-agenda sync
(setq org-scribe-planner-sync-to-agenda t)
```

### Using use-package

```elisp
(use-package org-scribe-planner
  :load-path "/path/to/org-scribe-planner"
  :custom
  (org-scribe-planner-directory "~/Documents/writing-projects")
  (org-scribe-planner-sync-to-agenda t)
  :bind
  (("C-c w n" . org-scribe-planner-new-plan)
   ("C-c w l" . org-scribe-planner-load-plan)
   ("C-c w u" . org-scribe-planner-update-progress)
   ("C-c w d" . org-scribe-planner-update-daily-word-count)
   ("C-c w r" . org-scribe-planner-recalculate)
   ("C-c w m" . org-scribe-planner-show-milestones)
   ("C-c w s" . org-scribe-planner-sync-agenda)))
```

## Usage

### Creating a New Writing Plan

Run `M-x org-scribe-planner-new-plan` and follow the interactive prompts:

1. **Enter project title**: "My Novel", "Blog Post Series", etc.

2. **Choose your calculation mode**:
   - Total words + Days → Calculate daily words
   - Total words + Daily words → Calculate days needed
   - Daily words + Days → Calculate total words

3. **Enter your known values**: e.g., 50,000 total words and 30 days

4. **Set start date**: Press Enter for today, or specify YYYY-MM-DD format

5. **Configure spare days** (optional):
   - **Add: Specific date**: Add individual dates (2024-12-25 for Christmas)
   - **Add: Date range**: Add multiple consecutive dates
   - **Add: All weekends**: Automatically add all Saturdays and Sundays
   - **Add: All Saturdays**: Add only Saturdays
   - **Add: All Sundays**: Add only Sundays
   - **Remove: Specific date**: Remove a single spare day from the list
   - **Remove: All spare days**: Clear all configured spare days
   - **List current spare days**: View all currently configured spare days

6. **View your plan**: A beautiful calendar visualization appears showing:
   - Daily word counts
   - Cumulative progress
   - Weekly summaries
   - Spare days marked as "REST"

### Example: NaNoWriMo Plan

```
Project title: My NaNoWriMo Novel 2024
What do you know? Total words + Days → Calculate daily words
Total words to write: 50000
Days available: 30
Start date: 2024-11-01
Do you want to set spare/break days? y
Add spare days by: All weekends
```

Result:
- Daily words: 1,667 words/day
- Working days: 22 (excluding 8 weekend days)
- Actual daily requirement: 2,273 words/day

### Loading an Existing Plan

```elisp
M-x org-scribe-planner-load-plan
```

Select from your saved plans to view the calendar visualization.

### Updating Progress

#### Overall Progress

```elisp
M-x org-scribe-planner-update-progress
```

1. Select your plan
2. Enter current word count
3. See updated calendar and progress report:
   - "Progress: 15000/50000 words (30.0%) | Ahead: +500 words"

#### Daily Word Count Tracking

```elisp
M-x org-scribe-planner-update-daily-word-count
```

Track your actual words written for specific days:

1. Select your plan
2. Choose a date from the schedule
3. Enter words written that day (0 or more)
4. Optionally add notes (e.g., "Great writing session!", "Struggled with plot")
5. View updated calendar with actual vs. target comparison

The calendar will show:
- Target words for each day
- Actual words written (when tracked)
- Percentage achieved for each day
- Notes alongside each entry

### Recalculating Your Plan

Life happens! Recalculate when things change:

```elisp
M-x org-scribe-planner-recalculate
```

Options:
- **Daily words (recalc days needed)**: Set new daily word target and recalculate days
- **Days available (recalc daily words)**: Set new total days and recalculate daily words
- **Total words (recalc daily words)**: Set new word count goal and recalculate daily words
- **Add/modify spare days**: Interactive menu to add or remove spare days
- **Remove spare days**: Select and remove a specific spare day
- **Clear all spare days**: Remove all spare days and recalculate

### Viewing Milestones

```elisp
M-x org-scribe-planner-show-milestones
```

Shows key milestone dates:
```
 25% - 2024-11-08 (12500 words)
 50% - 2024-11-15 (25000 words)
 75% - 2024-11-23 (37500 words)
100% - 2024-11-30 (50000 words)
```

### Syncing to Org-Agenda

```elisp
M-x org-scribe-planner-sync-agenda
```

This creates scheduled TODO entries for each writing day in your org file:

```org
*** TODO Write 2273 words
SCHEDULED: <2024-11-01>
:PROPERTIES:
:TARGET_WORDS: 2273
:CUMULATIVE: 2273
:END:
```

The org file is automatically added to `org-agenda-files` for easy tracking in your daily agenda.

## Calendar Visualization

The calendar buffer provides a comprehensive view:

```
Writing Plan: My NaNoWriMo Novel 2024
Location: ~/org/writing-projects/my-nanowrimo-novel-2024.org
================================================================================

Summary:
  Total Words:   50000
  Daily Words:   2273
  Total Days:    30
  Working Days:  22
  Start Date:    2024-11-01
  End Date:      2024-11-30
  Current Words: 15000 (30.0%)

Daily Schedule:
--------------------------------------------------------------------------------

Week 1:
  Date (Day)                 Daily Target   Expected Total   Actual Total     Daily Actual             Notes
  -------------------------  -------------  ---------------  ---------------  -----------------------  ------------------------------
  2025-11-19 (Wednesday)     1450 words     1200 words       1200 words       1200 words [82.8%]       Good day
  2025-11-20 (Thursday )     1450 words     2700 words       2700 words       1500 words [103.4%]      
  2025-11-21 (Friday   )     1450 words     3700 words       3700 words       1000 words [69.0%]       
  2025-11-22 (Saturday )     REST           4702 words       4702 words       1002 words               (spare day)
  2025-11-23 (Sunday   )     REST           5702 words       5702 words       1000 words               (spare day)
  Week total: 4350 words

Week 2:
  ...
```

### Calendar Buffer Commands

- `q` - Quit window
- `r` - Recalculate plan
- `u` - Update overall progress
- `d` - Update daily word count for a specific day

## File Structure

Plans are saved as org files in `org-scribe-planner-directory`:

```org
* My NaNoWriMo Novel 2024
:PROPERTIES:
:TOTAL_WORDS: 50000
:DAILY_WORDS: 2273
:DAYS: 30
:START_DATE: 2024-11-01
:END_DATE: 2024-11-30
:CURRENT_WORDS: 15000
:SPARE_DAYS: 2024-11-02,2024-11-03,2024-11-09,2024-11-10,...
:DAILY_WORD_COUNTS: 2024-11-01:2500:Great start!,2024-11-04:2100:Slow day
:END:

** Schedule

| Date       | Words | Cumulative | Notes     |
|------------+-------+------------+-----------|
| 2024-11-01 |  2273 |       2273 |           |
| 2024-11-02 | REST  |       2273 | Spare day |
...

** Agenda Entries
:PROPERTIES:
:VISIBILITY: folded
:END:

*** TODO Write 2273 words
SCHEDULED: <2024-11-01>
...
```

## Customization

### Available Options

```elisp
;; Directory for storing writing project files
(setq org-scribe-planner-directory "~/org/writing")

;; Default number of spare days when creating plans
(setq org-scribe-planner-default-spare-days 0)

;; Name of the calendar visualization buffer
(setq org-scribe-planner-calendar-buffer "*My Writing Plan*")

;; Whether to automatically sync plans to org-agenda
(setq org-scribe-planner-sync-to-agenda t)
```

## Use Cases

### NaNoWriMo (National Novel Writing Month)
- 50,000 words in November
- Exclude weekends for family time
- Track daily progress against schedule

### Academic Writing
- Thesis chapters with flexible deadlines
- Account for teaching days and conferences
- Break large writing projects into manageable pieces

### Blog Writing Schedules
- Plan content calendar
- Calculate realistic publishing frequency
- Track progress across multiple posts

### Professional Writing Projects
- Client deliverables with hard deadlines
- Account for meetings and non-writing work
- Adjust plan when scope changes

## Tips and Best Practices

1. **Be realistic with daily words**: Consider your actual writing speed, not aspirational goals

2. **Account for spare days**: Life happens - build in buffer days for unexpected events

3. **Use weekends wisely**: Many writers use "All weekends" as spare days for recharging

4. **Update progress regularly**: Daily updates give the best feedback on your pacing

5. **Recalculate when needed**: Don't stick to an impossible plan - adjust and keep going

6. **Use milestones**: They provide psychological wins and help maintain motivation

7. **Sync to org-agenda**: Integrate writing goals with your daily task management

## Troubleshooting

### "Not enough working days" error
The error message will tell you exactly how many days and spare days you have. Either:
- Use "Remove spare days" or "Clear all spare days" in the recalculate menu
- Increase the total number of days available

### "Cannot save plan" errors
Check the error message for:
- **File permission issues**: Ensure you have write access to the directory
- **Directory doesn't exist**: The package will ask if you want to create it
- **File already exists**: Make sure you're not overwriting an important file

### Plans not appearing in org-agenda
Run `M-x org-scribe-planner-sync-agenda` to manually sync and add to `org-agenda-files`.

### Date format errors
The package now validates all dates and will prompt you to re-enter if invalid. Dates must be in YYYY-MM-DD format (e.g., 2024-11-01, not 11/01/2024).

### Invalid number errors
The package validates all numeric input:
- For word counts and days, you must enter positive numbers (greater than 0)
- For daily word counts, you can enter 0 or any positive number
- Error messages will tell you what you entered and what's expected

## Contributing

Contributions are welcome! This package could be extended with:
- Export to various formats (PDF, CSV, iCal)
- Integration with word counting tools
- Automatic word count detection from org files
- Writing session timer integration
- Multi-project views
- Chart visualizations using gnuplot

## License

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

## Acknowledgments

Inspired by the excellent [pacemaker.press](https://pacemaker.press) by Linus Åkerlund.

## Support

For bugs, feature requests, or questions:
- Open an issue on GitHub
- Email: your-email@example.com

---

Happy writing!
