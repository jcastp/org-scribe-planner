# Quick Start Guide

Get up and running with org-scribe-planner in 5 minutes!

## Installation (30 seconds)

1. Add to your `~/.emacs` or `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "/path/to/org-scribe-planner")
(require 'org-scribe-planner)
```

2. Restart Emacs or evaluate the code with `M-x eval-buffer`

## Your First Writing Plan (2 minutes)

### Example: NaNoWriMo

1. Run: `M-x org-scribe-planner-new-plan`

2. Follow the prompts:
   ```
   Project title: My Novel
   What do you know? Total words + Days → Calculate daily words
   Total words to write: 50000
   Days available: 30
   Start date: [press Enter for today]
   Do you want to set spare/break days? y
   Add spare days by: All weekends
   Add spare days by: Done
   ```

3. See your beautiful writing schedule!

### Example: Academic Paper

1. Run: `M-x org-scribe-planner-new-plan`

2. Follow the prompts:
   ```
   Project title: Research Paper
   What do you know? Total words + Daily words → Calculate days needed
   Total words to write: 8000
   Words per day you can write: 500
   Start date: [press Enter for today]
   Do you want to set spare/break days? n
   ```

3. Result: You'll need 16 days at 500 words/day

## Daily Usage (30 seconds)

### Update Your Progress

When you finish writing:

```
M-x org-scribe-planner-update-progress
Select plan: My Novel
Current word count: 2500
```

See if you're ahead or behind schedule!

### View Your Plan

```
M-x org-scribe-planner-load-plan
Select plan: My Novel
```

## Essential Commands

| Command                              | What It Does                   |
|--------------------------------------|--------------------------------|
| `org-scribe-planner-new-plan`        | Create a new writing plan      |
| `org-scribe-planner-load-plan`       | View an existing plan          |
| `org-scribe-planner-update-progress` | Update your word count         |
| `org-scribe-planner-recalculate`     | Adjust plan when things change |
| `org-scribe-planner-show-milestones` | See 25%, 50%, 75%, 100% dates  |

## Recommended Key Bindings

Add to your config:

```elisp
(global-set-key (kbd "C-c w n") 'org-scribe-planner-new-plan)
(global-set-key (kbd "C-c w l") 'org-scribe-planner-load-plan)
(global-set-key (kbd "C-c w u") 'org-scribe-planner-update-progress)
```

## Tips for Success

1. **Be realistic**: Set achievable daily word counts
2. **Build in buffer**: Use spare days for unexpected events
3. **Update regularly**: Daily progress updates keep you on track
4. **Adjust when needed**: Life happens - use `recalculate` to adapt

## Common Scenarios

### Scenario 1: "I have a deadline"
- Know: Total words + Days
- Calculate: Daily words needed
- Example: 10,000 words / 20 days = 500 words/day

### Scenario 2: "What can I achieve?"
- Know: Daily capacity + Days available
- Calculate: Total achievable words
- Example: 1,000 words/day × 15 days = 15,000 words

### Scenario 3: "When will I finish?"
- Know: Total words + Daily capacity
- Calculate: Days needed
- Example: 75,000 words ÷ 2,000/day = 38 days

## Next Steps

- Read the full [README.md](README.md) for all features
- Check [example-config.el](example-config.el) for advanced configuration
- Integrate with org-agenda: `M-x org-scribe-planner-sync-agenda`

## Troubleshooting

**Q: Where are my plans stored?**
A: In `~/org/writing-projects/` by default (customizable via `org-scribe-planner-directory`)

**Q: How do I change my plan?**
A: Run `M-x org-scribe-planner-recalculate` and choose what changed

**Q: Can I exclude weekends?**
A: Yes! When creating a plan, choose "All weekends" in spare days configuration

**Q: How do I see my schedule?**
A: Run `M-x org-scribe-planner-load-plan` to view the calendar

---

**You're ready to write!** Create your first plan and start tracking your progress.

For detailed documentation, see [README.md](README.md)
