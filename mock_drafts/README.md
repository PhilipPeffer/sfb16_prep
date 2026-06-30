# Mock draft line-ups

Drop one plain-text file per team in this directory. Format:

- One player name per line
- 20 lines per team (first 10 = starting line-up, remaining 10 = bench)
- File name (without `.txt`) is used as the team label, e.g. `team_a.txt`

Player names must match the `name` column in `data/tradyr_vbd.csv`
(case and surrounding whitespace are ignored, but spelling must match).

Score all line-ups with:

```bash
Rscript eval_mock_drafts.R
```

This prints a ranked table and writes `data/mock_draft_eval.csv`.
