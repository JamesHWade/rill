# Reader Feedback in vitals

Synthetic Reader Feedback for demonstrating the vitals workflow. No real Reader data, provider calls, or measured model improvement.

## Human evaluations

Four retained outputs were reviewed: two helpful and two not helpful.
These are explicit ratings of saved outputs. No agent was rerun.

| Configuration | Rated | Helpful | Helpful rate |
| --- | ---: | ---: | ---: |
| orientation / fixture-model / fixture-v1 | 2 | 1 | 0.5 |
| question / fixture-model / fixture-v1 | 2 | 1 | 0.5 |

## Reported problems

Reasons tag an overall rating and can overlap.

| Reason | Rated | Helpful |
| --- | ---: | ---: |
| clarity | 1 | 1 |
| execution | 1 | 0 |
| source_faithfulness | 2 | 1 |
| usefulness | 1 | 1 |

## Paired comparison

The small-pilot example has distinct output IDs and separate synthetic ratings.
The original overstates the pilot result and is rated not helpful. The more cautious
version is independently rated helpful. Both identify the same synthetic source.
This demonstrates the workflow, not a measured improvement to live Orientation.
Original inputs must be recovered and checked before a real replay.

## Judge calibration

Three of four examples have independent synthetic judgments. Two agree with the
Reader and one disagrees. Coverage is 75%; agreement among judged examples is 66.7%.
The fourth judgment is missing and remains unknown.

The disagreement concerns the overconfident pilot interpretation. The calibration
log shows it alongside the human comment and judge explanation.

## Inspect the outputs

Run in R from the repository root:

```r
vitals::vitals_view("artifacts/reader-feedback-vitals/logs")
```

Logs retain the exact outputs, provenance, human feedback, and labels identifying
display transcripts. Timing and token usage concern the import. Rebuild from
current feedback after rating revisions or withdrawal.
