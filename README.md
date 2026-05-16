# Ecological resilience modelling workflow

This repository contains a reproducible R workflow for simulating stage-structured populations under additive and interacting disturbance regimes.

The example is self-contained. It creates synthetic species and scenario inputs, projects population trajectories through time, calculates resilience metrics, and compares additive and interacting disturbance cases using paired scenarios.

## Workflow

1. Define simulated species and stage-structured population matrices.
2. Create drought and land-use disturbance scenarios.
3. Project population trajectories against a no-disturbance reference.
4. Calculate final abundance, proportional loss, resistance, and recovery metrics.
5. Compare additive and interacting disturbance outcomes with paired scenario summaries.
6. Export tables and figures for checking the model behaviour.

## Repository layout

```text
R/          R functions used by the workflow
examples/   runnable example script
docs/       short workflow notes
data/       placeholder for user-supplied inputs
outputs/    placeholder for generated results
```

## Run the example

From the repository root:

```r
source("examples/run_example_workflow.R")
```

The workflow uses base R only.

Generated tables and figures are written to:

```text
outputs/example_run/
```

## Main outputs

Tables:

- `simulated_species_traits.csv`
- `disturbance_scenarios.csv`
- `resilience_metrics.csv`
- `paired_interaction_effects.csv`
- `interaction_effect_summary.csv`
- `run_summary.csv`

Figures:

- `final_ratio_by_disturbance.png`
- `interaction_penalty_final_ratio.png`
- `interaction_penalty_average_loss.png`
- `average_loss_by_life_history.png`
- `example_relative_trajectories.png`
- `baseline_lambda_by_life_history.png`

Interaction penalties are zero when one disturbance axis is absent, because the interaction term only modifies outcomes when drought and land-use pressure occur together.
