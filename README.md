# Stage-structured population resilience simulation in R

This repository contains a small R simulation for exploring how stage-structured populations respond to repeated drought and land-use disturbance.

The example uses synthetic species and scenarios. It was built to practise and document population-projection and resilience calculations, not to represent an empirical demographic study.

## What the code does

The scripts:

1. define simple stage-structured population matrices;
2. generate drought and land-use disturbance scenarios;
3. project population trajectories against an undisturbed reference;
4. calculate resistance, recovery and abundance-loss measures;
5. compare additive and interacting disturbance cases;
6. write summary tables and figures for checking model behaviour.

## Structure

- `R/` — functions for species setup, disturbance scenarios, projections and resilience metrics
- `examples/run_example_workflow.R` — self-contained example run
- `docs/workflow_overview.md` — short method notes
- `outputs/` — generated results when the example is run locally

## Running the example

From the repository root:

```r
source("examples/run_example_workflow.R")
```

The example uses base R.

## Scope and limitations

All species traits and disturbance scenarios in this repository are simulated. The code is useful for testing stage-structured modelling logic and resilience metrics, but the outputs should not be interpreted as estimates for a real population or management recommendation.

**Author:** Ali Moayedi  
University of St Andrews
