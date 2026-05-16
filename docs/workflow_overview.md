# Workflow overview

This workflow is a small, self-contained example of demographic resilience modelling in R.

It simulates stage-structured populations, applies drought and land-use disturbance scenarios, and compares population outcomes against a no-disturbance reference. The example uses synthetic inputs so it can be run without restricted datasets.

The main comparison is between additive and interacting disturbances. Additive scenarios apply drought and land-use pressure without an interaction term. Interactive scenarios use the same drought sequence and land-use level, but add an extra penalty when both pressures occur together.

The paired design means each interactive case is compared with an additive case using the same species, drought probability, land-use intensity, and drought sequence.

The outputs are intended for checking model behaviour rather than making ecological claims about a particular study system.
