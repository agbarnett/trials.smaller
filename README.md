# Smaller clinical trials
Testing whether we can make key decisions about what treatments to adopt using smaller clinical trials. Code for paper submitted to F1000Research.

The files are:
* `Simulate.R` the key file that simulates the cost-effective analysis based on a random sample of the TEXT ME clinical trial
* `text.me.sample.random.R` creates a random sample of the TEXT ME clinical trial data. This is a completely random sample that has similar features to the TEXT ME data. We do not have the rights to share the original trial data, but we used the original data in the paper.
* `state.costs.R` function to randomly generate state costs
* `event.costs.R` function to randomly generate event costs (costs of stroke and myocardial infarction)
* `risk.reductions.R` function to randomly select risk reductions in intervention and usual care groups
* `Life.Table.R` Australian life table data, used to give age and sex varying death rates
