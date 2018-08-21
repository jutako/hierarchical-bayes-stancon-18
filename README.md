# Hierarchical probabilistic models of instantaneous heart rate for StanCon2018 Helsinki conference

This repository contains the R and stan codes used to produce the analyses presented in the StanCon2018 Helsinki conference paper "Hierarchical Gaussian modelling of instantaneous heart rate distributions".

The conference paper is based on real data whereas this repository works with synthetic data. To reproduce the analyses follow instructions under section [HOWTO](#HOWTO).

## HOWTO

The code expects that your working directory in R is the root of this repository.

### 1. To generate synthetic data
```
source("R/generate_data.R")
```

### 2. To fit various stan models
```
source("R/batch_SC18.R")
```

### 3. To create reports of model fit
```
source("R/batch_report.R")
```

### 3. To plot figures like those in the poster
```
source("R/pubplots_posterior-comparison.R")
```

### 4. Perform model comparison
```
source("R/loo_example.R")
```


## Contact information

Jussi Korpela

Finnish Institute of Occupational Health

first.last`some-char-here`ttl.fi


## Copyrigth and license
The license for this code is described in the [LICENSE](LICENSE).
