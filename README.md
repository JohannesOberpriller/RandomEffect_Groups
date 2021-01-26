#  Oberpriller, Pichler, and de Souza Leite, < Title TBA >

This subfolder contains the code to reproduce the results of Oberpriller, Pichler, and de Souza Leite, < Title TBA >


## Simulations
We simulated two different mixed-effect model scenarios:

### Plant growth depends on Temperature
Height ~ Temperature + (Temperature|Mountain range)

Script to run simulations and fit models (5000 times for each number of mountain range):
```r
source("Mountain_simulation/simulation_mountain_lmm.R")
```

### Reproductive success depends on Temperature
Reproductive success ~ Temperature + (Temperature|Mountain range)

```r
source("Mountain_simulation/simulation_mountain_glmm.R")
```


## Figures
Script to build Figures:
```r
source("Figures.R")
```

Figures are saved in the "Figures" folder.