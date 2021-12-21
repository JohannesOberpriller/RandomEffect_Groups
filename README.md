#  Oberpriller, de Souza Leite, and Pichler, "Fixed or random? On the reliability of mixed-effects models for a small number of levels in grouping variables"

This subfolder contains the code to reproduce the results of Oberpriller, de Souza Leite, and Pichler, "Fixed or random? On the reliability of mixed-effects models for a small number of levels in grouping variables"


## Simulations
We simulated two different mixed-effect model scenarios:

1. Plant growth depends on Temperature
Height ~ Temperature + (Temperature|Mountain range)

2. Reproductive success depends on Temperature
Reproductive success ~ Temperature + (Temperature|Mountain range)

Script to run simulations and fit models (5000 times for each number of mountain range):
```r
scripts = list.files("Mountain_simulation/")
for(i in 1:length(scripts)) source(scripts[i])
```

## Figures
Script to build Figures:
```r
source("Figures_Main.R")
source("Figures_SI.R")
source("Figures_SI_glmmTMB.R")
```

Figures are saved in the "Figures" folder.