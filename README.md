The code in this repository includes a R project that was used to conduct a simulation study to explore bias that arises in treatment effect estimates according to how multinomial outcomes (i.e., competing events) are handled in studies of the effects of prenatal medication exposures on pregnancy outcomes. This manuscript was originally published to arXiv in April 2025: https://arxiv.org/abs/2504.12415. It is currently under review at Paediatric and Perinatal Epidemiology.

Multiple files are included in this repository. 

The .xlsx files were used to specify the data generation parameters for each scenario included in our simulation. The files named as a scenario number were used for the simulation. The file named "Simulation parameters.xlsx" was an original file that was used for testing: some justification for our parameter choices are included in this file.

This project uses the renv package to track versions of other packages used throughout: https://rstudio.github.io/renv/articles/renv.html. To start, use the `renv::restore()` function to install the same versions of the packages originally used in the analysis.

To generate the simulated data, you will run the file `01_generate data for each scenario.R`. This file calls the file `00_data generation functions.R`, which contains all fo the functions used to generate our simulated data.

To analyze the simulated data, you will run the file `02_Run analyses on generated data.R`. This file can only be run after the full set of simulated data has been created.

Finally, the RMD files that start with `03_analyze full simulation data` analyze the simulated data for each scenario. The file with the name ending in `_manuscript.Rmd` should contain all code used to generate numbers provided in the manuscript.
