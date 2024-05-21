The code in this repository was used to conduct a simulation study to explore bias that arises in treatment effect estimates according to how multinomial outcomes (i.e., competing events) are handled in studies of the effects of prenatal medication exposures on pregnancy outcomes. Multiple files are included in this repository. There are multiple files in this repository.

The .xlsx files were used to specify the data generation parameters for each scenario included in our simulation. The files named as a scenario number were used for the simulation. The file named "Simulation parameters.xlsx" was an original file that was used for testing: some justification for our parameter choices are included in this file.

To generate the simulation data, the following files will need to be run:
1. Data generation functions.R -- This file contains all of the functions used to generate our simulation data for each scenario.
2. Data generation - scenarios.R -- The actual data generation for each scenario was conducted in this file.

To analyze the simulation data, the following files will need to be run:
1. analysis functions.R -- This file contains all of the functions used to analyze our simulation data for a scenario.
2. Analyses.R -- This file runs the analysis functions for each of the scenarios. This file outputs datasets with the analysis results.

Finally, the view the results from the simulation:
1. descriptive results_1sim_large_n.Rmd -- This RMD file outputs an HTML with descriptive results from each scenario.
2. review results_1sim_large_n.Rmd -- This RMD file outputs an HTML file with the results of the analyses from each scenario.
