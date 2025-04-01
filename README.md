
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14710666.svg)](https://doi.org/10.5281/zenodo.14710666)

# Fifty years of research have deepened uncertainties in global irrigation water use

[Arnald Puy](https://www.arnaldpuy.com/), Carmen Aguiló, Seth N. Linga, Naomi Clarke, Lieke A. Melsen

This study compiles 1,624 global irrigation water withdrawal estimates (1990–2100) 
from literature published between 1974 and 2025 and analyzes the uncertainty spread 
to determine whether it has increased or decreased over time.

## Abstract

*For fifty years, refining models and datasets has been central to reducing 
uncertainties in the quantification of global irrigation water use. However, 
whether these efforts have delivered remains unclear. Here we compile 1,624 
global irrigation water withdrawal estimates from 65 studies (1974-2025) and 
examine them through 320 different possible analytical pipelines to ensure 
robustness of results. Our analysis reveals strong evidence that, rather than 
reducing uncertainties, research has actually amplified them or made them 
unstable. Only 20\% of analytical pathways suggest a decline in uncertainty 
over time, dropping to 10\% when excluding projections for 2100. Our findings 
suggest that embracing uncertainty may be a better strategy than chasing 
precision to improve our understanding of global irrigation water use.*

## Replication

We provide all the functions needed to replicate our workflow in the "functions" folder.

### Data

#### Generated data

The "dataset" folder contains the data produced in this study. 

* `iww_dataset.csv`   

#### Secondary data

Since we also used secondary data, we refer the interested reader to the following
links and repositories:

* [ISI-MIP](https://www.isimip.org/)
* [Khan et al 2023](https://www.nature.com/articles/s41597-023-02086-2)

### Functions

The "functions" folder contains all the custom functions coded for the analysis.
They are all sourced from the `.R`, `.pdf` and `.Rmd` files and therefore the 
user of the code does not need to source them separately.

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. There are two main analyses:

* `code_uncerainty_iww`: it includes the workflow and the results of the multiverse
analysis conducted to explore whether 50 years of dataset and model refinement
have reduced uncertainties in irrigation water withdrawals.

* `code_isimip`: it includes the exploratory analysis of variance on ISIMIP data
to check which source of uncertainty (model, climate scenario, socio-economic scenario
or climate forcing) conveys the most uncertainty to the estimation.

Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the generated data section are stored for a successful 
compilation.

