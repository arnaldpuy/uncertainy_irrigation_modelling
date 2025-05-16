
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14710666.svg)](https://doi.org/10.5281/zenodo.14710666)

# Fifty years of research have deepened uncertainties in global irrigation water use

[Arnald Puy](https://www.arnaldpuy.com/), Carmen Aguiló-Rivera, Seth N. Linga, Naomi Clarke, Lieke A. Melsen

This study compiles 1,624 global irrigation water withdrawal estimates (1990–2100) 
from literature published between 1974 and 2025 and analyzes the uncertainty spread 
to determine whether it has increased or decreased over time.

## Abstract

*For five decades, improving models and datasets has been central to reducing uncertainty 
in global irrigation water withdrawal estimates. However, whether these efforts have 
succeeded remains unclear. Here we compile 1,624 global estimates from 65 studies 
published between 1974 and 2025, and evaluate them across 320 analytical pathways in a
multiverse analysis framework. Contrary to expectations, our results show that uncertainty 
in global irrigation water withdrawals has not declined but has instead increased or 
become more unstable. Only 17% of analytical paths show a decline in uncertainty over 
time, falling to 10% when projections for 2100 are excluded. Our study thus suggests 
that uncertainties are likely to persist despite further research, and that embracing 
rather than attempting to dispel them may offer a stronger, more robust understanding 
of global irrigation water use.*

## Replication

We provide all the functions needed to replicate our workflow in the "functions" folder.

### Data

#### Generated data

The "dataset" folder contains the data produced in this study. 

* `iww_dataset.csv`: all irrigation water withdrawal data points compiled and examined in our paper.

* `model_and_dataset_updates.xlsx`: the most important refinements and updates 
implemented over the last thirty years in the irrigation module of nine global models
(CLM, PCR-GLOBWB, VIC, WaterGAP,WBMPlus, H08, LPJmL, MATSIRO and MPI-HM) and in 
three key irrigation datasets (irrigation efficiency, crop type and irrigated area).

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

