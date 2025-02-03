
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14710666.svg)](https://doi.org/10.5281/zenodo.14710666)

# Uncertainty in global irrigation water use persists after 50 years of research

[Arnald Puy](https://www.arnaldpuy.com/), Carmen Aguiló, Seth N. Linga, Naomi Clarke, Nanxin Wei

This study compiles the estimations of irrigation water withdrawals produced between
1970 and 2024 and analyzes the spread of the uncertainty to see if it has increased
or decreased over time.

## Abstract

*In the last decades, efforts to estimate global irrigation water withdrawals have 
intensified to better quantify human impacts on the water cycle. Through a computational 
multiverse analysis of 1,200 data points from 40 studies published over a 50 year-span 
(1974–2025), we find that these efforts have not led to more precise estimates, but to 
uncertainty evolving randomly through time. Evidence for research reducing uncertainty 
is just as strong as for research increasing it, with perceived trends fully depending 
on the methodology used to analyze the data. Our findings suggest that advancing our 
understanding of the global impact of irrigation requires not more estimates, but deeper 
insights into how uncertainties and methodological decisions shape irrigation modelling.*

## Replication

We provide all the functions needed to replicate our workflow in the ``functions'' folder.

### Data

#### Generated data

The ''data'' folder contains the data and files produced in this study. The file named
''current'' compiles studies whose water estimates are either historical or current.
The file named ```projected'' includes studies with water-related variables projected 
into the future.

* `references_current.xlsx`   
* `references_projected.xlsx`   

#### Secondary data

Since we also used secondary data, we refer the interested reader to the following
links and repositories:

* [ISI-MIP](https://www.isimip.org/)
* [Khan et al 2023](https://www.nature.com/articles/s41597-023-02086-2)

### Code

We offer the code in `.R`, `.pdf` and `.Rmd`. Our entire workflow can be run and the 
results replicated from either of these files. The user must run the code from the 
same folder where the files in the primary data section are stored for a successful 
compilation.

