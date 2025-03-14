
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.14710666.svg)](https://doi.org/10.5281/zenodo.14710666)

# Uncertainty in global irrigation water use persists after 50 years of research

[Arnald Puy](https://www.arnaldpuy.com/), Carmen Aguiló, Seth N. Linga, Naomi Clarke

This study compiles 1408 global irrigation water withdrawal estimates (1990–2100) 
from literature published between 1974 and 2025 and analyzes the uncertainty spread 
to determine whether it has increased or decreased over time.

## Abstract

*For fifty years, refining models and datasets has been central to reducing 
uncertainties in the quantification of global irrigation water use. However, 
whether these efforts have delivered remains unclear. Here we compile 1,408 
global irrigation water withdrawal estimates from 65 studies (1974-2025) and 
examine them through 320 different possible analytical pipelines to ensure 
robustness of results. Our analysis reveals strong evidence that, rather than 
reducing uncertainties, research has actually amplified them or made them 
unstable. Only 20\% of analytical pathways suggest a decline in uncertainty 
over time, dropping to 10\% when excluding projections for 2100. Our findings 
suggest that embracing uncertainty may be a better strategy than chasing 
precision to improve our understanding of global irrigation water use.*

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

