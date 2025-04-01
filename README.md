
## AttractiveITN

<!-- badges: start -->
<!-- badges: end -->
### Overview
This repository contains the code and data necessary to reproduce the analyses presented in our study entitled
_The modelled potential of attractive insecticide-treated nets (ITNs) to reduce malaria transmission_

### Repository Structure

- **R/**: Contains R scripts for data analysis and model simulations.
    - 'Simulations&Figure.R' helps reproduce all simulations, figures 2 to 5, and supplementary figures.
    - 'Uncertainty&Sensitivity.R' helps reproduce sensitivity analyses and figure 6.
    - 'Fun_VLAIB_fRTP.R' contains R functions of the model
    - 'Set_parameters.R' contains codes to set parameters values based on literature and field data 
    - 'my_ggarrange.R' contains a customized ggpubr::ggarrange function that allow to use the legend of any of the arranged plots as a common legend.
- **data/**: Includes datasets used in the study.
    - 'Data_Moiroux.txt' data from Moiroux et al. 2017 doi:10.1371/journal.pone.0170732
    - 'Data_Strode.txt' data from Strode et al. 2014 doi:10.1371/journal.pmed.1001619
    - 'Data_Figure2_Moiroux.txt' data of Figure 2 (panels A & B) in Moiroux et al. 2017 (doi:10.1371/journal.pone.0170732) containing deterence rates-ratios and confidence intervals

### Citation

If you use this code or data in your research, please cite our study as follows:

```
Author(s). (Year). Title of the study. Journal Name. DOI or URL
```