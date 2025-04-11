
## AttractiveITN

<!-- badges: start -->
<!-- badges: end -->
### Overview
This repository contains the supplementary materials, as well as codes and data necessary to reproduce the analyses presented in our study entitled
_Potential of attractive insecticide-treated nets (ITNs) to reduce malaria transmission: a modeling study_

### Repository Structure
- 'Supplementary materials (Figures).pdf' contains supplementary figures 1, 2 & 3.
- **R/**: Contains R scripts for data analysis and model simulations.
    - 'Simulations&Figure.R' helps reproduce simulations made for Figures 2 to 5, as well as for Supplementary figures 1 & 2.
    - 'Uncertainty&Sensitivity.R' helps reproduce sensitivity analyses and Figure 6.
    - 'Simulations_vs_NoNets.R' helps reproduce simulation made for Supplementary figure 3.
    - 'Fun_VLAIB_fRTP.R' contains R functions of the model
    - 'Set_parameters.R' contains codes to set parameters values based on literature and field data 
    - 'my_ggarrange.R' contains a customized ggpubr::ggarrange function that allow to use the legend of any of the arranged plots as a common legend.
- **data/**: Includes datasets used in the study.
    - 'Data_Moiroux.txt' data from Moiroux et al. 2017, doi:10.1371/journal.pone.0170732, used for supplementary Figure 1 & 2 and parameterization.
    - 'Data_Strode.txt' data from Strode et al. 2014 doi:10.1371/journal.pmed.1001619, used for supplementary Figure 1.
    - 'Data_Figure2_Moiroux.txt' data of Figure 2 (panels A & B) in Moiroux et al. 2017 (doi:10.1371/journal.pone.0170732) containing deterrence rates-ratios and confidence intervals, used to define preference parameter value for deterrent ITN.
    - 'Metadata.txt' description file for 'Data_Moiroux.txt', 'Data_Strode.txt', and 'Data_Figure2_Moiroux.txt'

### Citation

If you use this code or data in your research, please cite our study as follows:

```
Moiroux Nicolas & Pennetier Cédric. (2025). Potential of attractive insecticide-treated nets (ITNs) to reduce malaria transmission: a modeling study. doi.
```