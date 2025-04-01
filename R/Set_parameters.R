# this file show how default values of parameters and ranges for sensitivity 
# analysis were defined or calculated. Used to feed Table 1 of the manuscript

library(tidyverse)

### set or calculate defaults values of model parameters ----
## determine reference value of preference for deterrent LLIN (mean of pref among LLIN that were significantly deterrent in Moiroux et al. 2017)
Data_deterence <- read.delim("data/Data_Figures2_Moiroux.txt")
pLLIN_ref_det <- Data_deterence %>% 
	filter(IC_high < 1) %>%	       # select only significantly deterrent ITNs
	mutate(pLLIN=RR/(RR+1)) %>%    # convert Rate-ratio to preference values
	summarise(mean= mean(pLLIN)) %>%
	round(2) %>%
	unlist

## set parameters default values according to literature
S_ref <- 0.9   # baseline daily survival, Davidson 1954, Nature
g_ref <- 3     # gonotrophic cycle duration, Afrane et al. 2005, JME; Carnevale & Robert 2009
k_ref <- 0.1   # infectiousness, Churcher et al. 2015, Nat. Comm
n_ref <- 11    # sporogony duration, according to the formula of Detinova 1962, at 26°C
Uh_ref <- 0.49 # LLIN use rate, WHO world malaria report 2023
pi_ref <- 0.7  # Personal protection by LLIN, Mean of data reported in Monroe et al 2019 & Soma et al 2021 (mean(c(0.7,0.59,0.38, 0.8, 0.87, 0.51, 0.8,0.82,0.85)) )
Du_ref <- 0.12 # Diversion probability (as the ratio of unfed alive mosquitoes over the total entering mosq. in EHT with no nets), Curtis 85, Majori 87.

## set parameters default values according to Moiroux et al. 2017
# load summarized data of Experimental hut trials (EHT) from Moiroux et al. 2017
Data_moiroux <- read_delim("data/Data_Moiroux.txt")	

range_moiroux <- Data_moiroux %>%
	dplyr::mutate(m1 = Tot_D_unfd / (total-Tot_L_unfd)) %>% 	       # calculate pre-bite mortality
	dplyr::mutate(m2 = Tot_D_fed / Total_bfed) %>%  	                   # calculate post-bite mortality
	dplyr::mutate(D = Tot_L_unfd / total) %>%   	                       # calculate Diversion rate
	group_by(ITN) %>% 																			             # group by type of treatment (ITN, CTN or control UTN)
	summarise_at(c("m1","m2","D"),funs(min,max,mean), na.rm=TRUE)    # calculate min and max values of m1, m2 and D

range_moiroux[3,1] <- "UTN"

# mean valued for pre-bite, post-bite mortality and diversion for untreated nets and LLINs
# to be used as default values
m1u_ref <- range_moiroux[2:3,c(1,8:10)][2,2] %>% unlist()
m2u_ref <- range_moiroux[2:3,c(1,8:10)][2,3] %>% unlist()
m1p_ref <- range_moiroux[2:3,c(1,8:10)][1,2] %>% unlist()
m2p_ref <- range_moiroux[2:3,c(1,8:10)][1,3] %>% unlist()
#Du_ref  <- range_moiroux[2:3,c(1,8:10)][2,4]
Dp_ref  <- range_moiroux[2:3,c(1,8:10)][1,4] %>% unlist()

# set other parameters (user defined)
Nh_ref <- 1000 # Size of the human community
Ih_ref <- 0.5  # plasmodium infection prevalence



### define range (min and max) values for each parameters (for uncertainty/sensitivity analysis and plots) ----
## from literature
S_rg   <- list(min=0.61, max=0.98) # Silver 2008, Chapter 13
g_rg   <- list(min=2, max=6)			 # Afrane et al. 2005, JME; Carnevale & Robert 2009
k_rg   <- list(min=0.02, max=0.2)  # Churcher et al. 2015, Nat. Comm
n_rg   <- list(min=8, max=16)			 # Hien 2016, Plos Path; Ohm et al. 2018, Par.& Vec.
Uh_rg  <- list(min=0.1, max=0.9)	 # Malaria Atlas Project 
Ih_rg  <- list(min=0.1, max=0.9)   # Malaria Atlas Project 
pi_rg  <- list(min=0.38, max=0.9)  # Monroe et al. 2019, Soma et al. 2021, 
Du_rg  <- list(min=0, max=0.3)     # from 0 to the ~triple observed in Curtis 85 & Majori 87


## user defined
Nh_rg  <- list(min=300, max=5000)
   

## find min and max values of pre-bite, post-bite mortality and diversion for untreated nets and LLINs
# from Moiroux et al. 2017
range_UTN <- range_moiroux %>% filter(ITN=="UTN") %>% select(-ITN) %>% as.data.frame()
range_ITN <- range_moiroux %>% filter(ITN=="ITN")%>% select(-ITN) %>% as.data.frame()

m1u_rg <- list(min=range_UTN[1,1], max=range_UTN[1,4])
m2u_rg <- list(min=range_UTN[1,2], max=range_UTN[1,5])
m1p_rg <- list(min=range_ITN[1,1], max=range_ITN[1,4])
m2p_rg <- list(min=range_ITN[1,2], max=range_ITN[1,5])
Dp_rg  <- list(min=range_ITN[1,3], max=range_ITN[1,6])

	


