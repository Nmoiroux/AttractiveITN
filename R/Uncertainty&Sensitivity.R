# Uncertainty and Sensitivity analysis
# Figure 6


# load required packages, functions & default parameters values 
library(pse)    # Hypercube sampling and uncertainty/sensitivity analysis
#as pse in no longer on CRAN, use commented lines below to install the last CRAN version
#require(devtools)
#install_version("pse", version = "0.4.7", repos = "http://cran.us.r-project.org")
library(ggpubr) # figure customizing
library(ggtext) # figure customizing
source("R/Fun_VLAIB_fRTP.R") # models functions
source("R/Set_parameters.R") # parameters default and ranges

## set preference values for the comparison of transmission
# this value are used inside the `fRTP_sens` function
pref <- 0.6					# in the sensitivity analysis, we will compare an slightly attractive LLIN to
pref_ref <- 0.36    # an attractive one (pLLIN=0.36)


### Uncertainty analysis following tutorial of the 'pse' package ----
## prepare data and functions for uncertainty analysis----
# names of parameters
factors <- c("S", "g", "Nh", "Ih", "k", "n", 
						 "m1u" ,   
						 "m1p",    
						 "m2u",    
						 "m2p",     
						 "Du",       
						 "Dp",     
						 "Uh",     
						 "pi"
						 #"Pllin" 
) 

# the probability density functions for each parameter (uniform or discrete uniform)
qdunif<-function(p, min, max) floor(qunif(p, min, max)) # discrete uniform probablity function (for parameters g and n)

q <- c("qunif",  #S
			 "qdunif", #g
			 "qunif",  #Nh
			 "qunif",  #Ih
			 "qunif",  #k
			 "qdunif", #n
			 "qunif",  #m1u
			 "qunif", #m1p     
			 "qunif",  #m2u
			 "qunif",  #m2p
			 "qunif", #Du
			 "qunif", #Dp
			 "qunif", #Uh     
			 "qunif" #pi 
			 #"qunif"  #Pllin
)

## a list containing the lists with all the parameters ranges to the density functions
q.arg <- list(S_rg, #S
							g_rg, #g
							Nh_rg, #Nh
							Ih_rg,  #Ih
							k_rg, #k
							n_rg,     #n
							m1u_rg,   #m1u
							m1p_rg,  #m1p # TESTED 
							m2u_rg,   #m2u
							m2p_rg, #m2p
							Du_rg, #Du
							Dp_rg,   #Dp # TESTED 
							Uh_rg,   #Uh # TESTED 
							pi_rg   #pi # TESTED 
							#list(min=0.2, max=0.4)  #Pllin
)

## function `modelRun` encapsulates fRTP_sens function, in a manner to receive a data.frame containing
# all parameter combinations and returning the results in one array.
modelRun <- function (my.data) {
	return(mapply(fRTP_sens, 100, my.data[,1], my.data[,2], my.data[,3], my.data[,4], my.data[,5], my.data[,6],
								my.data[,7], my.data[,8], my.data[,9], my.data[,10]
								, my.data[,11], my.data[,12], my.data[,13], my.data[,14], pref, pref_ref
	))
}

## Generates the Latin Hypercube Sampling and simulates results for uncertainty and sensitivity analyses----

set.seed(123) # for reproducibility

if(!(exists("myLHS"))){
	myLHS <- pse::LHS(modelRun, factors, 500, q, q.arg, nboot=50, res.names = "% reduction in transmission potential")
}

# accessing the result dataframe from an LHS object
res_LHS <- pse::get.results(myLHS) %>%
	sort() %>%
	as.data.frame()


# extract summary values: median, mean and 95% credible interval for transmission reduction
median(res_LHS[,1]) # median red. in transmission potential
mean(res_LHS[,1]) # mean red. in transmission potential
quantile(res_LHS[,1], probs = c(0.05, 0.95)) # 95% credible interval


# Estimates the partial slope (i.e. regression) coefficient of the model response in relation with all model input variables----
pic <- pse::pic(myLHS, nboot=100)[[1]]$pic


# plots the partial rank correlation coefficient (PRCC) from an LHS object (using ggplot2, suppFig3A)----
## parameters names to print (markdown)
var_plot_lab <- c("*S*", "*g*",  "*I~h~*", "*k*","*n*", 
									"*D~u~*", "*µ~1u~*", "*µ~2p~*", "*µ~2u~*", "*N~h~*") 
## variable to select (for plotting in sensitivity analysis)
var_plot <- c("S", "g",  "Ih", "k","n", 
							"Du", "m1u", "m2p", "m2u", "Nh") 

## plots PRCC
prcc_plot <- myLHS$prcc[[1]]$PRCC %>%
	mutate(var= rownames(.) %>% factor(levels = var_plot)) %>%
	filter(var %in% var_plot) %>%
	ggplot(aes(x=var,y=original)) + 
	geom_pointrange(aes(ymin=`min. c.i.`, ymax=`max. c.i.`)) +
	ylab("PRCC") +
	xlab("Parameters") +
	ylim(-1,1) +
	scale_x_discrete(labels=var_plot_lab)+
	theme(axis.text.x = element_markdown())
	

# plots model response over parameters ranges with loess curve (suppFig3B)----
dat <- pse::get.data(myLHS) 
res <- pse::get.results(myLHS)
dat <- dat %>% cbind(res) %>% mutate(g = jitter(g), n = jitter(n)) # jitter discrete data for plotting purpose

dat_plot <- dat %>% 
	pivot_longer(cols = c(1:14), names_to = "var") %>% 
	mutate(var = var %>% factor(levels = var_plot)) %>%
	filter(var %in% var_plot)  %>%
	mutate(var = fct_drop(var))

sens_plot <- ggplot(dat_plot, aes(x=value, y=res)) + 
	facet_wrap(~var, scales = "free", drop=FALSE, 
						 labeller = labeller(var = function(string){var_plot_lab[which(var_plot == string)]})) + # labeller to print parameters names with italics...
	scale_x_continuous(guide = guide_axis(check.overlap = TRUE)) +
	geom_point(size = 0.5) +
	ylab("Change in transmission potential (%)") +
	geom_smooth() +
	theme(strip.text.x = element_markdown()) 



# figure 6
figure6 <- ggarrange(prcc_plot, 
												sens_plot, 
												labels = c("A", "B"),
												ncol = 1, nrow = 2, heights = c(0.5, 1))

# save figure as pdf
width <- 16
ggsave(plot = figure6, filename =  "Fig6.pdf",  width = width, height = width, units = "cm")


# table prcc & pic
prcc_tbl <- myLHS$prcc[[1]]$PRCC %>%
	mutate(var= rownames(.) %>% factor(levels = var_plot)) %>%
	filter(var %in% var_plot) %>%
	select(var, original,'min. c.i.','max. c.i.') %>%
	rename(prcc = "original",min = 'min. c.i.',max = 'max. c.i.')

pic_tbl <- pic %>%
	mutate(var= rownames(.) %>% factor(levels = var_plot)) %>%
	filter(var %in% var_plot) %>%
	select(var, original,'min. c.i.','max. c.i.') %>%
	rename(pic = "original",min = 'min. c.i.',max = 'max. c.i.')

pic_prcc_tbl <- prcc_tbl %>%
	left_join(pic_tbl, by="var") %>%
	arrange(desc(abs(prcc)))
