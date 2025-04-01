
# load required packages, functions & default parameters values 
source("R/Set_parameters.R") # parameters default and ranges
source("R/Fun_VLAIB_fRTP.R") # behavior and transmission mddel functions
source("R/Fun_fitness_f_kdr.R") # relative fitness of genotypes (function)
source("R/Fun_predict_kdr.R") # kdr frequency prediction (function)


#### figure 5 (kdr dynamics) ----
#### parameters (only based on pre-bite mortality)
m1p_kdr     <- c(0.05,0.5,0.95)     # pre-bite mortality when faced to an LLIN of genotypes RR, RS and SS respectively (Diop et al 2015, permethrin)
m2p_kdr     <- c(0.005,0.005,0.005) # post-bite mortality when faced to an LLIN of genotypes RR, RS and SS respectively 
N.gen <- 100 												# nb of generations to simulate

# create a dataframe with different combinations of preference (Pllin) per genotypes
df_pref <- as.data.frame(matrix(rep(c(0.6,0.5,0.36),3), 3,3))  # same pref. value for all genotypes, various level of preference (attraction, neutral, deterrence)
df_pref[4,] <- c(0.6,0.5,0.5)																  # only RR genotype attracted (as in Poriciani et. al 2017)

# loop that calculate relative fitness for each combination of preference and simulate kdr dynamic accordingly
v_kdr <- NULL # vector that will receive allelic frequency values
W_Ft <- data.frame("W_RR"=double(),"W_RS"=double(),"W_SS"=double())

for (i in (1:nrow(df_pref))){
	W_F <- fitness_f_kdr(as.numeric(df_pref[i,]),m1p_kdr,m2p_kdr)      # relative fitness of females
	W_Ft[i,] <- W_F
	v_kdr <- c(v_kdr, predict_kdr4(W_F=W_F,W_M=c(1,1,1), N.gen=N.gen)[[1]]) # vector of R allele frequency
}

# data frame that will receive the results
df_kdr <- merge(data.frame(1:N.gen), df_pref, by=NULL)				
df_kdr$fkdr <- v_kdr

# prepare data for the plot's legend
df_kdr$scn <- as.factor(paste(df_kdr$V1, df_kdr$V2, df_kdr$V3))
levels(df_kdr$scn) <- c("deterrent for all genotypes", "inert for all genotypes", "attractive for RR and inert for others", "attractive for all genotypes")

# plot 
supp_fig3 <- ggplot(df_kdr, aes(x=X1.N.gen-1, y=fkdr)) + 
	xlab("Time (generations)") + ylab("Kdr allelic frequency") +
	geom_line(aes(linetype=fct_rev(scn))) +
	scale_linetype_discrete(name="LLIN remote effect")+
	xlim(0,40) + 
	theme(aspect.ratio=1) 


## search number of generations needed to reach allelic frequency of 0.8
supp_fig3$data %>% group_by(scn) %>% summarise(val = which.min(abs(fkdr-0.8)))

# width <- 16
# ggsave(plot = supp_fig3, filename =  "suppFig3.pdf",  width = width, height = width*0.56, units = "cm")
