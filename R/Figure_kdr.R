#### figure 6 (kdr dynamics) ----
#### parameters (only based on pre-bite mortality)
m1p_kdr     <- c(0.05,0.5,0.95)     # pre-bite mortality when faced to an LLIN of genotype RR, RS and SS respectively (Diop et al 2015, permethrin)
m2p_kdr     <- c(0.005,0.005,0.005) # post-bite mortality when faced to an LLIN of genotype RR, RS and SS respectively 
N.gen <- 100 												# nb of generations to simulate

# create a dataframe with different combinations of preference (Pllin) per genotypes
df_pref <- as.data.frame(matrix(rep(c(0.6,0.5,0.36),3), 3,3))  # same pref. value for all genotypes, various level of preference (attraction, neutral, deterrence)
df_pref[4,] <- c(0.6,0.5,0.5)																  # only RR genotype attracted (as in Poriciani et. al 2017)

# loop that calculate relative fitness for each combination of preference and simulate kdr dynamic accordingly
v_kdr <- NULL                                                        # vector that will receive allelic frequency values
for (i in (1:nrow(df_pref))){
	W_F <- fitness_f_kdr(as.numeric(df_pref[i,]),m1p_kdr,m2p_kdr)      # relative fitness of females
	v_kdr <- c(v_kdr, predict_kdr4(W_F=W_F,W_M=c(1,1,1), N.gen=N.gen)) # vector of R allele frequency
}

# data frame that will receive the results
df_kdr <- merge(data.frame(1:N.gen), df_pref, by=NULL)				
df_kdr$fkdr <- v_kdr

# prepare data for the plot's legend
df_kdr$scn <- as.factor(paste(df_kdr$V1, df_kdr$V2, df_kdr$V3))
levels(df_kdr$scn) <- c("deterrent for all genotypes", "inert for all genotypes", "attractive for RR and inert for others", "attractive for all genotypes")

# plot 
figure6 <- ggplot(df_kdr, aes(x=X1.N.gen-1, y=fkdr)) + 
	xlab("Time (discrete generations)") + ylab("Kdr allele frequency") +
	geom_line(aes(linetype=fct_rev(scn))) +
	scale_linetype_discrete(name="LLIN remote effect")+
	xlim(0,30) + 
	theme(aspect.ratio=1) 
