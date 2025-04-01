###### create Data_moiroux, a file that summarise data from Moiroux 2017 ----

Data_meta <- read.delim("data/DB_meta_AnGambiae.txt")
Data_meta$Total_unfd <- Data_meta$Tot_L_unfd + Data_meta$Tot_D_unfd

# create a new column categorizing nets in three categorie (no=control, CTN and ITN)
Data_meta$ITN <- as.factor(Data_meta$ttmt2)
levels(Data_meta$ITN)[1] <- "no"
levels(Data_meta$ITN)[grep("CTN",levels(Data_meta$ITN))] <- "CTN"
levels(Data_meta$ITN)[!(levels(Data_meta$ITN) %in% c("no","CTN"))] <- "ITN"

# create summarised data for sharing
Data_meta %>% group_by(Place, Eval, ttmt, wash, ITN) %>%   # group by treatments
	summarise_at(c(22:29),sum) %>% as.data.frame() -> Data_moiroux						   # sum some columns

write_tsv(Data_moiroux, path="data/Data_moiroux.txt")
	

### plot prefernce value recorded by EHT in nature from two meat-analysis studies ----
# calculate prefernce value from field data
# from EHT in Moiroux et al. 2017, Plos One
Data_moiroux <- read.delim("data/Data_moiroux.txt")
Data_moiroux %>% 
	dplyr::filter(ITN == "no") %>% 
	select(Eval, total) %>% 
	right_join(Data_moiroux,by="Eval") %>% 
	#dplyr::filter(ITN == "ITN") %>%
	dplyr::filter(ITN != "no") %>%
	select(ttmt,total.x,total.y) %>%
	mutate(pLLIN = total.y/(total.y+total.x)) %>%
	mutate(study = "moiroux") %>%
	rename(Net = ttmt, TotalUTN = total.x, TotalITN = total.y) -> Data_pref_moiroux

# from EHT in Strode et al. 2014, Plos Med
Data_strode <- read.delim("data/Data_strode.txt")
Data_strode %>% 
	#filter(Intervention=="LLIN") %>%
	select(Net, TotalITN, TotalUTN) %>%
	mutate(study = "strode") %>%
	mutate(pLLIN = TotalITN / (TotalITN+TotalUTN)) -> Data_pref_strode

# bind the two dataframe together											 
Data_pref <- rbind(Data_pref_moiroux, Data_pref_strode)

# plot
qplot( x=study , y=pLLIN , data=Data_pref , geom=c("boxplot","jitter") ,  ylab="Preference for treated net (pLLIN)")

#### justify baseline value (m1, m2, D) from moiroux 2017 :
Data_moiroux <- read.delim("data/Data_moiroux.txt")	

Data_moiroux %>%
	dplyr::mutate(m1 = Tot_D_unfd / Total_unfd) %>% 	       # calculate pre-bite mortality
	dplyr::mutate(m2 = Tot_D_fed / Total_bfed) %>%  	       # calculate post-bite mortality
	dplyr::mutate(D = Tot_L_unfd / total) -> Data_moiroux2 
Data_moiroux2 %>%   	           # calculate Diversion rate
	group_by(ITN) %>% 																			 # group by type of tretament (ITN, CTN or control)
	summarise_at(c("m1","m2","D"),mean, na.rm=TRUE) -> mean_moiroux # calculate mean values of m1, m2 and D

Data_moiroux2 %>% 
	select(ttmt, ITN, m1, m2, D) %>%
	gather("m1","m2","D", key ="parameter", value = "value") -> plot_parameters

plot_parameters %>%
	filter(ITN == "no") -> plot_param_UTN

plot_parameters %>%
	filter(ITN == "ITN") -> plot_param_ITN

fig_param_A <- ggplot(plot_param_UTN, aes(x=parameter, y=value, fill=parameter)) +
	geom_boxplot(alpha=0.4) +
	stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
	theme(legend.position="none") +
	scale_fill_brewer(palette="Set3") +
	labs(y=" ")+
	ylim(0,1)+
	scale_x_discrete(labels=c(expression(paste('D'['U'])), expression(paste('µ'['1U'])), expression(paste('µ'['2U']))))+ 
	theme(axis.title.x = element_blank())

fig_param_B <- ggplot(plot_param_ITN, aes(x=parameter, y=value, fill=parameter)) +
	geom_boxplot(alpha=0.4) +
	stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
	theme(legend.position="none") +
	scale_fill_brewer(palette="Set3") +
	labs(y=" ")+
	ylim(0,1)+
	scale_x_discrete(labels=c(expression(paste('D'['P'])), expression(paste('µ'['1P'])), expression(paste('µ'['2P']))))+
	theme(axis.title.x = element_blank())

figure_param <- ggarrange(fig_param_A, 
										 fig_param_B, 
										 labels = c("A", "B"),
										 ncol = 2, nrow = 1)

annotate_figure(figure_param,
								bottom = text_grob("Parameters"),
								left = text_grob("value", rot=90)#,
								#fig.lab = "Figure 1", fig.lab.face = "bold"
)