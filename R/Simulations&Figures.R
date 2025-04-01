# Simulations and figures (1 to 5 and supp 1 & 2))


# load required packages, functions & default parameters values ----
source("R/Fun_VLAIB_fRTP.R") # models functions
source("R/Set_parameters.R") # parameters default values and ranges
library(ggpubr) # figure customizing
library(ggtext) # figure customizing
insertSource("R/my_ggarrange.R", package = "ggpubr") # customized ggpubr::ggarrange function (allow to pick the legend of any arranged plots as common legend)


# Variables common to all analysis ----
## common value for Fig 2-5
y_title <- "Change in transmission\n potential (%)"
FUN <- TP                # function used to calculate transmission potential
## legends titles and labels
legend_title <- "ITN: "
leg_label_A <- c("attractive (*p~ITN~* = 0.6)","attractive (*p~ITN~* = 0.7)")
leg_label_B <- c("inert (*p~ITN~* = 0.5)","attractive (*p~ITN~* = 0.6)","attractive (*p~ITN~* = 0.7)")


#### Effect of ITN use (Figure 2) ----
# common parameters to panels A & B (Figure 2)
use <- seq(0,1,by = 0.01)	  # use ITN values used for simulations

# panel A
p<- c(0.6,0.7)		      # preference for ITN protected human (against unprotected human) 
ref_pref <- 0.5					# preference value of the reference ITN (inert = 0.5)


RTP.fit <- expand.grid(p.=p,use.=use)   # create table of all combinations of pITN and use
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(use.,p.),~fRTP(p=.y, Uh=.x, Uh2=.x, p2=ref_pref,FUN= FUN))) # calculates TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in TP

fig2_A <- ggplot(RTP.fit, aes(x=use.*100, y=redVC)) + 
	xlab("ITN use (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(2,3),name=legend_title, labels=leg_label_A)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())

# panel B
p<- c(0.5,0.6,0.7)		      # preference for ITN protected human (against unprotected human) 
ref_pref <- pLLIN_ref_det 	# preference value of the reference ITN (deterrent = 0.36)

RTP.fit <- expand.grid(p.=p,use.=use)   # create table of all combinations of p and use
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(use.,p.),~fRTP(p=.y, Uh=.x, Uh2=.x, p2=ref_pref,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in TP

fig2_B <- ggplot(RTP.fit, aes(x=use.*100, y=redVC)) + 
	xlab("ITN use (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(1,2,3),name=legend_title, labels=leg_label_B)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())


# arrange panels of figure 2
figure2 <- ggarrange(fig2_A + rremove("legend"), 
										 fig2_B + rremove("legend"), 
										 common.legend = TRUE,
										 plot_legend = 2,
										 #legend = "bottom",
										 labels = c("A", "B"),
										 ncol = 2, nrow = 1)

figure2 <- annotate_figure(figure2,
													 bottom = text_grob("ITN use (%)"),
													 left = text_grob(y_title, rot=90))

# plot figure
figure2

## search use rates for which reduction in transmission is the higher
fig2_A$data %>% group_by(p.) %>% summarise(min=min(redVC)) -> minVC
fig2_A$data %>% filter(redVC %in% minVC$min) -> fig2A_min

fig2_B$data %>% group_by(p.) %>% summarise(min=min(redVC)) -> minVC
fig2_B$data %>% filter(redVC %in% minVC$min) -> fig2B_min

fig2A_min$use
fig2B_min$use


### Effect of physiological resistance (Figure 3)----
# common parameters to panels A & B (Figure 3)
m <- seq(0,1,by = 0.01)	  # pre-bite mortality

# panel A
p<- c(0.6,0.7)		      # preference for ITN protected human (against unprotected human) 
ref_pref <- 0.5							# preference value of the reference ITN (inert = 0.5)

RTP.fit <- expand.grid(p.=p,m.=m)   # create table of all combinations of p and m
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(m.,p.),~fRTP(p=.y, m=.x, m2=.x, p2=ref_pref,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in transmission potential

fig3_A <- ggplot(RTP.fit, aes(x=(1-m.)*100, y=redVC)) + 
	xlab("Feeding attempt survival (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(2,3),name=legend_title)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())+
	geom_rect(aes(xmin = (1-m1p_rg$min)*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)


# panel B
p<- c(0.5,0.6,0.7)		      # preference for ITN protected human (against unprotected human) 
ref_pref <- pLLIN_ref_det		# preference value of the reference ITN (detterent = 0.36)

RTP.fit <- expand.grid(p.=p,m.=m)   # create table of all combinations of p and m
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(m.,p.),~fRTP(p=.y, m=.x, m2=.x, p2=ref_pref,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in transmission potential TP

fig3_B <- ggplot(RTP.fit, aes(x=(1-m.)*100, y=redVC)) + 
	xlab("feeding attempt survival (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(1,2,3),name=legend_title, labels=leg_label_B)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())+
	geom_rect(aes(xmin = (1-m1p_rg$min)*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)


# arrange panels of figure 3
figure3 <- ggarrange(fig3_A + rremove("legend"), 
										 fig3_B + rremove("legend"), 
										 common.legend = TRUE,
										 plot_legend = 2,
										 labels = c("A", "B"),
										 ncol = 2, nrow = 1)

figure3 <- annotate_figure(figure3,
													 bottom = text_grob("Feeding attempt survival (%)"),
													 left = text_grob(y_title, rot=90))

figure3
## search reduction in transmission value for survival set to m1p_rg$min (22% mortality: max resistance in field)
fig3_A$data %>% filter(m. == round(m1p_rg$min,2)) -> fig3A_hi
fig3_B$data %>% filter(m. == round(m1p_rg$min,2)) -> fig3B_hi
## search reduction in transmission value for survival set to max (100% mortality: no resistance)
fig3_A$data %>% filter(m. == 1) -> fig3A_lo
fig3_B$data %>% filter(m. == 1) -> fig3B_lo


### Effect of behavioral quantitative resistance (Figure 4)----
# common parameters to panels A & B (Figure 4)
d <- seq(0,1,by = 0.01)	  # diversion probability

# panel A
p<- c(0.6,0.7)		        # preference for ITN protected human (against unprotected human) 
ref_pref <- 0.5						# preference value of the reference ITN (inert = 0.5)

RTP.fit <- expand.grid(p.=p,d.=d)   # create table of all combinations of p and d
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(d.,p.),~fRTP(p=.y, D=.x, D2=.x, p2=ref_pref,FUN= FUN))) # calculate VC ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in VC

fig4_A <- ggplot(RTP.fit, aes(x=d.*100, y=redVC)) + 
	xlab("Diversion (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(2,3),name=legend_title)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank()) +
	theme(legend.text = element_markdown())+
	geom_rect(aes(xmin = Dp_rg$max*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)

# panel B
p<- c(0.5,0.6,0.7)		      # preference for ITN protected human (against unprotected human) 
ref_pref <- pLLIN_ref_det		# preference value of the reference ITN (detterent = 0.36)

RTP.fit <- expand.grid(p.=p,d.=d)   # create table of all combinations of p and d
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(d.,p.),~fRTP(p=.y, D=.x, D2=.x, p2=ref_pref,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in transmission potential

fig4_B <- ggplot(RTP.fit, aes(x=d.*100, y=redVC)) + 
	xlab("Diversion (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(1,2,3),name=legend_title, labels=leg_label_B)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())+
	geom_rect(aes(xmin = Dp_rg$max*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)


# arrange panels of figure 4
figure4 <- ggarrange(fig4_A + rremove("legend"), 
										 fig4_B + rremove("legend"), 
										 common.legend = TRUE,
										 plot_legend = 2,
										 labels = c("A", "B"),
										 ncol = 2, nrow = 1)

figure4 <- annotate_figure(figure4,
													 bottom = text_grob("Diversion (%)"),
													 left = text_grob(y_title, rot=90))

figure4

## search reduction in transmission value for escaping set to 0 (no quantitative behavioral resistance)
fig4_A$data %>% filter(d. == 0) -> fig4A_lo
fig4_B$data %>% filter(d. == 0) -> fig4B_lo
## search reduction in transmission value for escaping set to Dp_rg$max (max quantitative behavioral resistance observed in the field)
fig4_A$data %>% filter(d. == round(Dp_rg$max,2)) -> fig4A_hi
fig4_B$data %>% filter(d. == round(Dp_rg$max,2)) -> fig4B_hi


### Effect of behavioral qualitative resistance (Figure 5)----
# common parameters to panels A & B (Figure 5)
pi <- seq(0,1,by = 0.01)	  # proportion of exposure to bite during which ITN is in use

# panel A
p<- c(0.6,0.7)		        # preference for ITN protected human (against unprotected human) 
ref_pref <- 0.5							# preference value of the reference ITN (inert = 0.5)

RTP.fit <- expand.grid(p.=p,pi.=pi)   # create table of all combinations of p and pi
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(pi.,p.),~fRTP(p=.y, pi=.x, pi2=.x, p2=ref_pref,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in TP

fig5_A <- ggplot(RTP.fit, aes(x=(1-pi.)*100, y=redVC)) + 
	xlab("Spatiotemporal avoidance (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(2,3),name=legend_title)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())+
	geom_rect(aes(xmin = (1-pi_rg$min)*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)

# panel B
p<- c(0.5,0.6,0.7)		      # preference for ITN protected human (against unprotected human) 
ref_pref <- pLLIN_ref_det		# preference value of the reference ITN (detterent = 0.36)

RTP.fit <- expand.grid(p.=p,pi.=pi)   # create table of all combinations of p and d
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(pi.,p.),~fRTP(p=.y, pi=.x, pi2=.x, p2=ref_pref,FUN= FUN))) # calculate VC ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in VC

fig5_B <- ggplot(RTP.fit, aes(x=(1-pi.)*100, y=redVC)) + 
	xlab("Spatiotemporal avoidance (%)") + 
	geom_line(aes(linetype=as.factor(p.))) +
	scale_linetype_manual(values=c(1,2,3),name=legend_title, labels=leg_label_B)+
	xlim(0,100) + ylim(-100,0)+
	theme(aspect.ratio=1) +
	theme(axis.title.y = element_blank())+
	theme(axis.title.x = element_blank())+
	theme(legend.text = element_markdown())+
	geom_rect(aes(xmin = (1-pi_rg$min)*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)


# arrange panels of figure 4
figure5 <- ggarrange(fig5_A + rremove("legend"), 
										 fig5_B + rremove("legend"), 
										 common.legend = TRUE,
										 plot_legend = 2,
										 #legend = "right",
										 labels = c("A", "B"),
										 ncol = 2, nrow = 1)

figure5 <- annotate_figure(figure5,
													 bottom = text_grob("Spatiotemporal avoidance (%)"),
													 left = text_grob(y_title, rot=90))

figure5

## search reduction in transmission value for spatial-temporal avoidance set to 0% (no qualitative behavioral resistance)
fig5_A$data %>% filter(pi. == 1) -> fig5A_lo
fig5_B$data %>% filter(pi. == 1) -> fig5B_lo
## search reduction in transmission value for spatial-temporal avoidance set to pi_rg$min (max avoidance = min % of exposure to bite occurs when ITN are not in use, field data)
fig5_A$data %>% filter(pi. == pi_rg$min) -> fig5A_mi
fig5_B$data %>% filter(pi. == pi_rg$min) -> fig5B_mi


# Save figure 2-5 as .pdf files (to be converted to tiff with PACE tool)----
width <- 16
ggsave(plot = figure2, filename =  "Fig2.pdf",  width = width, height = width*0.56, units = "cm")
ggsave(plot = figure3, filename =  "Fig3.pdf",  width = width, height = width*0.56, units = "cm")
ggsave(plot = figure4, filename =  "Fig4.pdf",  width = width, height = width*0.56, units = "cm")
ggsave(plot = figure5, filename =  "Fig5.pdf",  width = width, height = width*0.56, units = "cm")



# Supplementary Figure 1 plots preference value recorded by EHT in nature from two meta-analysis studies ----
## calculate preference value from field data
## from EHT in Moiroux et al. 2017, Plos One
Data_moiroux <- read.delim("data/Data_Moiroux.txt")
Data_pref_moiroux <- Data_moiroux %>% 
	dplyr::filter(ITN == "no") %>% 
	select(Eval, total) %>% 
	right_join(Data_moiroux,by="Eval") %>% 
	#dplyr::filter(ITN == "ITN") %>%
	dplyr::filter(ITN != "no") %>%
	select(ttmt,total.x,total.y) %>%
	mutate(pITN = total.y/(total.y+total.x)) %>%
	mutate(study = "moiroux") %>%
	rename(Net = ttmt, TotalUTN = total.x, TotalITN = total.y)

# from EHT in Strode et al. 2014, Plos Med
Data_strode <- read.delim("data/Data_Strode.txt")
Data_pref_strode <- Data_strode %>% 
	#filter(Intervention=="ITN") %>%
	select(Net, TotalITN, TotalUTN) %>%
	mutate(study = "strode") %>%
	mutate(pITN = TotalITN / (TotalITN+TotalUTN))

# bind the dataframes								 
Data_pref <- rbind(Data_pref_moiroux, Data_pref_strode)

# plot
supp_fig1 <- Data_pref %>%
	ggplot(aes(x=study, y=pITN)) +
		geom_boxplot() + 
	  geom_jitter()+
		ylab("Preference for insecticide-treated net (*p~ITN~*)") +
		theme(axis.title.y = element_markdown())

supp_fig1

ggsave(plot = supp_fig1, filename =  "suppFig1.pdf",  width = 13.2, height = 13.2*0.66, units = "cm")


# Supplementary Figure 2 plots Diversion, pre-bite and post-bite mortality value from Moiroux et al. 2017 ----
## justify baseline value (m1, m2, D) from moiroux 2017 :
Data_moiroux2 <- Data_moiroux %>%
	dplyr::mutate(m1 = Tot_D_unfd / (total-Tot_L_unfd)) %>% 	       # calculate pre-bite mortality
	dplyr::mutate(m2 = Tot_D_fed / Total_bfed) %>%  	      				# calculate post-bite mortality
	dplyr::mutate(D = Tot_L_unfd / total)     							# calculate Diversion rate


# prepare data for plot
plot_parameters <- Data_moiroux2 %>% 
	select(ttmt, ITN, m1, m2, D) %>%
	gather("m1","m2","D", key ="parameter", value = "value")

plot_param_UTN <- plot_parameters %>%
	filter(ITN == "no") %>%
	filter(parameter != "D")

plot_param_ITN <- plot_parameters %>%
	filter(ITN == "ITN")

#plot
fig_param_A <- ggplot(plot_param_UTN, aes(x=parameter, y=value, fill=parameter)) +
	geom_boxplot(alpha=0.4) +
	geom_jitter()+
	stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
	theme(legend.position="none") +
	scale_fill_brewer(palette="Set3") +
	labs(y=" ")+
	ylim(-0.001,1.001)+
	scale_x_discrete(labels=c(expression(paste('µ'['1U'])), expression(paste('µ'['2U']))))+ 
	theme(axis.title.x = element_blank())

fig_param_B <- ggplot(plot_param_ITN, aes(x=parameter, y=value, fill=parameter)) +
	geom_boxplot(alpha=0.4)  +
	geom_jitter()+
	stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red") +
	theme(legend.position="none") +
	scale_fill_brewer(palette="Set3") +
	labs(y=" ")+
	ylim(-0.001,1.001)+
	scale_x_discrete(labels=c(expression(paste('D'['P'])), expression(paste('µ'['1P'])), expression(paste('µ'['2P']))))+
	theme(axis.title.x = element_blank())

supp_fig2 <- ggarrange(fig_param_A, 
											 fig_param_B, 
											 labels = c("A", "B"),
											 widths = c(2/5,3/5),
											 ncol = 2, nrow = 1)

annotate_figure(supp_fig2,
								bottom = text_grob("Parameters"),
								left = text_grob("value", rot=90)#,
								#fig.lab = "Figure 1", fig.lab.face = "bold"
)

ggsave(plot = supp_fig2, filename =  "suppFig2.pdf",  width = 13.2, height = 13.2*0.66, units = "cm")

