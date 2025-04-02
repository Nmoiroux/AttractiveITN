# Effect of different types of ITNs (dterent, inert, attractives) against no nets

# according to use rate
use <- seq(0,1,by = 0.01)	  # ITN use to simulate
ref_use <- 0                # use rate (0%) as reference (no nets)

p<- c(0.36,0.5,0.7)		  # preference for ITN protected human (against unprotected human) 

RTP.fit <- expand.grid(p.=p,use.=use)   # create table of all combinations of pITN and use
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(use.,p.),~fRTP(p=.y, Uh=.x, Uh2=ref_use,FUN= FUN))) # calculates TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in TP

## legends titles and labels
legend_title <- "ITN: "
leg_label <- c("deterent (*p~ITN~* = 0.36)","inert (*p~ITN~* = 0.5)","attractive (*p~ITN~* = 0.7)")

fig_A <- ggplot(RTP.fit, aes(x=use.*100, y=redVC)) + 
  xlab("ITN use (%)") + 
  geom_line(aes(linetype=as.factor(p.))) +
  scale_linetype_manual(values=c(4,1,3),name=legend_title, labels=leg_label)+
  xlim(0,100) + ylim(-100,0)+
  theme(aspect.ratio=1) +
  theme(axis.title.y = element_blank())+
  theme(legend.text = element_markdown())

# Change in TP for use = 50%
fig_A$data %>% filter(use. == 0.5) 

# according to physiological resistance
m <- seq(0,1,by = 0.01)	  # pre-bite mortality

RTP.fit <- expand.grid(p.=p,m.=m)   # create table of all combinations of p and m
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(m.,p.),~fRTP(p=.y, m=.x, m2=.x, Uh2=ref_use,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in transmission potential

fig_B <- ggplot(RTP.fit, aes(x=(1-m.)*100, y=redVC)) + 
  xlab("feeding attempt survival (%)") + 
  geom_line(aes(linetype=as.factor(p.))) +
  scale_linetype_manual(values=c(4,1,3),name=legend_title, labels=leg_label)+
  xlim(0,100) + ylim(-100,0)+
  theme(aspect.ratio=1) +
  theme(axis.title.y = element_blank())+
  theme(legend.text = element_markdown())+
  geom_rect(aes(xmin = (1-m1p_rg$min)*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)

# Change in TP for m. = m1p_rg$min (max resistance)
fig_B$data %>% filter(m. == round(m1p_rg$min,2)) 


# according to quantitative behavioral resistance
d <- seq(0,1,by = 0.01)	  # diversion probability

RTP.fit <- expand.grid(p.=p,d.=d)   # create table of all combinations of p and d
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(d.,p.),~fRTP(p=.y, D=.x, Uh2=ref_use,FUN= FUN))) # calculate TP ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in transmission potential

fig_C <- ggplot(RTP.fit, aes(x=d.*100, y=redVC)) + 
  xlab("Diversion (%)") + 
  geom_line(aes(linetype=as.factor(p.))) +
  scale_linetype_manual(values=c(4,1,3),name=legend_title, labels=leg_label)+
  xlim(0,100) + ylim(-100,0)+
  theme(aspect.ratio=1) +
  theme(axis.title.y = element_blank())+
  theme(legend.text = element_markdown())+
  geom_rect(aes(xmin = Dp_rg$max*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)

# Change in TP for d. = Dp_rg$max (max resistance)
fig_C$data %>% filter(d. == round(Dp_rg$max,2)) 



# according to qualitative behavioral resistance
pi <- seq(0,1,by = 0.01)	  # proportion of exposure to bite during which ITN is in use

RTP.fit <- expand.grid(p.=p,pi.=pi)   # create table of all combinations of p and d
RTP.fit <- mutate(RTP.fit, z = pmap_dbl(list(pi.,p.),~fRTP(p=.y, pi=.x, pi2=.x, Uh2=ref_use,FUN= FUN))) # calculate VC ratio
RTP.fit$redVC <- -(1-RTP.fit$z)*100        # reduction in VC

fig_D <- ggplot(RTP.fit, aes(x=(1-pi.)*100, y=redVC)) + 
  xlab("Spatiotemporal avoidance (%)") + 
  geom_line(aes(linetype=as.factor(p.))) +
  scale_linetype_manual(values=c(4,1,3),name=legend_title, labels=leg_label)+
  xlim(0,100) + ylim(-100,0)+
  theme(aspect.ratio=1) +
  theme(axis.title.y = element_blank())+
  theme(legend.text = element_markdown())+
  geom_rect(aes(xmin = (1-pi_rg$min)*100, xmax = 100, ymin = -100, ymax = 0), fill="white", alpha = 0.008)

# Change in TP for pi. = (1-pi_rg$min) (max resistance)
fig_D$data %>% filter(pi. == round((1-pi_rg$min),2)) 


# arrange panels of figure
figure <- ggarrange(fig_A + rremove("legend"), 
                     fig_B + rremove("legend"),
                    fig_C + rremove("legend"),
                    fig_D + rremove("legend"),
                     common.legend = TRUE,
                     plot_legend = 2,
                     #legend = "right",
                     labels = c("A", "B", "C", "D"),
                     ncol = 2, nrow = 2)

figure <- annotate_figure(figure,
                           left = text_grob(y_title, rot=90))

figure

# save figure as pdf
width <- 16
ggsave(plot = figure, filename =  "suppFig3.pdf",  width = width, height = width, units = "cm")
