## Results concerning 'pertubed learning distribution' case
cols <- c("dodgerblue1", "darkorchid1", "darkorange1", "chartreuse1")

axis.ts <- 36

small.size <- 30
large.size <- 36

PRR1 <- EVAL %>% 
  filter(n.train==200, n == 200, P==50, X.corr==0.0,
         beta.mean %in% c(4, 6), pertTF, delta==0, div < 2.5) %>%
  ggplot() + 
  geom_hline(yintercept = 0.05, col="red", lty=2, cex=2) +
  geom_point(aes(div, RR, color=(selection), pch=selection), cex=7) + 
  scale_y_continuous(limits=c(0,0.1), breaks=seq(0,0.1, 0.025)) +
  labs(x = bquote(KL(D, tilde(D))), 
       y=bquote(pi(0)== P(varphi[m*"**"]==1~"|" ~ delta==0)))+ 
  scale_shape_manual(guide = FALSE, values=c(15,18,16))+
  scale_linetype_manual(guide = FALSE, values=c(1,1,1))+ # 1,1,2,2
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1),
                                                   shape=c(18,16,15)))) + 
  scale_color_manual("Selection rule", 
                     breaks=c("default", "c2o",  "all"),
                     labels = c("default", "within 1 SE", "no selection"),
                     values=cols[c(1,3,4)]) +
  theme_grey()+
  theme(axis.text=element_text(size=small.size),
        axis.title.y=element_text(size=axis.ts,face="bold"),
        axis.title.x=element_text(size=axis.ts,face="bold"),
        legend.text = element_text(size=large.size),
        legend.title = element_text(size=large.size, face="bold"),
        strip.text.x = element_text(size=large.size, face="bold"),
        strip.text.y = element_text(size=large.size, face="bold", angle=0),
        legend.position = "none",
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"))

PRR2 <- EVAL %>% 
  filter(n.train==200, n == 200, P==50, X.corr==0.0,
         beta.mean %in% c(4, 6), pertTF, delta==0.05, div < 2.5) %>%
  ggplot() + 
  geom_point(aes(div, RR, color=(selection), pch=selection), cex=7) + 
  scale_y_continuous(limits=c(0.2,0.8), breaks=seq(0,1,0.1)) +
  labs(x = bquote(KL(D, tilde(D))), 
       y=bquote(pi(.05)== P(varphi[m*"**"]==1~"|"~ delta==.05)))+
  scale_shape_manual(guide = FALSE, values=c(15,18,16))+
  scale_linetype_manual(guide = FALSE, values=c(1,1,1))+ # 1,1,2,2
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1),
                                                   shape=c(18,16,15)))) + 
  scale_color_manual("Selection rule", 
                     breaks=c("default", "c2o",  "all"),
                     labels = c("default", "within 1 SE", "no selection"),
                     values=cols[c(1,3,4)]) +
  theme_grey()+
  theme(axis.text=element_text(size=small.size),
        axis.title.y=element_text(size=axis.ts,face="bold"),
        axis.title.x=element_text(size=axis.ts,face="bold"),
        legend.text = element_text(size=large.size),
        legend.title = element_text(size=large.size, face="bold"),
        strip.text.x = element_text(size=large.size, face="bold"),
        strip.text.y = element_text(size=large.size, face="bold", angle=0),
        legend.position = "none",
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"))


PTT <- RAW %>% 
  filter(selection2 %in% c("default", "c2o", "all"), P==50,
         n.train %in% c(200, 400),
         X.corr==0, beta.mean %in% c(4), div < 2.5, pertTF) %>%
  ggplot(aes(selection2, (theta)/theta_opt, color=selection2)) + 
  geom_hline(yintercept = 1, col="black", lty=1, cex=1) +
  geom_boxplot(cex=1.1) +
  theme_grey()+
  theme(axis.text=element_text(size=small.size),
        axis.title.y=element_text(size=4/3*large.size),
        legend.text = element_text(size=4/3*large.size),
        legend.title = element_text(size=4/3*large.size, face="bold"),
        strip.text = element_text(size=large.size/3*2),
        #legend.position = c(0.1,0.2),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'))+
  facet_grid( ~ div, labeller = label_bquote(cols=KL==.(div))) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=7, col="red") +
  #labs(y = bquote(bold(vartheta[m*"**"]/vartheta[opt])))+
  labs(y = bquote(vartheta[m*"**"]/vartheta[opt]))+
  scale_color_manual("Selection rule: ", 
                     breaks=c("default", "c2o", "all"),
                     labels = c("default  ", "within 1 SE  ", "no selection  "),
                     values=cols[c(3, 4, 1)]) +
  scale_y_continuous(limits=c(0.8, 1))

w = 0.02
h = 0.02


# SAVE FILE:
#png(filename = "PERT.png", width = 1500, height = 1200)
jpeg("figure6.jpg", width=7500, height= 6000, quality=95, res=360, pointsize=large.size)
ggdraw(ylim=c(0,0.9)) +
  draw_plot(PRR1, x = w,     y = 0.4, width = 0.5-w, height = 0.5-h) +
  draw_plot(PRR2, x = 0.5+w, y = 0.4, width = 0.5-w, height = 0.5-h) +
  draw_plot(PTT,  x = w,     y = 0,   width = 1-2*w, height = 0.4-h) +
  draw_plot_label(label = c("(a)", "(b)", "(c)"), size = large.size,
                  x = c(0, 0.5, 0), 
                  y = c(0.9, 0.9, 0.4),
                  fontface = "bold",
                  colour="black")
dev.off()

