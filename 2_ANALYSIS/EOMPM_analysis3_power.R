## REJECTION RATE / POWER (CORRECT/FALSE FOR m**)

cols <- c("dodgerblue1", "darkorchid1", "darkorange1", "chartreuse1")

##  Overll rejection rate for final model:
#   1500 x 1000:
small.size <- 30
large.size <- 36

jpeg("figure3.jpg", width=7500, height= 5000, quality=95, res=360, pointsize=large.size)

EVAL %>% 
  filter(selection != "quantile", n.train==200, n %in% c(100, 200, 400), P==50,
         pert=="0.0.0.0",
         X.corr==0.0, beta.mean %in% c(4, 6), !pertTF, cv.folds==10) %>%
  ggplot() + 
  geom_hline(yintercept = 0.05, col="red", lty=2, cex=1.5) +
  geom_vline(xintercept = 0, col="black", lty=2, cex=1.5) +
  geom_line(aes(delta, RR, color=(selection), lty=selection), cex=1.5) + 
  geom_point(aes(delta, RR, color=(selection), pch=selection), cex=6) + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  scale_x_continuous(limits=c(-0.055, 0.105), breaks=c(-0.05, 0, 0.05, 0.1), labels=c("-0.05", "0", "0.05", "0.1")) +
  labs(x = bquote(delta == vartheta[opt] - vartheta[0]), 
       y=bquote(bold("Rejection rate: ")*
                  pi(delta)==P(varphi[m*"**"]==1~"|"~delta))) +
  scale_shape_manual(guide = FALSE, values=c(15,17,18,16))+
  scale_linetype_manual(guide = FALSE, values=c(1,1,1,1))+ # 1,1,2,2
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1,1), 
                                                   shape=c(17,18,16,15)))) +
  scale_color_manual("Selection rule: ", 
                     breaks=c("oracle", "default", "c2o",  "all"),
                     labels = c("oracle ","default ","within 1 SE ","no selection "),
                     values=cols) +
  theme_grey()+
  theme(axis.text.y=element_text(size=small.size),
        axis.text.x=element_text(size=small.size, angle=0),
        axis.title.y=element_text(size=large.size,face="bold"),
        axis.title.x=element_text(size=large.size,face="bold", vjust=-0.5),
        legend.text = element_text(size=large.size),
        legend.title = element_text(size=large.size, face="bold"),
        strip.text.x = element_text(size=large.size, face="bold"),
        strip.text.y = element_text(size=large.size, face="bold", angle=0),
        #legend.position = c(0.09,0.84),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.75, 'char'))+
  facet_grid(beta.str ~ n, labeller =
               label_bquote(rows= bold(.(beta.str)), cols=bold(n==.(n))))

dev.off()



################################################################################
##  Incorrect rejections for final model:
#   1500 x 1000:
EVAL %>% 
  filter(selection != "quantile", n.train==200, n %in% c(100, 200, 400), P==50,
         pert=="0.0.0.0",
         X.corr==0.0, beta.mean %in% c(4, 6), !pertTF, cv.folds==10) %>%
  ggplot() + 
  geom_hline(yintercept = 0.05, col="red", lty=1, cex=0.75) +
  geom_vline(xintercept = 0, col="black", lty=1, cex=0.75) +
  geom_line(aes(delta, IRR, color=(selection), lty=selection), cex=0.75) + 
  geom_point(aes(delta, IRR, color=(selection), pch=selection), cex=3) + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  labs(x = bquote(delta == vartheta[opt] - vartheta[0]), 
       y=bquote(bold("Incorrect rejections: ")*
                  pi(delta)==P(varphi[m*"**"]==1~"|"~H[0]^"m**"*","~delta))) +
  scale_shape_manual(guide = FALSE, values=c(15,17,18,16))+
  scale_linetype_manual(guide = FALSE, values=c(1,1,1,1))+ # 1,1,2,2
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1,1), 
                                                   shape=c(17,18,16,15)))) +
  scale_color_manual("Selection rule", 
                     breaks=c("oracle", "default", "c2o",  "all"),
                     labels = c("oracle","default","within 1 SE","no selection"),
                     values=cols) +
  theme_grey()+
  theme(axis.text=element_text(size=18),
        axis.title.y=element_text(size=24,face="bold"),
        axis.title.x=element_text(size=24,face="bold"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24, face="bold"),
        strip.text.x = element_text(size=24, face="bold"),
        strip.text.y = element_text(size=24, face="bold", angle=0),
        legend.position = c(0.1,0.9),
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"))+
  facet_grid(beta.str ~ n, labeller =
               label_bquote(rows= bold(.(beta.str)), cols=bold(n==.(n))))


################################################################################
##  Correct rejections for final model:
#   1500 x 1000:
EVAL %>% 
  filter(selection != "quantile", n.train==200, n %in% c(100, 200, 400), P==50,
         pert=="0.0.0.0",
         X.corr==0.0, beta.mean %in% c(4, 6), !pertTF, cv.folds==10) %>%
  ggplot() + 
  geom_hline(yintercept = 0.05, col="red", lty=1, cex=0.75) +
  geom_vline(xintercept = 0, col="black", lty=1, cex=0.75) +
  geom_line(aes(delta, CRR, color=(selection), lty=selection), cex=0.75) + 
  geom_point(aes(delta, CRR, color=(selection), pch=selection), cex=3) + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  labs(x = bquote(delta == vartheta[opt] - vartheta[0]), 
       y=bquote(bold("Correct rejections: ")*
                  pi(delta)==P(varphi[m*"**"]==1~"|"~H[1]^"m**"*","~delta))) +
  scale_shape_manual(guide = FALSE, values=c(15,17,18,16))+
  scale_linetype_manual(guide = FALSE, values=c(1,1,1,1))+ # 1,1,2,2
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1,1), 
                                                   shape=c(17,18,16,15)))) +
  scale_color_manual("Selection rule", 
                     breaks=c("oracle", "default", "c2o",  "all"),
                     labels = c("oracle","default","within 1 SE","no selection"),
                     values=cols) +
  theme_grey()+
  theme(axis.text=element_text(size=18),
        axis.title.y=element_text(size=24,face="bold"),
        axis.title.x=element_text(size=24,face="bold"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24, face="bold"),
        strip.text.x = element_text(size=24, face="bold"),
        strip.text.y = element_text(size=24, face="bold", angle=0),
        legend.position = c(0.1,0.9),
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"))+
  facet_grid(beta.str ~ n, labeller =
               label_bquote(rows= bold(.(beta.str)), cols=bold(n==.(n))))


################################################################################
##  Disjuctive power (any correct rejection):
#   1500 x 1000:
EVAL %>% 
  filter(selection != "quantile", n.train==200, n %in% c(100, 200, 400), P==50,
         pert=="0.0.0.0",
         X.corr==0.0, beta.mean %in% c(4, 6), !pertTF, cv.folds==10) %>%
  ggplot() + 
  geom_hline(yintercept = 0.05, col="red", lty=1, cex=0.75) +
  geom_vline(xintercept = 0, col="black", lty=1, cex=0.75) +
  geom_line(aes(delta, POWER, color=(selection), lty=selection), cex=0.75) + 
  geom_point(aes(delta, POWER, color=(selection), pch=selection), cex=3) + 
  scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +
  labs(x = bquote(delta == vartheta[opt] - vartheta[0]), 
       y=bquote(bold("Disjunctive power"))) +
  scale_shape_manual(guide = FALSE, values=c(15,17,18,16))+
  scale_linetype_manual(guide = FALSE, values=c(1,1,1,1))+ # 1,1,2,2
  guides(colour = guide_legend(override.aes = list(linetype = c(1,1,1,1), 
                                                   shape=c(17,18,16,15)))) +
  scale_color_manual("Selection rule", 
                     breaks=c("oracle", "default", "c2o",  "all"),
                     labels = c("oracle","default","within 1 SE","no selection"),
                     values=cols) +
  theme_grey()+
  theme(axis.text=element_text(size=18),
        axis.title.y=element_text(size=24,face="bold"),
        axis.title.x=element_text(size=24,face="bold"),
        legend.text = element_text(size=24),
        legend.title = element_text(size=24, face="bold"),
        strip.text.x = element_text(size=24, face="bold"),
        strip.text.y = element_text(size=24, face="bold", angle=0),
        legend.position = c(0.1,0.9),
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"))+
  facet_grid(beta.str ~ n, labeller =
               label_bquote(rows= bold(.(beta.str)), cols=bold(n==.(n))))



################################################################################
##   Mstar summary for within 1 SE (=close2opt) rule
RAW %>% 
  filter(!pertTF, selection=="c2o") %>%
  group_by(beta.seed, n.train, beta.mean, X.corr) %>%
  summarize(N_sim=n(),
            Mstar = Mstar[1]) %>%
  group_by(n.train, beta.mean, X.corr) %>%
  summarise(N_sim=n(),
            Mstar.min = min(Mstar),
            Mstar.q25 = quantile(Mstar, 0.25),
            Mstar.mean = mean(Mstar),
            Mstar.median = median(Mstar),
            Mstar.q75 = quantile(Mstar, 0.75),
            Mstar.max = max(Mstar),
            Mstar.IQR = Mstar.q75 - Mstar.q25)

