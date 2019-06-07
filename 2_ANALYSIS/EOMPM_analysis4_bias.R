## Results concerning (relative) bias of naive and corrected point estimates

# TRANSFORM DATA:
RAW3 <- gather(RAW, est.type, RD, RD_naive, RD_alt, factor_key=TRUE)
RAW3 <- RAW3 %>%
  mutate(type.str = revalue(est.type, c("RD_alt" = "corrected estimate",
                                         "RD_naive" = "naive estimate")))

# 1500 x 750:
small.size <- 30
large.size <- 36

jpeg("figure4.jpg", width=7500, height= 4000, quality=95, res=360, pointsize=large.size)

RAW3 %>% 
  filter(selection2 %in% c("default", "c2o", "all"), n.train==200,
         n %in% c(100, 200, 400), P==50, X.corr==0.0,
         beta.mean %in% c(6), pert=="0.0.0.0", !pertTF, cv.folds==10) %>%
  ggplot(aes(selection2, RD, color=selection2)) + 
  geom_hline(yintercept = 0, col="black", lty=1, cex=1) +
  geom_boxplot(cex=1.1) +
  labs(y=bquote(bold("Bias: ")*(hat(vartheta)[m ~"**"]^(c) -
                                  vartheta[m ~"**"])/vartheta[m ~"**"])) +
  scale_color_manual("Selection rule: ", 
                     breaks=c("default", "c2o", "all"),
                     labels = c("default ", "within 1 SE ", "no selection "),
                     values=cols[c(3, 4, 1)]) +
  theme_gray()+
  theme(axis.text=element_text(size=small.size),
        axis.title.y=element_text(size=large.size,face="bold"),
        legend.text = element_text(size=large.size),
        legend.title = element_text(size=large.size, face="bold"),
        strip.text.x = element_text(size=large.size, face="bold"),
        strip.text.y = element_text(size=large.size, face="bold", angle=90),
        #legend.position = c(0.875,0.5), #c(0.875,0.5)
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'))+
  facet_grid(type.str ~ n, labeller =
               label_bquote(rows= bold(.(as.character(type.str))),
                            cols=bold(n==.(n)))) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=7, col="red") + 
  scale_y_continuous(limits=c(-0.15, 0.15))
dev.off()
