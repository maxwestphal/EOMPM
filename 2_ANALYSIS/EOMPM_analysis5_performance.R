## Results concerning true performance of final model: 

## Results: PERFORMANCE OF FINAL MODEL:
## 1500x750
small.size <- 30
large.size <- 36

jpeg("figure5.jpg", width=7500, height= 4000, quality=95, res=360, pointsize=large.size)

RAW %>% 
  filter(selection2 %in% c("default", "c2o", "all"), P==50,
         n.train %in% c(200, 400),
         X.corr==0, beta.mean %in% c(6), pert=="0.0.0.0", !pertTF, n < 500) %>%
  ggplot(aes(selection2, (theta)/theta_opt, color=selection2)) + 
  geom_hline(yintercept = 1, col="black", lty=1, cex=1) +
  geom_boxplot(cex=1.1) +
  theme_grey() +
  theme(axis.text=element_text(size=small.size),
        axis.title.y=element_text(size=large.size ,face="bold"),
        #axis.title.x=element_text(size=18,face="bold"),
        legend.text = element_text(size=large.size ),
        legend.title = element_text(size=large.size , face="bold"),
        strip.text.x = element_text(size=large.size , face="bold"),
        strip.text.y = element_text(size=large.size , face="bold", angle=90),
        #legend.position = c(0.875,0.1),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.background = element_rect(fill="white",
                                         size=0.75, linetype="solid", 
                                         colour ="black"),
        legend.direction = "horizontal", legend.position = "bottom", legend.box = "vertical",
        legend.spacing.x = unit(0.5, 'char'))+
  facet_grid(n.train ~ n, labeller = label_bquote(rows= bold(n[learn]==.(n.train)),
                                                  cols=bold(n==.(n)))) +
  stat_summary(fun.y = mean, geom = "point", pch=18, cex=7, col="red") +
  labs(y = bquote(bold("Relative performance: ")*vartheta[m*"**"]/vartheta[opt]))+
  scale_color_manual("Selection rule: ", 
                     breaks=c("default", "c2o", "all"),
                     labels = c("default ", "within 1 SE ", "no selection "),
                     values=cols[c(3, 4, 1)]) +
  scale_y_continuous(limits=c(0.9, 1))
dev.off()
