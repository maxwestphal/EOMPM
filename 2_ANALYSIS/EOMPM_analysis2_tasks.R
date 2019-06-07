# Preparations for labels:
anno <- data.frame(dist.class=c("Coefficient model 1 (sparse)",
                                "Coefficient model 2 (dense)"), 
                   n.train=200,
                   label=c("A", "B"),
                   x=3, y=0.9875)

bayes <- data.table(dist.class=c(rep("Coefficient model 1 (sparse)",4),
                                 rep("Coefficient model 2 (dense)", 4)),
                    x=1:4, 
                    bp=BP)

##  Prediction tasks:
#   (1500 x 500)
small.size <- 30
large.size <- 36

jpeg("figure2.jpg", width=7500, height= 2500, quality=95, res=360, pointsize=large.size)
TT %>% 
  filter(n.train==200)  %>%
  ggplot() +
  geom_boxplot(aes(scenario, theta_q50, colour="c2"), cex=1, position="dodge") +
  geom_boxplot(aes(scenario, theta_q100, colour="c1"), cex=1) +
  geom_point(data=bayes, aes(x, bp), col="blue", pch=18, cex=7) +
  theme_gray() + 
  theme(axis.text.y=element_text(size=small.size),
        axis.title.y=element_blank(),
        axis.text.x=element_text(face="bold", color="black", 
                                 size=small.size, angle=330, hjust=0),
        axis.title.x=element_blank(),
        legend.title = element_text(size=large.size, face="bold"),
        legend.text = element_text(size=large.size),
        strip.text.x = element_text(size=large.size, face = "bold")
  ) +
  scale_color_manual("Candidate \nmodel \nperformance", 
                     labels=c("optimal", "median"),
                     values=c("coral", "aquamarine")) +
  scale_y_continuous(limits = c(0.5, 1),
                     sec.axis = dup_axis()) +
  geom_label(data=anno, aes(x=x, y=y), size=10, fontface="bold", label=c("A", "B"), col="black") + 
  scale_x_discrete(labels=c("0.2" =   bquote(mu[1]==2*"," ~ rho==0), #"S1:" ~ 
                          "0.5.2" = bquote(mu[1]==2*"," ~ rho==0.5),
                          "0.4" =   bquote(mu[1]==4*"," ~ rho==0),
                          "0.5.4" = bquote(mu[1]==4*"," ~ rho==0.5),
                          "0.3" =   bquote(mu[2]==3*"," ~ rho==0),
                          "0.5.3" = bquote(mu[2]==3*"," ~ rho==0.5),
                          "0.6" =   bquote(mu[2]==6*"," ~ rho==0),
                          "0.5.6" = bquote(mu[2]==6*"," ~ rho==0.5)),
                 drop=T) +
  facet_grid(~ dist.class, scales="free_x")
dev.off()







