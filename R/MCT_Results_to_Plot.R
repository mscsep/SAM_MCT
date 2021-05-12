#' ---	
#' title: "Visualize dPrime & dPrime contextualization data"
#' author: "Milou Sep"	
#' output:	
#'   html_document: default	
#' ---	
rm(list=ls())
source('R/Load_MCT_Data.R') # Load & Restructure MCT data for analysis

# Load packages
library(ggplot2)
library(ggpubr)
library(Rmisc) # needed to calculate means & CI's
library(ggsignif) # to add significance marks to plots

# Calculate M & 95% CI's --------------------------------------------------
dataset_dprime=subset(data_long.dprime,!id %in% c(96,196,296,396))
dataset_dprime_diff=dataset=subset(data_long.dprime_diff,!id %in% c(96,196,296,396))

# NB CIs are not calculated via ggplot, as ggplot did not calculate CI's per variable level (and therefor error-bars were not calculated correctly by ggplot).
#https://www.rdocumentation.org/packages/Rmisc/versions/1.5/topics/group.CI
dPrime.Mean.CI <- group.CI(dPrime~variable+Condition, dataset_dprime, ci = 0.95)
dPrime_diff.Mean.CI <- group.CI(dprime_contextualization~variable+Condition, dataset_dprime_diff, ci = 0.95)


# Plot dPrime data --------------------------------------------------------

dPrime.Plot<-
  ggplot(data=dPrime.Mean.CI, aes(variable, dPrime.mean)) + 
  theme_classic() +
  ylab('d-prime sensitivity \n (mean+/-95%CI)') + xlab("") +   # axis labels & title
  scale_x_discrete(limits=c("IN_dPrime","RN_dPrime","IA_dPrime","RA_dPrime"), # Change order x-axis
                   labels=c("Neutral\nCongruent ", "Neutral\nIncongruent ", "Negative\nCongruent ", "Negative\nIncongruent ")) +
  scale_fill_manual( # Colors: http://sape.inf.usi.ch/quick-reference/ggplot2/colour
    values= c(Delayed = "navyblue", Direct = "red3", Control = "black"),
    labels =c(Delayed = "delayed-stress",
              Direct = "immediate-stress",
              Control = "no-stress")) +
  # adjust fontsize legend
  theme(legend.title=element_text(size=12), legend.text=element_text(size=9))+
  # Add mean & CI's
  geom_bar(aes(fill = Condition), stat="identity", position = position_dodge(0.9), alpha=.8) + # NB fill and group must be added here, otherwise 'geom-signif' does not work
  geom_errorbar(aes(ymin=dPrime.lower, ymax=dPrime.upper, group = Condition), width=.2, position=position_dodge(.9)) +
  
  # Add asterisk and lines to note significance:
  #info: https://stackoverflow.com/questions/17084566/put-stars-on-ggplot-barplots-and-boxplots-to-indicate-the-level-of-significanc
  
  # Results of the dPrime followup analyses:
  # - delayed vs control in (congruent neutral) p = .016 --> Corresponds to x=0.7 tot 1.3 (when dodge is 0.9)
  # - congruent - incongruent in (delayed neutral) p < .001 --> Corresponds to x=0.7 tot 1.7
  # - negative vs neutral in (delayed incongruent) p = .05 --> Corresponds to x= 1.7 tot 3.7
  geom_signif(stat="identity",
              data=data.frame(x=c(0.7, 0.7, 1.7), xend=c(1.3, 1.7, 3.7),
                              y=c(1.2, 1.3, 1.1), annotation=c("*", "* ", "#")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))
dPrime.Plot


# Plot dPrime Contextualization Index -----------------------------------

dPrime.DIFF.Plot<-
  ggplot(data=dPrime_diff.Mean.CI, aes(x=variable, y=dprime_contextualization.mean)) +
  theme_classic() +
  ylab('d-prime contextualization \n (mean+/-95%CI') + xlab("") +
  scale_x_discrete(limits=c("N_dPrime_diff","A_dPrime_diff"),
                   labels=c("Neutral", "Negative")) +
  scale_fill_manual(
    values= c( Delayed = "navyblue", Direct = "red3", Control = "black"),
    labels =c(Delayed = "delayed-stress",
              Direct = "immediate-stress",
              Control = "no-stress")) +
  # adjust fontsize legend
  theme(legend.title=element_text(size=12), legend.text=element_text(size=9))+
  # Add mean & CI's
  geom_bar(aes(fill=Condition ), stat="identity", position = position_dodge(.9), alpha=.8) +
  geom_errorbar(aes(ymin=dprime_contextualization.lower, ymax=dprime_contextualization.upper, group=Condition), width=.2, position=position_dodge(.9)) +
  
  # Results of the dPrime contextualization followup analyses:
  # Delayed - direct neutral p = .015 --> Corresponds to x=0.7 tot 1.3 (when dodge is 0.9)
  # negative vs neutral delayed condition p = .002 --> Corresponds to x=0.7, tot 1.7
  geom_signif(stat="identity",
              data=data.frame(x=c(0.7, 0.7), xend=c(1, 1.7),
                              y=c(0.36, 0.4), annotation=c("*", "* ")),
              aes(x=x,xend=xend, y=y, yend=y, annotation=annotation))

dPrime.DIFF.Plot


# Merge plots in one figure -----------------------------------------------
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/#annotate-the-arranged-figure
MCT.plots<- ggarrange(dPrime.Plot  
                      + font("y.text", size = 9)
                      + font("ylab", size = 12)
                      + font("x.text", size = 9)
                      + font("xlab", size = 12),
                      dPrime.DIFF.Plot 
                      + font("y.text", size = 9)
                      + font("ylab", size = 12)
                      + font("x.text", size = 9)
                      + font("xlab", size = 12), 
                      labels = c("A", "B"),
                      ncol = 1, nrow = 2,
                      font.label = list(size=12, face = "bold"),                 
                      align = "v",
                      legend=c("top"),
                      common.legend = T)
MCT.plots

# more info on annotate see: https://www.rdocumentation.org/packages/ggpubr/versions/0.2/topics/annotate_figure
annotate_figure(MCT.plots, 
                fig.lab=c("Figure 3"), 
                fig.lab.face = "bold",
                fig.lab.size = 12)

# Save figure to jpeg ------------------------------------------------------
# info on set resolution (dpi) https://ggplot2.tidyverse.org/reference/ggsave.html
ggsave("results/Figure3.MCT.dPrime.Plots.colour.v3.5.19.v2.pdf", device="pdf", dpi = 500, height = 7, width = 6, limitsize = T )



# Plot pHit (unpublished, for visualization / exploration purposes) -------
dataset_hit=subset(data_long.pHit,!id %in% c(96,196,296,396))
pHit.Mean.CI <- group.CI(pHit~variable+Condition, dataset_hit, ci = 0.95)

pHit.Plot<-
  ggplot(data=pHit.Mean.CI, aes(variable, pHit.mean)) + 
  theme_classic() +
  ylab('Hit Rate (mean+/-95%CI)') + xlab("") +
  scale_x_discrete(limits=c("IN_hit_Rate","RN_hit_Rate","IA_hit_Rate","RA_hit_Rate"),
                   labels=c("Neutral\nCongruent ", "Neutral\nIncongruent ", "Negative\nCongruent ", "Negative\nIncongruent ")) +
  scale_fill_manual(
    values= c(Delayed = "navyblue", Direct = "red3", Control = "black"),
    labels =c(Delayed = "delayed-stress",
              Direct = "immediate-stress",
              Control = "no-stress")) +
  # adjust fontsize legend
  theme(legend.title=element_text(size=12), legend.text=element_text(size=9))+
  # Add mean & CI's
  geom_bar(aes(fill = Condition), stat="identity", position = position_dodge(0.9), alpha=.8) +
  geom_errorbar(aes(ymin=pHit.lower, ymax=pHit.upper, group = Condition), width=.2, position=position_dodge(.9)) 

pHit.Plot
