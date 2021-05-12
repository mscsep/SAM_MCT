#' ---	
#' title: "Analyses of Memory Contextualization Task (MCT): pHit, pFA, dPrime & dPrime Contextualization Index"
#' author: "Milou Sep"	
#' output:	
#'   html_document: default	
#' ---	

# Info on script layout to compile report: http://brooksandrew.github.io/simpleblog/articles/render-reports-directly-from-R-scripts/
rm(list=ls()) # clean environment
library(RCurl) # to source from github
library(ggplot2)
library(Rmisc) # for CIs

#' 
#+ Load_data_&_functions, include=FALSE
source('R/Load_MCT_Data.R') # Load & Restructure MCT data for analysis
source('R/MCT_LMM_Analyses_Source.R') # source code to perform LMER analyses
#+ source codes from github
script.lmm.assumptions <- getURL("https://raw.githubusercontent.com/mscsep/SAM_sAA_Cortisol/master/R/LMM_Assumptions.R", ssl.verifypeer = FALSE)
eval(parse(text = script.lmm.assumptions))
script.lmm.to.word <- getURL("https://raw.githubusercontent.com/mscsep/SAM_sAA_Cortisol/master/R/LMM_Results_to_Word.R", ssl.verifypeer = FALSE)
eval(parse(text = script.lmm.to.word))

#' descriptives
summary(data_MCT)

#' ## **Memory Sensitivity Index (dPrime)**   
# Memory Sensitivity Index (dPrime) ---------------------------------------

#' ### **dPrime**   
#+ dPrime_analysis, include=F
# dPrime ------------------------------------------------------------------
LMER_assumptions(dependent_var='dPrime', check.data=data_long.dprime, logtransform = F)
# Inspect influential point. id=96 [96  0.0383109058]
# for description of influential point
data_MCT[(data_MCT$id == 096),]
# Perform analysis without subject (=id96)
dPrime.Analysis <- MCT_Analysis.w.Emotion.Context.b.Condition(dependent.var = 'dPrime', dataset=subset(data_long.dprime,!id %in% c(96,196,296,396)))
#' #### **Results dPrime LMER Model Comparisons** 
Export.Chi2.Table(Chi2Table=dPrime.Analysis$ChiSqTable, TableName = "results/dPrime.Chi2", file.export=F)
#' #### **Results dPrime LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = dPrime.Analysis$PostHoc.C.S.V$PostHoc.Table, TableName = "results/dPrime.PH.Context", file.export = F)
Export.PostHoc.Table(PostHocTable = dPrime.Analysis$PostHoc.S.C.V$PostHoc.Table, TableName = "results/dPrime.PH.Stress", file.export = F)
Export.PostHoc.Table(PostHocTable = dPrime.Analysis$PostHoc.V.S.C$PostHoc.Table, TableName = "results/dPrime.PH.Valence", file.export = F)
#' plot results
plot(dPrime.Analysis$PostHoc.S.C.V$PostHoc.Tests, main="post-hoc tests", ylab='dPrime', by=c("Condition", "emotion"),
     comparisons=T, horizontal=F,  alpha=0.05) # comparisons gives red errors for the tukey comparisons, use these to determine differences, not the purple confidence intervals


#' ### **dPrime Contextualization Index**  
#+ dPrime_contextualization_analysis, include=F 
# dPrime Contextualization Index ------------------------------------------
LMER_assumptions(dependent_var='dprime_contextualization', check.data=data_long.dprime_diff, logtransform = F)
dPrime.contextualization.Analysis <- MCT_Analysis.w.Emotion.b.Condition(dependent.var = 'dprime_contextualization', dataset=subset(data_long.dprime_diff,!id %in% c(96,196,296,396)))
#' #### **Results dPrime Contextualization Index LMER Model Comparisons**  
Export.Chi2.Table(Chi2Table=dPrime.contextualization.Analysis$ChiSqTable, TableName = "results/dPrime.Contextualization.Chi2", file.export=F)
#' #### **Results dPrime Contextualization Index LMER PostHoc Tests** 
#' Posthoc pairwise comparisons are performed on the levels of factor "Stress", for each Valence category. And on the levels of the factor "Valence", for each experimental condition (Stress).
Export.PostHoc.Table(PostHocTable = dPrime.contextualization.Analysis$PostHoc.SxV$PostHoc.Table, TableName = "results/dPrime.contextualization.PH.Stresst", file.export = F) # Table
dPrime.contextualization.Analysis$PostHoc.SxV$PostHoc.Plot # Figure
Export.PostHoc.Table(PostHocTable = dPrime.contextualization.Analysis$PostHoc.VxS$PostHoc.Table, TableName = "results/dPrime.contextualization.PH.Valence", file.export = F) # Table
dPrime.contextualization.Analysis$PostHoc.VxS$PostHoc.Plot # Figure


#' ## **Signal Detection Measures (incl pHit, pFA)**   
# Signal Detection Measures (pHit, pFA, pMiss, pCR) -----------------------
#'
#' ### **pHit**   
#+ pHit_analysis, include=F
# pHit --------------------------------------------------------------------
LMER_assumptions(dependent_var='pHit', check.data=data_long.pHit, logtransform = F)
pHit.Analysis <- MCT_Analysis.w.Emotion.Context.b.Condition(dependent.var = 'pHit', dataset=subset(data_long.pHit,!id %in% c(96,196,296,396)))
#' #### **Results pHit LMER Model Comparisons**  
Export.Chi2.Table(Chi2Table=pHit.Analysis$ChiSqTable, TableName = "results/pHit.Chi2", file.export=F)
#' #### **Results pHit LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = pHit.Analysis$PostHoc.C.S.V$PostHoc.Table, TableName = "results/pHit.PH.Context", file.export = F) # Table
Export.PostHoc.Table(PostHocTable = pHit.Analysis$PostHoc.S.C.V$PostHoc.Table, TableName = "results/pHit.PH.Stress", file.export = F) # Table
Export.PostHoc.Table(PostHocTable = pHit.Analysis$PostHoc.V.S.C$PostHoc.Table, TableName = "results/pHit.PH.Valence", file.export = F) # Table
pHit.Analysis$PostHoc.V$PostHoc.Plot # Figure


#' ### **pHit Contextualization Index**  
#+ pHit_contextualization_analysis, include=F
# pHit Contextualization index --------------------------------------------
LMER_assumptions(dependent_var='pHit.ctx', check.data=data_long.pHit.ctx, logtransform = F)
pHit.contextualization.Analysis <- MCT_Analysis.w.Emotion.b.Condition(dependent.var = 'pHit.ctx', dataset=subset(data_long.pHit.ctx,!id %in% c(96,196,296,396)))
#' #### **Results pHit Contextualization Index LMER Model Comparisons**  
Export.Chi2.Table(Chi2Table = pHit.contextualization.Analysis$ChiSqTable, TableName = "results/pHit.contextualization.Chi2", file.export = F)
#' #### **Results pHit Contextualization Index LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = pHit.contextualization.Analysis$PostHoc.VxS$PostHoc.Table, TableName = "results/pHit.contextualization.PH.Valence", file.export = F) # Table
Export.PostHoc.Table(PostHocTable = pHit.contextualization.Analysis$PostHoc.SxV$PostHoc.Table, TableName = "results/pHit.contextualization.PH.Stress", file.export = F) # Table


#' ### **pFA**   
#+ pFA_analysis, include=F
# pFA ---------------------------------------------------------------------
LMER_assumptions(dependent_var='pFA', check.data=data_long.pFA, logtransform = F)
pFA.Analysis <- MCT_Analysis.w.Emotion.b.Condition(dependent.var = 'pFA', dataset=subset(data_long.pFA,!id %in% c(96,196,296,396)))
#' #### **Results pFA LMER Model Comparisons**  
Export.Chi2.Table(Chi2Table = pFA.Analysis$ChiSqTable, TableName = "results/pFA.Chi2", file.export = F)
#' #### **Results pFA LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = pFA.Analysis$PostHoc.V$PostHoc.Table, TableName = "results/pFA.PH.Valence", file.export = F) # Table
pFA.Analysis$PostHoc.V$PostHoc.Plot # Figure


#' ## **Response Bias (Criterion C) (unpublished)**
#' 
#' Criterion is a measure for response bias (i.e. the general tendency to respond yes (old face) or no (new face)). 
#' c is defined as the distance between the criterion and the neutral point, where neither response is favored. 
#' Negative values of c signify a bias toward responding yes (=old face in MCT) (the criterion lies to the left of the neutral point), 
#' whereas positive values signify a bias toward the no response (=new face in MCT) (the criterion lies to the right of the neutral point). 
#' Thus, a liberal criterion biases the subject toward responding yes, regardless of the stimulus. By contrast, a high, or conservative, value 
#' for the criterion biases the subject toward responding no, because the criterion will rarely be exceeded on signal or noise trials. (Stanislaw,1999).  
#+ c_analysis, include=F
# C -----------------------------------------------------------------------
LMER_assumptions(dependent_var='C', check.data=data_long.C, logtransform = F)
C.Analysis <- MCT_Analysis.w.Emotion.Context.b.Condition(dependent.var = 'C', dataset=subset(data_long.C,!id %in% c(96,196,296,396)))
#' #### **Results C LMER Model Comparisons**  
Export.Chi2.Table(Chi2Table = C.Analysis$ChiSqTable, TableName = "results/response_bias_c", file.export = F)
#'  
#' #### **Results C LMER PostHoc Tests**  
Export.PostHoc.Table(PostHocTable = C.Analysis$PostHoc.C.S.V$PostHoc.Table, TableName = "results/c.PH.Context", file.export = F)
Export.PostHoc.Table(PostHocTable = C.Analysis$PostHoc.S.C.V$PostHoc.Table, TableName = "results/c.PH.Stress", file.export = F)
Export.PostHoc.Table(PostHocTable = C.Analysis$PostHoc.V.S.C$PostHoc.Table, TableName = "results/c.PH.Valence", file.export = F)


#' ### **Sensitivity MCT analyses with certainty scores (NOTE, quick check rather than complete analysis)**
# sensitivity certainty scores --------------------------------------------

#' *dPrime*
FullModel.d <-lmer(dPrime~1+Condition*certainty*emotion*context+(1|id)+(1|certainty:id),data = dPrime_CER_long, REML=F) # NB inclusion / exclusion subject 96 doesn't change results here [ dataset=subset(dPrime_CER_long,!id %in% c("96")))]
NOinteractions.d <-lmer(dPrime~1+Condition+certainty+emotion+context+(1|id)+(1|certainty:id),data = dPrime_CER_long, REML=F) 
mcp.fnc(FullModel.d)  # **Model Criticism Plots:** # Quick LMER assumption check -----> OK
anova(FullModel.d, NOinteractions.d) # ---> no interactions

# boxplot
qplot(x=certainty, y=dPrime, colour=Condition, data=dPrime_CER_long, geom="boxplot" )
# barplot
Mean.CI.d <- group.CI(dPrime~certainty+Condition, dPrime_CER_long, ci = 0.95)
ggplot(data=Mean.CI.d, aes(x=certainty, y=dPrime.mean, fill=Condition)) +
  geom_bar(stat="identity", position = position_dodge(.9), alpha=.8) +
  geom_errorbar(aes(ymin=dPrime.lower, ymax=dPrime.upper), width=.2, position=position_dodge(.9))
