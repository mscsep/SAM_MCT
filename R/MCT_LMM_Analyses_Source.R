#'---
#'  title:  "Source script with functions for analyses of MCT task"
#'  author: "Milou Sep"
#'  output: html_document
#'---

#' ### **Load packages to library**
#+ Load_packages, include=FALSE
library(lme4) # for LMM's
library(LMERConvenienceFunctions)
library(lmerTest) # NB only after loading lmerTest, p-values for lmer interaction are displayed https://featuredcontent.psychonomic.org/putting-ps-into-lmer-mixed-model-regression-and-statistical-significance/
library(emmeans)

#' NB This script uses "get(dependent.var)" from: https://datascienceplus.com/how-to-create-a-loop-to-run-multiple-regression-models/

#' ### **Function for LMER posthoc testing**
PostHocTest.MCT <- function(dependent.var, FittedModel, selected_contrast, factor_levels){
  
  # Pairwise Comparisons ----------------------------------------------------
  PostHocResults <-emmeans::emmeans(FittedModel, #fitted model  
                                    eval(selected_contrast), #set contrasts
                                    adjust="tukey",#p-value adjustment for multiple comparisons family wise (note: Benjamini-Hochberg correction is not appropriate here because p-values are not independent http://www-stat.wharton.upenn.edu/~steele/Courses/956/Resource/MultipleComparision/Writght92.pdf)
                                    lmer.df = "kenward-roger")#use k-r method for computing denom df #Although it is computationally expensive, often times it is worth specifying the KR because it corrects for inflated Type I error rates.
  
  PostHocResults$emmeans # Estimated Means per group
  PostHocResults$contrasts # Statistical comparisons
  
  # Plot Results ------------------------------------------------------------
  PostHocPlot<-plot(PostHocResults, main="post-hoc tests", ylab=as.character(dependent.var), comparisons=T, horizontal=F)#comparisons gives red errors for the tukey comparisons, use these to determine differences, not the purple confidence intervals
  
  # Calculate effect size's -------------------------------------------------
  
  # for experimental condition (a factor with 3 levels)
  #  Effect sizes were calculated for the planned contrast analysis using Cohen's d which is calculated by dividing 2 times the t-value by the square root of the degrees of freedom 
  # (Rosenthal and Rosnow, 1991) for the comparisons between the different conditions. 
  if(factor_levels == 3){
    cohens.d<-(data.frame(PostHocResults$contrasts)$t.ratio*2)/ 
      sqrt(data.frame(PostHocResults$contrasts)$df)
  } 
  # for valence & context (factors with 2 levels)
  #  For the within factors valence and context, Cohen's d was calculated by dividing the t-value by the square root of the degrees of freedom (Rosenthal, 1991). 
  if(factor_levels == 2){
    cohens.d<-(data.frame(PostHocResults$contrasts)$t.ratio)/ 
      sqrt(data.frame(PostHocResults$contrasts)$df)
  }
  
  PostHoc.Table<-cbind(as.data.frame(PostHocResults$contrasts), cohens.d) # Bind cohens d's to table with post hoc Results
  
  # Create function output --------------------------------------------------
  Results <- list(PostHocResults, PostHoc.Table, PostHocPlot) 
  names(Results) <- c("PostHoc.Tests", "PostHoc.Table", "PostHoc.Plot")
  return( Results)
}

#' ### **Function suitable for MCT analyses with 2 within factors (Emotion & Condition) and between factor (Condition)**
#' NOTE,the optimizer for all models is set to "bobyqa" (for better model convergence):
#   allFit(show.meth.tab=TRUE) # Show all available optimizers, info from [https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html] and [http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/]
#   optimizers_main_stress<-allFit(Main_Stress) # Test which optimizers work ok
#   summary(optimizers_main_stress) # --> optimizer for all models set to "bobyqa"
#   isSingular(Main_Stress)
#   rePCA(Main_Stress)

MCT_Analysis.w.Emotion.Context.b.Condition <- function(dependent.var, dataset){
  # w(ithin factors): Emotion & Context; b(etween factor): Condition
  
  # LMER Models -------------------------------------------------------------
  
  # Full Model
  FullModel <-lmer(get(dependent.var)~1+Condition*context*emotion+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                   control=lmerControl(optimizer="bobyqa")) 
  
  # Test random effects.
  ### 3-way interaction [compared to Full Model] ###
  Full_NO_3way<-lmer(get(dependent.var)~1+Condition*context+emotion*context+Condition*emotion+(1|id)+(1|context:id)+(1|emotion:id),data = dataset,REML=F, 
                     control=lmerControl(optimizer="bobyqa")) 
  
  # 2 Way interactions [compared to Full Model without 3Way]
  # Define Models
  Full_NO_Stress.Valence<-lmer(get(dependent.var)~1+Condition*context+emotion*context+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                               control=lmerControl(optimizer="bobyqa"))
  Full_NO_Valence.Context<-lmer(get(dependent.var)~1+Condition*context+Condition*emotion+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                                control=lmerControl(optimizer="bobyqa"))
  Full_NO_Stress.Context<-lmer(get(dependent.var)~1+Condition*emotion+emotion*context+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                               control=lmerControl(optimizer="bobyqa"))
  # Compare Models
  Stress.Valence.Context <- anova(FullModel,Full_NO_3way) # Show's if there is a 3way interaction & the Log-Likelihood (en df) of the full model
  Stress.Valence <- anova(Full_NO_3way,Full_NO_Stress.Valence)
  Valence.Context <- anova(Full_NO_3way,Full_NO_Valence.Context)
  Stress.Context <- anova(Full_NO_3way,Full_NO_Stress.Context)
  
  ### Main effects ###
  # Define Models
  Main_NO_2ways<-lmer(get(dependent.var)~1+Condition+context+emotion+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                      control=lmerControl(optimizer="bobyqa"))
  Main_Stress<-lmer(get(dependent.var)~1+emotion+context+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                    control=lmerControl(optimizer="bobyqa"))
  Main_Valence<-lmer(get(dependent.var)~1+Condition+context+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                     control=lmerControl(optimizer="bobyqa"))
  Main_Context<-lmer(get(dependent.var)~1+Condition+emotion+(1|id)+(1|context:id)+(1|emotion:id),data = dataset, REML=F, 
                     control=lmerControl(optimizer="bobyqa"))
  # Compare Models
  Stress <- anova(Main_NO_2ways,Main_Stress)
  Valence <- anova(Main_NO_2ways,Main_Valence)
  Context <- anova(Main_NO_2ways,Main_Context)
  
  # Bind Models & Test results for output
  LMER.Models <- list(FullModel, Full_NO_3way, Full_NO_Stress.Valence, Full_NO_Valence.Context, Full_NO_Stress.Context, 
                      Main_NO_2ways, Main_Stress, Main_Valence, Main_Context)
  names(LMER.Models) <- c("FullModel", "Full_NO_3way", "Full_NO_Stress.Valence", "Full_NO_Valence.Context", "Full_NO_Stress.Context", 
                          "Main_NO_2ways", "Main_Stress", "Main_Valence", "Main_Context")
  ANOVAs.Tests <- list(Stress.Valence.Context, Stress.Valence, Stress.Context, Valence.Context, Stress, Valence, Context)
  names(ANOVAs.Tests) <- c("Stress.Valence.Context", "Stress.Valence", "Stress.Context", "Valence.Context", "Stress", "Valence", "Context")
  
  
  # Table of model comparisons (for Publication) -------------------------------
  # Set model names in export table
  Model <- c("Full Model", "Stress x Valence x Context","Stress x Context", "Stress x Valence", "Context x Valence", "Only Main", "Stress", "Context", "Valence")
  # Bind relevant information for publication table (chi2)
  Parameters <- (rbind(
    # Full model
    cbind(Stress.Valence.Context[2,c(1,4)], Stress.Valence.Context[1,c(6,7,8)]), # final 3 empty
    # 3way
    cbind(Stress.Valence.Context[1,c(1,4)], Stress.Valence.Context[2,c( 6, 7, 8)]),  # NB df en log-likelihood from model (row 1), chi-square chi-difference en p-value on row 2
    # 2way's
    cbind(Stress.Context[1,c(1,4)], Stress.Context[2,c(6, 7, 8)]),
    cbind(Stress.Valence[1,c(1,4)], Stress.Valence[2,c(6, 7, 8)]),
    cbind(Valence.Context[1,c(1,4)], Valence.Context[2,c( 6, 7, 8)]),
    # only main model
    cbind(Stress[2,c(1,4)], Stress[1,c(6,7,8)]),   # NB row 2 = complete model. 3= empty
    # Main effects
    cbind(Stress[1,c(1,4)], Stress[2,c(6,7,8)]),
    cbind(Context[1,c(1,4)], Context[2,c(6,7,8)]),
    cbind(Valence[1,c(1,4)], Valence[2,c(6,7,8)])
  ))
  ChiSquTable <- cbind(Model, Parameters)
  names(ChiSquTable)<- c("Model", "df", "LogLikelihood", "Chi2", "deltadf", "pvalue")  # Rename variables for flextable
  
  
  # PostHoc Tests & Effect Size's -------------------------------------------
  # PostHoc Testing for main effects:
  # NB PostHoc testing needs to be performed on the  Model with only main effects, if there is no significant interaction effects.
  PostHoc.Context<- PostHocTest.MCT(dependent.var, FittedModel=Main_NO_2ways, selected_contrast=expression(pairwise ~ context), factor_levels=2) 
  PostHoc.Valence<- PostHocTest.MCT(dependent.var, FittedModel=Main_NO_2ways, selected_contrast=expression(pairwise ~ emotion), factor_levels=2)
  PostHoc.Condition<- PostHocTest.MCT(dependent.var, FittedModel=Main_NO_2ways, selected_contrast=expression(pairwise ~ Condition), factor_levels=3)
  #PostHoc testing in the presence of a 3way interaction:
  # NB PostHoc testing needs to be performed on the Full Model, if there is a significant 3way interaction between the variables.
  PostHoc.Context.Stress.x.Valence<- PostHocTest.MCT(dependent.var, FittedModel=FullModel, selected_contrast=expression(pairwise ~ context|Condition+emotion), factor_levels=2) 
  PostHoc.Stress.Context.x.Valence<- PostHocTest.MCT(dependent.var, FittedModel=FullModel, selected_contrast=expression(pairwise ~ Condition|context+emotion), factor_levels=3)
  PostHoc.Valence.Stress.x.Context<- PostHocTest.MCT(dependent.var, FittedModel=FullModel, selected_contrast=expression(pairwise ~ emotion|Condition+context), factor_levels=2)
  
  # Create function output --------------------------------------------------
  Results<-  list(LMER.Models, ANOVAs.Tests, ChiSquTable, 
                  PostHoc.Condition, PostHoc.Valence,   PostHoc.Context,
                  PostHoc.Context.Stress.x.Valence, PostHoc.Stress.Context.x.Valence, PostHoc.Valence.Stress.x.Context)
  names(Results) <- c("LMER.Models", "ANOVAs.Tests", "ChiSqTable", 
                      "PostHoc.S", "PostHoc.V", "PostHoc.C", 
                      "PostHoc.C.S.V", "PostHoc.S.C.V", "PostHoc.V.S.C")
  return(Results)
  
} 


#' ### **Function suitable for MCT analyses with 1 within factor (Emotion) and between factor (Condition)**
MCT_Analysis.w.Emotion.b.Condition <- function(dependent.var, dataset){
  # w(ithin factors): Emotion; b(etween factor): Condition
  
  # LMER models -------------------------------------------------------------
  
  # Full Model
  FullModel <- lmer(get(dependent.var)~1+Condition*emotion+(1|id),data = dataset, REML=F)
  
  # # Test random effects.
  # # Define Models
  Main_NO_2ways<-lmer(get(dependent.var)~1+Condition+emotion+(1|id),data = dataset, REML=F)
  Main_Stress<-lmer(get(dependent.var)~1+emotion+(1|id),data = dataset, REML=F)
  Main_Valence<-lmer(get(dependent.var)~1+Condition+(1|id),data = dataset, REML=F)
  
  # Compare Models
  Stress.Valence <- anova(FullModel,Main_NO_2ways) # 2 Way interaction [compared to Full Model]
  Stress <- anova(Main_NO_2ways,Main_Stress)
  Valence <- anova(Main_NO_2ways,Main_Valence)
  
  # Bind Models & Test for output
  LMER.Models <- list(FullModel, Main_NO_2ways, Main_Stress, Main_Valence)
  names(LMER.Models) <- c("FullModel", "Main_NO_2ways", "Main_Stress", "Main_Valence")
  ANOVAs.Tests <- list(Stress.Valence, Stress, Valence)
  names(ANOVAs.Tests) <- c("Stress.Valence", "Stress", "Valence")
  
  
  # Table of model comparisons (Publication) --------------------------------
  # Note names for export table
  Model <- c("Full Model", "Stress x Valence", "Stress", "Valence")
  # Bind relevant information for Publication table chi2
  Parameters <- (rbind(
    # Full model
    cbind(Stress.Valence[2,c(1,4)], Stress.Valence[1,c(6,7,8)]), # NB row 2 full model, last 3 columns of row 1 are NA (which is correct)
    # 2way's
    cbind(Stress.Valence[1,c(1,4)],  Stress.Valence[2,c(6, 7, 8)]),
    # Main effects
    cbind(Stress[1,c(1,4)], Stress[2,c(6,7,8)]),
    cbind(Valence[1,c(1,4)], Valence[2,c(6,7,8)])
  ))
  ChiSquTable <- cbind(Model, Parameters)
  names(ChiSquTable)<- c("Model", "df", "LogLikelihood", "Chi2", "deltadf", "pvalue")  # Rename variables for 'flextable'
  
  
  # PostHoc Tests & Effect Size's -------------------------------------------
  # Posthoc tests if there are only main effects
  # NB PostHoc testing needs to be performed on the only Main Model, if there are only main effects (and no interaction)
  PostHoc.Stress<- PostHocTest.MCT(dependent.var, FittedModel=Main_NO_2ways, selected_contrast=expression(pairwise ~ Condition), factor_levels=3) 
  PostHoc.Valence<- PostHocTest.MCT(dependent.var, FittedModel=Main_NO_2ways, selected_contrast=expression(pairwise ~ emotion), factor_levels=2) 
  # Posthoc tests if there is a 2way interaction
  # NB PostHoc testing needs to be performed on the Full Model, if there is a significant 3way interaction between the variables.
  PostHoc.Stress.Valence<- PostHocTest.MCT(dependent.var, FittedModel=FullModel, selected_contrast=expression(pairwise ~ Condition|emotion), factor_levels=3) 
  PostHoc.Valence.Stress<- PostHocTest.MCT(dependent.var, FittedModel=FullModel, selected_contrast=expression(pairwise ~ emotion|Condition), factor_levels=2)
  
  
  # Create function output --------------------------------------------------
  Results<-  list(LMER.Models, ANOVAs.Tests, ChiSquTable, 
                  PostHoc.Stress, PostHoc.Valence, PostHoc.Stress.Valence, PostHoc.Valence.Stress)
  names(Results) <- c("LMER.Models", "ANOVAs.Tests", "ChiSqTable" , 
                      "PostHoc.S", "PostHoc.V", "PostHoc.SxV", "PostHoc.VxS")
  return(Results)
  
}
