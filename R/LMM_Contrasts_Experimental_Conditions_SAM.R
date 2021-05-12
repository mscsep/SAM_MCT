# Define experimental contrasts SAM study
# written by Milou Sep

set_contrasts_SAM <- function(){
  #' ### **Set Contrasts**
  #' **Planned contrasts for experimental conditions (NB same for MCT & FGT)**
  # Experimental conditions 1=delayed;2=direct;3=control.
  # - Compare delayed stress (1) with control (3)
  # - Compare direct stress (2) with control (3)
  # - Compare delayed stress (1) with direct stress (2)
  # - Compare stress (1&2) with control (3)
  
  # Experimental Contrast 1: Delayed vs control & Direct vs control
  # Non-orthogonal contrast: intercept represents control condition
  # group     contrast1    contrast2
  # 1.delayed      1             0
  # 2.direct       0             1
  # 3.control      0             0
  delayedVScontrol<-c(1,0,0)
  directVScontrol<-c(0,1,0)
  exp.contr.1 <-cbind(delayedVScontrol,directVScontrol)
  
  # Experimental Contrast 2: Delayed vs Direct stress
  # Non-orthogonal contrast: intercept represents direct stress condition
  # group     contrast3   contrast4
  # 1.delayed      1           0
  # 2.direct       0           0
  # 3.control      0           1
  delayedVSdirect<-c(1,0,0)
  controlVSdirect<-c(0,0,1)
  exp.contr.2 <-cbind(delayedVSdirect,controlVSdirect)
  
  # Experimental Contrast 3: Stress (Delayed & Direct) vs Control
  # Orthogonal intercept represents mean [NB the desired comparison of 2 groups with 1 group, asked for orthogonal contrasts]
  #  group     contrast5   contrast6
  # 1.delayed      1            1
  # 2.direct       1           -1
  # 3.control      -2           0
  stressVScontrol<-c(1,1,-2)
  delayedVSdirect<-c(1,-1,0)
  exp.contr.3 <-cbind(stressVScontrol,delayedVSdirect)
  
  # Bind contrast 1, 2 and 3 for experimental condition in list "exp.contr"
  # NB run LMER model separately for contrasts 1-3
  exp.contr <- list(exp.contr.1, exp.contr.2, exp.contr.3)
  
  return(exp.contr)
  
}

exp.contr <- set_contrasts_SAM()
