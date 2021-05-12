#' ---	
#' title: Load & Restructure MCT data
#' author: "Milou Sep"	
#' output:	
#'   html_document: default	
#' ---	

#' **prepare environment**
rm(list=ls())
# Load Packages -----------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)

#' **Load Data**
# Load data ---------------------------------------------------------------
data_MCT <- read_delim("data/SAM_MCT.csv", ";",locale=locale(decimal_mark = ","), escape_double = FALSE, trim_ws = TRUE, na = "NaN")

#' **id**
# Set factor for id
data_MCT$subjName<-sub("SMGH","",data_MCT$subjName) #remove letters from variable names 
data_MCT$id<-as.factor(c(1:100))

#' **contrasts**
# Load Contrasts ----------------------------------------------------------  
source('R/LMM_Contrasts_Experimental_Conditions_SAM.R') # Experimental contrasts
# MCT task contrasts to emotion & context!
MCT_task_contrasts <- function(dataset, withinfactors){
  # assign task contrasts for MCT:
  if(withinfactors == 2){
    # Contrast for Congruent/Incongruent within subject factor where intercept represents Incongruent
    # group      contrast1
    # 1.Congruent    1
    # 2.Incongruent  0
    CongruentVSIncongruent<-c(1,0)
    contrasts(dataset$context)<-cbind(CongruentVSIncongruent)
    str(dataset$context)
  }
  # Contrast for Negative/Neutral within subject factor where intercept represents Neutral
  # group      contrast1
  # 1.Negative      1
  # 2.Neutral       0  
  NegativeVSNeutral<-c(1,0)
  contrasts(dataset$emotion)<-cbind(NegativeVSNeutral)
  str(dataset$emotion)
  
  return(dataset)
}


# Restructure Data (Memory Sensitivity Index) -----------------------------

#' **dPrime**
# Reshape dprime data long
table(data_MCT$Condition) # Condition distribution

data_MCT %>% select("id","Condition", contains("dPrime"), # Select columns id, condition and column names with dPrime in it
                    -contains("CER"), -contains("INL"), -contains("_diff"), -contains("total_")) %>% # Don't select columns with CER, INL, _diff or total_ in it.
  data.frame() %>% droplevels() -> dPrime # store new dataframe

# Change to long format.
gather(dPrime, key= "variable", value="dPrime", names(dPrime)[3:6]) -> dPrime_long

# Create variables "context" and "emotion
dPrime_long %>% 
  mutate(Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         context=factor(substr(variable, 1,1), levels=c("I", "R"), labels=c("Congruent", "Incongruent")), 
         emotion=factor(substr(variable, 2,2), levels=c("A", "N"), labels=c("Negative", "Neutral")))  %>%
  data.frame() %>% droplevels() -> dPrime_long

# Apply task contrasts
data_long.dprime <- MCT_task_contrasts(dataset= dPrime_long ,withinfactors=2) # NB if within factors == 2 (contrast's for emotion & context changed)
contrasts(data_long.dprime$emotion) #check
contrasts(data_long.dprime$context)
# which(is.na(data_long.dprime))

#' ** FOR sensitivity analysis dPrime with certainty rates**
data_MCT %>% select("id","Condition", contains("CER_dPrime"), -contains("diff"))%>% # Select columns id, condition and column names with CER_dPrime in it
  data.frame() %>% droplevels() -> dPrime_CER # store new dataframe
# Change to long format.
gather(dPrime_CER, key= "variable", value="dPrime", names(dPrime_CER)[3:14]) -> dPrime_CER_long 
# Create variables "context" and "emotion and "Certainty"
dPrime_CER_long %>% 
  mutate(Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         context=factor(substr(variable, 1,1), levels=c("I", "R"), labels=c("Congruent", "Incongruent")), 
         emotion=factor(substr(variable, 2,2), levels=c("A", "N"), labels=c("Negative", "Neutral")),
         certainty=factor(substr(variable, 4,6), levels=c("HIG","MID", "LOW"), labels=c("HIGH", "MID", "LOW")))  %>%
  data.frame() %>% droplevels() -> dPrime_CER_long
dPrime_CER_long <- MCT_task_contrasts(dataset= dPrime_CER_long ,withinfactors=2)

#' **dPrime contextualization index**
# Reshape dprime contextualization index data long 

# Select relevant variables
data_MCT %>% 
  select("id","Condition", contains("dPrime_diff"), # Select columns id, condition and column names with dPrime in it
         -contains("CER"), -contains("INL"), -contains("total_")) %>% # Don't select columns with CER, INL, _diff or total_ in it.
  data.frame() %>% droplevels()-> dPrime_diff # store new dataframe
# Change to long format.
gather(dPrime_diff, key= "variable", value="dprime_contextualization", names(dPrime_diff)[3:4]) -> dPrime_diff_long
# Create varibale "emotion
dPrime_diff_long %>% 
  mutate( Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
          emotion=factor(substr(variable, 1,1), levels=c("A", "N"), labels=c("Negative", "Neutral")))  %>%
  data.frame() %>% droplevels() -> dPrime_diff_long
# Apply task contrasts
data_long.dprime_diff<- MCT_task_contrasts(dataset= dPrime_diff_long ,withinfactors=1)
contrasts(data_long.dprime_diff$emotion) #check

#' ** FOR sensitivity analysis dPrime contextualization index with certainty rates**
data_MCT %>% select("id","Condition", contains("CER_dPrime_diff"))%>% # Select columns id, condition and column names with CER_dPrime in it
  data.frame() %>% droplevels() -> dPrime_diff_CER # store new dataframe
# Change to long format.
gather(dPrime_diff_CER, key= "variable", value="dPrime", names(dPrime_diff_CER)[3:8]) -> dPrime_diff_CER_long
# Create varibales "context" and "emotion and "Certainty"
dPrime_diff_CER_long %>% 
  mutate(Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         emotion=factor(substr(variable, 1,1), levels=c("A", "N"), labels=c("Negative", "Neutral")),
         certainty=factor(substr(variable, 3,5), levels=c("HIG","MID", "LOW"), labels=c("HIGH", "MID", "LOW")))  %>%
  data.frame() %>% droplevels() -> dPrime_diff_CER_long

dPrime_diff_CER_long<- MCT_task_contrasts(dataset= dPrime_diff_CER_long ,withinfactors=1)


# Restructure Data (Criterion c) ------------------------------------------

#' **Criterion c**
# Reshape c data long

# Select relevant variables
data_MCT %>% 
  select("id","Condition", contains("_c"), # Select columns id, condition and column names with dPrime in it
         -contains("CER"), -contains("INL"), -contains("total_"), -contains("Count")) %>% # Don't select columns with CER, INL, _diff or total_ in it.
  data.frame() %>% droplevels()-> c # store new dataframe

# Change to long format.
gather(c, key= "variable", value="C", names(c)[3:6]) -> c_long

# Create variables "context" and "emotion"
c_long %>% 
  mutate(Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         context=factor(substr(variable, 1,1), levels=c("I", "R"), labels=c("Congruent", "Incongruent")), 
         emotion=factor(substr(variable, 2,2), levels=c("A", "N"), labels=c("Negative", "Neutral")))  %>%
  data.frame() %>% droplevels() -> c_long

# Apply task contrasts
data_long.C<- MCT_task_contrasts(dataset= c_long ,withinfactors=2)
contrasts(data_long.C$emotion) #check
contrasts(data_long.C$context) #check


# Restructure Data (Signal Detection Measures) ----------------------------

#' **pHit**
# Reshape pHit data long

# Select relevant variables
data_MCT %>% 
  select("id","Condition", contains("hit"), # Select columns id, condition and column names with dPrime in it
         -contains("CER"), -contains("INL"), -contains("total_"), -contains("Count")) %>% # Don't select columns with CER, INL, _diff or total_ in it.
  data.frame() %>% droplevels()-> pHit # store new dataframe

# Change to long format.
gather(pHit, key= "variable", value="pHit", names(pHit)[3:6]) -> pHit_long

# Create variables "context" and "emotion
pHit_long %>% 
  mutate( Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
          context=factor(substr(variable, 1,1), levels=c("I", "R"), labels=c("Congruent", "Incongruent")), 
          emotion=factor(substr(variable, 2,2), levels=c("A", "N"), labels=c("Negative", "Neutral")))  %>%
  data.frame() %>% droplevels() -> pHit_long
# Apply task contrasts
data_long.pHit<- MCT_task_contrasts(dataset= pHit_long ,withinfactors=2)
contrasts(data_long.pHit$emotion) #check
contrasts(data_long.pHit$context) #check


#' ** FOR sensitivity analysis hit with certainty rates**
data_MCT %>% select("id","Condition", contains("CER_Count")) %>% # Select columns id, condition and column names with CER_Count in it
data.frame() %>% droplevels() -> hit_CER # store new dataframe

# Change to long format.
gather(hit_CER, key= "variable", value="hit", names(hit_CER)[3:20]) -> hit_CER_long
# Create variables "context" and "emotion and "Certainty"
hit_CER_long %>% 
  mutate(Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         context=factor(substr(variable, 1,1), levels=c("I", "R"), labels=c("Congruent", "Incongruent")), 
         emotion=factor(substr(variable, 2,2), levels=c("A", "N"), labels=c("Negative", "Neutral")),
         certainty=factor(substr(variable, 8,10), levels=c("HIG","MID", "LOW"), labels=c("HIGH", "MID", "LOW")))  %>%
  data.frame() %>% droplevels() -> hit_CER_long
hit_CER_long <- MCT_task_contrasts(dataset= hit_CER_long ,withinfactors=2)


#' **pHit contextualization**
# Reshape hit contextualization (I-R)
# Use pHit to calculate contextualization indices
pHit %>% mutate(N_pHit_diff= IN_hit_Rate - RN_hit_Rate,  # Calculate Hit Contextualization score (Congruent - Incongruent) for Neutral & Negative faces
                A_pHit_diff= IA_hit_Rate - RA_hit_Rate) %>% select(id, Condition, N_pHit_diff, A_pHit_diff) %>%
  data.frame() %>% droplevels() -> pHit_diff

# Change to long format.
gather(pHit_diff, key= "variable", value="pHit.ctx", names(pHit_diff)[3:4]) -> pHit_diff_long
table(pHit_diff_long$Condition)

# Create variable "emotion
pHit_diff_long %>% 
  mutate(Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         emotion=factor(substr(variable, 1,1), levels=c("A", "N"), labels=c("Negative", "Neutral")))  %>%
  data.frame() %>% droplevels() -> pHit_diff_long
# Apply task contrasts
data_long.pHit.ctx<- MCT_task_contrasts(dataset= pHit_diff_long ,withinfactors=1)
contrasts(data_long.pHit.ctx$emotion) #check

#' **pFA**
# Reshape pFA data long
table(data_MCT$Condition)

# Select relevant variables
data_MCT %>% 
  select("id","Condition", contains("FA"), # Select columns id, condition and column names with pFA in it
         -contains("CER"), -contains("INL"), -contains("total_"), -contains("Count")) %>% # Don't select columns with CER, INL, _diff or total_ in it.
  data.frame() %>% droplevels()-> pFA # store new dataframe

# Change to long format.
gather(pFA, key= "variable", value="pFA", names(pFA)[3:4]) -> pFA_long

# Create variables "context" and "emotion
pFA_long %>% 
  mutate(id=as.factor(id), 
         Condition=factor(as.character(Condition), levels = c(1,2,3), labels=c("Delayed", "Direct", "Control")),
         emotion=factor(substr(variable, 2,2), levels=c("A", "N"), labels=c("Negative", "Neutral")))  %>%
  data.frame() %>% droplevels() -> pFA_long
# Apply task contrasts
data_long.pFA<- MCT_task_contrasts(dataset= pFA_long ,withinfactors=1)
contrasts(data_long.pFA$emotion) #check
