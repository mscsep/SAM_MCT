# SAM_MCT
Analyses of Memory Contextualization Task in SAM study, as described in:

* Sep, M.S.C., van Ast, V.A., Gorter, R., Joëls, M., & Geuze, E. (2019) Time-dependent effects of psychosocial stress on the contextualization of neutral memories. Psychoneuroendocrinology, 108, 140–149. https://doi.org/10.1016/j.psyneuen.2019.06.021

#### Step 1: Data and Preprocessing
- Dataset with preprocessed MCT data `SAM_MCT.csv` (will be available on Dataverse). 
- Data can be loaded and preprocessed for analysis using the script `Load_MCT_Data.r` (in the 'R' folder). Note this script sources `LMM_Contrasts_Experimental_Conditions_SAM.r` (also in the 'R' folder).

#### Step 2: Analyses with linear mixed effect models (LMM)
Perform LMM assumption checks and analyses via `MCT_LMM_Analyses_Call.r`. Note, this script sources:
- `Load_MCT_Data.r` and `MCT_LMM_Analyses_Source.r`(available in 'R' folder) 
- `LMM_Assumptions.R` and  `LMM_Results_to_Word.R` (available via [this repository](https://github.com/mscsep/SAM_sAA_Cortisol.git))

#### Step 3: Data Visualization:
- Figures: `MCT_Results_to_Plot.r`
- Tables: `LMM_Results_to_Word.R` (available via [this repository](https://github.com/mscsep/SAM_sAA_Cortisol.git))
