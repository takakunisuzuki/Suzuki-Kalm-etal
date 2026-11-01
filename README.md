This folder contains de-identified data and scripts for:
Suzuki, T., Kalm, C., Willett, Z., & Lui, P. P. (2026, October 14). Disentangling Ethnicity from Nationality: Equivalence and Correlates of the CES-D Among Asian American, Asian National, and Euro American College Students. Retrieved from osf.io/preprints/psyarxiv/tmjcp_v1

If you use any part of the code or data in this repository, please cite the preprint above or the subsequent manuscript. 

License
- R and Mplus scripts: MIT License
- Data: CC BY 4.0


R Script
- Data Management.R: This script imports the data with variables extraneous for this project. It subsets, checks the descriptive statistics, and exports data for Mplus and statistical analyses
- Statistical Analyses.R: This script conducts the statistical analyses

Mplus
- Step5b MI_3sansINT_7_All_Scalar.inp: This is one of the Mplus measurement invariance scripts. This is the script that tested for scalar invariance of the modified three-factor CESD across all three groups. 

De-identified Data (Please do not attempt to re-identify participants)
- original_truncated.csv: This includes all data necessary for this project. 
- Data4MpluswithID.csv: This data is a subset of data only including CESD and ID variables to conduct the measurement invariance analyses. 
- fscores_3_All_Scalar.dat: This data contains the latent factor scores from the modified three-factor CESD model

Contact: Takakuni Suzuki
