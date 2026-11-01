#####################################################################
# Title: Disentangling Ethnicity from Nationality: Equivalence and 
# Correlatesof the CES-D Among Asian American, Asian National, and 
# Euro American College Students
# 
# Authors: Suzuki, Kalm, Willett, & Lui
#
# Statistical Analysis  Script: Conducted after Mplus measurement
# invariance analyses. 
# 
# Written by Takakuni Suzuki
#####################################################################

library(foreign)
library(psych)

rm(list = ls())

# Importing all data
setwd("")

df_sav_short <- read.csv("original_truncated.csv")

### CESD
# Reverse code
df_sav_short[,c("cesd4r", "cesd8r", "cesd12r", "cesd16r")] <- 5 - df_sav_short[,c("cesd4", "cesd8", "cesd12", "cesd16")]

# Total scale reliabiltiy
CESD_Total_Reliability <- alpha(df_sav_short[, c("cesd3", "cesd6", "cesd9", "cesd10", "cesd14", "cesd17", "cesd18",
                                                 "cesd4r", "cesd8r", "cesd12r", "cesd16r",
                                                 "cesd1", "cesd2", "cesd5", "cesd7", "cesd11", "cesd13", "cesd20",
                                                 "cesd15", "cesd19")], na.rm = TRUE)
CESD_Total_Reliability[["total"]][["raw_alpha"]]

# Dep Aff scale reliabiltiy
CESD_DepAFf_Reliability <- alpha(df_sav_short[, c("cesd3", "cesd6", "cesd9", "cesd10", "cesd14", "cesd17", "cesd18")], na.rm = TRUE)
CESD_DepAFf_Reliability[["total"]][["raw_alpha"]]

# Pos Aff scale reliabiltiy
CESD_PosAff_Reliability <- alpha(df_sav_short[, c("cesd4r", "cesd8r", "cesd12r", "cesd16r")], na.rm = TRUE)
CESD_PosAff_Reliability[["total"]][["raw_alpha"]]

# Som Sxs scale reliabiltiy
CESD_SomSxs_Reliability <- alpha(df_sav_short[, c("cesd1", "cesd2", "cesd5", "cesd7", "cesd11", "cesd13", "cesd20")], na.rm = TRUE)
CESD_SomSxs_Reliability[["total"]][["raw_alpha"]]

# Int Pro scale reliabiltiy
CESD_IntPro_Reliability <- alpha(df_sav_short[, c("cesd15", "cesd19")], na.rm = TRUE)
CESD_IntPro_Reliability[["total"]][["raw_alpha"]]


### Individualism and Collectivism 

### Loss of Face (Zane et al)
df_sav_short$LoF <- (rowMeans(df_sav_short[, c("lof1", "lof2", "lof3", "lof4", "lof5", "lof6", "lof7", "lof8", 
                                               "lof9", "lof10", "lof11", "lof12", "lof13", "lof14", "lof15", "lof16", 
                                               "lof17", "lof18", "lof19", "lof20", "lof21")], na.rm = TRUE)-1) * 21

LoF_Reliability <- alpha(df_sav_short[, c("lof1", "lof2", "lof3", "lof4", "lof5", "lof6", "lof7", "lof8", 
                                          "lof9", "lof10", "lof11", "lof12", "lof13", "lof14", "lof15", "lof16", 
                                          "lof17", "lof18", "lof19", "lof20", "lof21")], na.rm = TRUE)
LoF_Reliability[["total"]][["raw_alpha"]]



### Individualism and Collectivism (Triandis 1995) 
# Reverse code
df_sav_short[,c("indcol30r")] <- 10 - df_sav_short[,c("indcol30")] 

# Individualism Horizontal scale score calculation and reliability
df_sav_short$Indiv_H <- (rowMeans(df_sav_short[, c("indcol1", "indcol5", "indcol6", "indcol15", "indcol18", 
                                                   "indcol21", "indcol25", "indcol32")], na.rm = TRUE)-1) * 8
Indiv_H_Reliability <- alpha(df_sav_short[, c("indcol1", "indcol5", "indcol6", "indcol15", "indcol18", 
                                              "indcol21", "indcol25", "indcol32")], na.rm = TRUE)
Indiv_H_Reliability[["total"]][["raw_alpha"]]

# Individualism Vertical scale score calculation and reliability
df_sav_short$Indiv_V <- (rowMeans(df_sav_short[, c("indcol4", "indcol8", "indcol10", "indcol12", "indcol19",  
                                                   "indcol23", "indcol26", "indcol30r")], na.rm = TRUE)-1) * 8
Indiv_V_Reliability <- alpha(df_sav_short[, c("indcol4", "indcol8", "indcol10", "indcol12", "indcol19",  
                                              "indcol23", "indcol26", "indcol30r")], na.rm = TRUE)
Indiv_V_Reliability[["total"]][["raw_alpha"]]

# Collectivism Horizontal scale score calculation and reliability
df_sav_short$Coll_H <- (rowMeans(df_sav_short[, c("indcol2", "indcol9", "indcol11", "indcol14", "indcol16", 
                                                  "indcol20", "indcol22", "indcol28")], na.rm = TRUE)-1) * 8
Coll_H_Reliability <- alpha(df_sav_short[, c("indcol2", "indcol9", "indcol11", "indcol14", "indcol16", 
                                             "indcol20", "indcol22", "indcol28")], na.rm = TRUE)
Coll_H_Reliability[["total"]][["raw_alpha"]]

# Collectivism Vertical scale score calculation and reliability
df_sav_short$Coll_V <- (rowMeans(df_sav_short[, c("indcol3", "indcol7", "indcol13", "indcol17", "indcol24", 
                                                  "indcol27", "indcol29", "indcol31")], na.rm = TRUE)-1) * 8
Coll_V_Reliability <- alpha(df_sav_short[, c("indcol3", "indcol7", "indcol13", "indcol17", "indcol24", 
                                             "indcol27", "indcol29", "indcol31")], na.rm = TRUE)
Coll_V_Reliability[["total"]][["raw_alpha"]]


### Scale of Ethnic Experience (Malcarne et al 2006)
# Reverse code
df_sav_short[,c("see1r", "see4r", "see7r", "see8r", "see12r", "see27r", "see2r", "see3r", "see21r", "see31r", "see11r", "see32r")] <- 6 - df_sav_short[,c("see1", "see4", "see7", "see8", "see12", "see27", "see2", "see3", "see21", "see31", "see11", "see32")] 

# Ethnic Identify scale score calculation and reliability
df_sav_short$EthID <- (rowMeans(df_sav_short[, c("see1r", "see4r", "see7r", "see8r", "see12r", "see14", 
                                                 "see16", "see20", "see23", "see25", "see27r", "see30")], na.rm = TRUE)-1) * 12
EthID_Reliability <- alpha(df_sav_short[, c("see1r", "see4r", "see7r", "see8r", "see12r", "see14", 
                                            "see16", "see20", "see23", "see25", "see27r", "see30")], na.rm = TRUE)
EthID_Reliability[["total"]][["raw_alpha"]]

# Discrimination scale score calculation and reliability
df_sav_short$Disc <- (rowMeans(df_sav_short[, c("see2r", "see3r", "see13", "see19", "see21r", 
                                                "see24", "see26", "see29", "see31r")], na.rm = TRUE)-1) * 9
Disc_Reliability <- alpha(df_sav_short[, c("see2r", "see3r", "see13", "see19", "see21r", 
                                           "see24", "see26", "see29", "see31r")], na.rm = TRUE)
Disc_Reliability[["total"]][["raw_alpha"]]

# Minastream Comfort scale score calculation and reliability
df_sav_short$Comf <- (rowMeans(df_sav_short[, c("see6", "see9", "see11r", "see17", "see28", "see32r")], na.rm = TRUE)-1) * 6
Comf_Reliability <- alpha(df_sav_short[, c("see6", "see9", "see11r", "see17", "see28", "see32r")], na.rm = TRUE)
Comf_Reliability[["total"]][["raw_alpha"]]

# Ethnic Affiliation scale score calculation and reliability
df_sav_short$Affil <- (rowMeans(df_sav_short[, c("see5", "see10", "see15", "see18", "see22")], na.rm = TRUE)-1) * 5
Affil_Reliability <- alpha(df_sav_short[, c("see5", "see10", "see15", "see18", "see22")], na.rm = TRUE)
Affil_Reliability[["total"]][["raw_alpha"]]


##### Correlation between Latent CESD and Cultural Experiences
# Import latent CESD factor scores data and combine
fscores <- read.table("fscores_3_All_Scalar.dat",
                      na.strings = "*")
names(fscores) <- c("mcesd1", "mcesd2", "mcesd3", "mcesd4", "mcesd5", "mcesd6", 
                    "mcesd8", "mcesd9", "mcesd10", "mcesd11", "mcesd12", 
                    "mcesd13", "mcesd14", "cmesd16", "mcesd17", "mcesd18", 
                    "mcesd20", "ID", "DepAffL", "DepAffSE", "PosAffL", "PosAffSE", "SomSxsL", "SomSxsSE", "group")
for (i in 1:17){
  fscores[,i] = as.numeric(unlist(fscores[,i]))
}

# Merge cultural variables and CESD latent scores
combined <- merge(df_sav_short, fscores, by.x = "X", by.y = "ID")

# Correlations by group
df_L_AA <- combined[which(combined$grp == 1),]
r_L_AA <- corr.test(df_L_AA[,c("DepAffL", "PosAffL", "SomSxsL", "LoF", "Indiv_H", "Indiv_V", "Coll_H", "Coll_V", "EthID", "Comf", "Disc", "Affil")])
print(r_L_AA$r, digits = 3)
print(r_L_AA$p, digits = 3)
max(r_L_AA$n)

df_L_AN <- combined[which(combined$grp == 2),]
r_L_AN <- corr.test(df_L_AN[,c("DepAffL", "PosAffL", "SomSxsL", "LoF", "Indiv_H", "Indiv_V", "Coll_H", "Coll_V", "EthID", "Comf", "Disc", "Affil")])
print(r_L_AN$r, digits = 3)
print(r_L_AN$p, digits = 3)
max(r_L_AN$n)

df_L_EA <- combined[which(combined$grp == 3),]
r_L_EA <- corr.test(df_L_EA[,c("DepAffL", "PosAffL", "SomSxsL", "LoF", "Indiv_H", "Indiv_V", "Coll_H", "Coll_V", "EthID", "Comf", "Disc", "Affil")])
print(r_L_EA$r, digits = 3)
print(r_L_EA$p, digits = 3)
max(r_L_EA$n)

# Fisher Z Transformations and merging
df_AA_z <- fisherz(r_L_AA$r[4:12,1:3])
dimnames(df_AA_z)[[2]] <- c("DepAff_AA", "PosAff_AA", "SomSxs_AA")
df_AN_z <- fisherz(r_L_AN$r[4:12,1:3])
dimnames(df_AN_z)[[2]] <- c("DepAff_AN", "PosAff_AN", "SomSxs_AN")
df_EA_z <- fisherz(r_L_EA$r[4:12,1:3])
dimnames(df_EA_z)[[2]] <- c("DepAff_EA", "PosAff_EA", "SomSxs_EA")

df_All_z <- cbind(df_AA_z, df_AN_z)
df_All_z <- cbind(df_All_z, df_EA_z)

# r, alerting-CV
corr.test(df_All_z)

