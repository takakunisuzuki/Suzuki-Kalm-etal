#####################################################################
# Title: Disentangling Ethnicity from Nationality: Equivalence and 
# Correlatesof the CES-D Among Asian American, Asian National, and 
# Euro American College Students
# 
# Authors: Suzuki, Kalm, Willett, & Lui
#
# Data Management Script: Subsetting to only necessary variables and 
# exporting data for Mplus and later analyses
# 
# Written by Takakuni Suzuki
#####################################################################

library(foreign)
library(psych)

rm(list = ls())

# Importing, subsetting, and managing data
setwd("")

df_sav <- read.spss("File.sav", to.data.frame = TRUE)

df_sav_short <- df_sav[,c("cesd1", "cesd2", "cesd3", "cesd4", "cesd5", "cesd6", 
                          "cesd7", "cesd8", "cesd9", "cesd10", "cesd11", "cesd12", 
                          "cesd13", "cesd14", "cesd15", "cesd16", "cesd17", "cesd18", 
                          "cesd19", "cesd20", "gender", "age", "eth", "immigstat",
                          "lof1", "lof2", "lof3", "lof4", "lof5", "lof6", "lof7", "lof8", 
                          "lof9", "lof10", "lof11", "lof12", "lof13", "lof14", "lof15", "lof16", 
                          "lof17", "lof18", "lof19", "lof20", "lof21",
                          "indcol1", "indcol2", "indcol3", "indcol4", "indcol5", "indcol6", "indcol7", 
                          "indcol8", "indcol9", "indcol10", "indcol11", "indcol12", "indcol13", "indcol14", 
                          "indcol15", "indcol16", "indcol17", "indcol18", "indcol19", "indcol20", 
                          "indcol21", "indcol22", "indcol23", "indcol24","indcol25", "indcol26", 
                          "indcol27", "indcol28", "indcol29", "indcol30", "indcol31", "indcol32",
                          "see1", "see2", "see3", "see4", "see5", "see6", "see7", 
                          "see8", "see9", "see10", "see11", "see12", "see13", "see14", 
                          "see15", "see16", "see17", "see18", "see19", "see20", 
                          "see21", "see22", "see23", "see24","see25", "see26", 
                          "see27", "see28", "see29", "see30", "see31", "see32")]

df_sav_short <- df_sav_short[df_sav_short$eth %in% c("European American", "Asian/Asian American"),]

df_sav_short$grp <- ifelse((df_sav_short$eth == "European American"), 3,
                           ifelse(df_sav_short$immigstat == "Temporary stay/International student (you were foreign-born)", 2, 1))

df_sav_short[(df_sav_short$eth == "European American")&(df_sav_short$immigstat == "Temporary stay/International student (you were foreign-born)"),] <-NA

df_sav_short$grp <- as.factor(df_sav_short$grp)

df_sav_short <- df_sav_short[rowSums(is.na(df_sav_short[,1:20])) != 20, ]
df_sav_short <- df_sav_short[!is.na(df_sav_short$grp), ]

for (i in 1:20){
  df_sav_short[,i] = as.numeric(unlist(df_sav_short[,i]))
}
for (i in 25:109){
  df_sav_short[,i] = as.numeric(unlist(df_sav_short[,i]))
}

for (i in 1:20){
  print(table(df_sav_short$grp,df_sav_short[,i]))
}


# Checking descriptive statistics of CESD items by group
describeBy(df_sav_short[,c(1:22)], df_sav_short$grp)

# Checking Item 17 response rate by group, and recoding
table(df_sav_short[,c("cesd17", "grp")])

df_sav_short$cesd17[df_sav_short$cesd17 == 4] <- 3


# Demographics
print(describe(df_sav_short[,1:22]), digits = 3)
print(describeBy(df_sav_short[,c(21:22)], df_sav_short$grp), digits = 3)
table(df_sav_short[,c(25:26)])
# AA 2nd/3rd generation
100*93/144
# AA 1st/1.5 gen
100*51/144

# EA 4th gen
100*116/127

df_sav_short$miss <- rowSums(is.na(df_sav_short[,1:20]))
table(df_sav_short$miss)
print(summary(df_sav_short[,1:22]), digits = 3)

100 * 6 / (20*439)


# Export relevant data
# Includes all data
write.csv(df_sav_short, file = "original_truncated.csv")

# Only CESD items for Mplus analyses
write.csv(df_sav_short[,c("cesd1", "cesd2", "cesd3", "cesd4", "cesd5", "cesd6", 
                          "cesd7", "cesd8", "cesd9", "cesd10", "cesd11", "cesd12", 
                          "cesd13", "cesd14", "cesd15", "cesd16", "cesd17", "cesd18", 
                          "cesd19", "cesd20","grp")], file = "Data4MpluswithID.csv", row.names = T, na = "999")

##### Conduct Mplus Analyes Before Proceeding #####