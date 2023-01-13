### Master Script ###


### PART 1 - LOAD NECESSARY PACKAGES ###

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
#Required Packages #

#no longer required
install.packages("ggpubr")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("dplyr")
install.packages("car")
install.packages("writexl")
install.packages("xlsx")
install.packages("psych")
install.packages("cowplot")
install.packages("caret")
install.packages("varrank")
install.packages("mgcv")
install.packages("plyr")
install.packages("dplyr")
#install.packages("xlsx")
install.packages("naniar")
#install.packages("summarytools") #Crashes R
install.packages("epiDisplay")
install.packages("ltm")
install.packages("psych")

library(readxl)
library(ggpubr)
library(tidyverse)
library(Hmisc)
library(corrplot)
library(dplyr)
library(car)
library(writexl)
library(psych)
library(cowplot)
library(caret)
library(varrank)
library(mgcv)
library(plyr)
library(dplyr)
#library(xlsx)
library(naniar)
#library(summarytools)
library(epiDisplay)
library(ltm)
library(psych)


##### PART 2 - CLEAN THE DATAFRAME #####


#First let's important the dataframes

df.firebase <- read_excel("data/2021_12_09_Preliminary_Data_Analysis.xlsx")
df.qualtrics <- read.csv("data/PEM_2022_Qualtrics.csv")

names(df.firebase)[names(df.firebase) == "Ethnicity"] <- "Gender"
names(df.firebase)[names(df.firebase) == "Native_Language"] <- "Education"

df <- bind_rows(df.firebase,df.qualtrics)

View(df)



# PERSONALITY SCORING 

#let's reverse score variables

##### REVERSE SCORING THE BFAS ######

columns_to_reverse <- c("BFAS_15", "BFAS_45", "BFAS_55", "BFAS_85",
                        "BFAS_50", "BFAS_60", "BFAS_80", "BFAS_90",
                        "BFAS_13", "BFAS_23", "BFAS_33", "BFAS_53",
                        "BFAS_83", "BFAS_93", "BFAS_8", "BFAS_48",
                        "BFAS_68", "BFAS_78", "BFAS_14", "BFAS_24",
                        "BFAS_34", "BFAS_54", "BFAS_64", "BFAS_29",
                        "BFAS_49", "BFAS_79", "BFAS_99", "BFAS_2",
                        "BFAS_32", "BFAS_52", "BFAS_62", "BFAS_82",
                        "BFAS_17", "BFAS_37", "BFAS_67", "BFAS_77",
                        "BFAS_87", "BFAS_97", "BFAS_1", "BFAS_21",
                        "BFAS_41", "BFAS_71", "BFAS_16", "BFAS_36",
                        "BFAS_56", "BFAS_76")

df_v1_old <- df 

reversed.scores <- df


reversed.scores[, columns_to_reverse] <- 6 - df[, columns_to_reverse]

df <- reversed.scores


#Create a function to calculate row means: return

#var_mean <- function(x, y, z){
#  x$y <- rowMeans(x[, z, na.rm = TRUE])
#  return(x$y)
#}

#ote <- c(50, 12, 23, 34,
#         45, 56, 67, 78,
#         89, 100, 6, 18,
#         29, 40, 51, 62,
#         73, 84, 95, 7)

#var_mean(df, Openness_to_Experience, ote)


## Create new Variables - Personality & Emotion

df$Openness_to_Experience <- rowMeans(df[, c(50, 12, 23, 34,
                                             45, 56, 67, 78,
                                             89, 100, 6, 18,
                                             29, 40, 51, 62,
                                             73, 84, 95, 7), na.rm = TRUE])


df$Intellect <- rowMeans(df[, c(50, 12, 23, 34,
                                45, 56, 67, 78,
                                89, 100), na.rm = TRUE])

df$Openness <- rowMeans(df[, c(6, 18,
                               29, 40, 51, 62,
                               73, 84, 95, 7), na.rm = TRUE])

### Conscientiousness 


df$Conscientiousness <- rowMeans(df[, c(28, 10, 21, 32,
                                        43, 54, 65, 76,
                                        87, 98,
                                        83, 15,
                                        26, 37, 48, 59,
                                        70, 81, 92, 103), na.rm = TRUE])
df$Industriousness <- rowMeans(df[, c(28, 10, 21, 32,
                                      43, 54, 65, 76,
                                      87, 98), na.rm = TRUE])
df$Orderliness <- rowMeans(df[, c(
  83, 15,
  26, 37, 48, 59,
  70, 81, 92, 103), na.rm = TRUE])

##



df$Extraversion <- rowMeans(df[, c(39, 11, 22, 33, 44, 55, 66, 77, 88, 99,
                                   94, 16, 27, 38, 49, 60, 71, 82, 93, 104), na.rm = TRUE])


df$Enthusiasm <- rowMeans(df[, c(39, 11, 22, 33, 44, 55, 66, 77, 88, 99), na.rm = TRUE])


df$Assertiveness <- rowMeans(df[, c(94, 16, 27, 38, 49, 60, 71, 82, 93, 104), na.rm = TRUE])



## Agreeableness ##


df$Agreeableness <- rowMeans(df[, c(17, 9, 20, 31, 42, 53, 64, 75, 86, 97,
                                    72, 14, 25, 36, 47, 58, 69, 80, 91, 102), na.rm = TRUE])



df$Compassion <- rowMeans(df[, c(17, 9, 20, 31, 42, 53, 64, 75, 86, 97), na.rm = TRUE])

df$Politeness <- rowMeans(df[, c(72, 14, 25, 36, 47, 58, 69, 80, 91, 102), na.rm = TRUE])

## Neuroticism ##


df$Neuroticism <- rowMeans(df[, c(5, 8, 19, 30, 41, 52, 63, 74, 85, 96,
                                  61, 13, 24, 35, 46, 75, 68, 79, 90, 10), na.rm = TRUE])


df$Withdrawal <- rowMeans(df[, c(5, 8, 19, 30, 41, 52, 63, 74, 85, 96), na.rm = TRUE])

df$Volatility <- rowMeans(df[, c(61, 13, 24, 35, 46, 75, 68, 79, 90, 101), na.rm = TRUE])

###Only Baseline

df$Anger_baseline <-  rowMeans(df[, c(105), na.rm = TRUE])
df$Disgust_baseline <-  rowMeans(df[, c(106), na.rm = TRUE])
df$Fear_baseline <-  rowMeans(df[, c(107), na.rm = TRUE])
df$Joy_baseline <-  rowMeans(df[, c(108), na.rm = TRUE])
df$Sadness_baseline <-  rowMeans(df[, c(109), na.rm = TRUE])
df$Surprise_baseline <-  rowMeans(df[, c(110), na.rm = TRUE])


###Emotions without Baseline

df$Anger_reaction <-  rowMeans(df[, c(120, 132, 144, 156, 168, 180), na.rm = TRUE])
df$Disgust_reaction <-  rowMeans(df[, c(121, 133, 145, 157, 169, 181), na.rm = TRUE])
df$Fear_reaction <-  rowMeans(df[, c(122, 134, 146, 158, 170, 182), na.rm = TRUE])
df$Joy_reaction <-  rowMeans(df[, c(123, 135, 147, 159, 171, 183), na.rm = TRUE])
df$Sadness_reaction <-  rowMeans(df[, c(124, 136, 148, 160, 172, 184), na.rm = TRUE])
df$Surprise_reaction <-  rowMeans(df[, c(125, 137, 149, 161, 173, 185), na.rm = TRUE])


### EMOTIONS WITH BASELINE ###


df$Anger_overall <-  rowMeans(df[, c(105, 120, 132, 144, 156, 168, 180), na.rm = TRUE])
df$Disgust_overall <-  rowMeans(df[, c(106, 121, 133, 145, 157, 169, 181), na.rm = TRUE])
df$Fear_overall <-  rowMeans(df[, c(107, 122, 134, 146, 158, 170, 182), na.rm = TRUE])
df$Joy_overall <-  rowMeans(df[, c(108, 123, 135, 147, 159, 171, 183), na.rm = TRUE])
df$Sadness_overall <-  rowMeans(df[, c(109, 124, 136, 148, 160, 172, 184), na.rm = TRUE])
df$Surprise_overall <-  rowMeans(df[, c(110, 125, 137, 149, 161, 173, 185), na.rm = TRUE])


####CRT SCORES


#### SCORING THE CRT ####

#### CRT Scoring ###

### Breakdown Answers for Each CRT Item ##

table(df[, 162]) # CRT 1
table(df[, 138]) # CRT 2
table(df[, 150]) # CRT 3
table(df[, 186]) # CRT 4
table(df[, 174]) # CRT 5
table(df[, 126]) # CRT 6
table(df[, 111]) # CRT 7

### Store responses as variables to enable scoring of individual items. 

CRT1_Raw <- df[, 162]
CRT2_Raw <- df[, 138]
CRT3_Raw <- df[, 150]
CRT4_Raw <- df[, 186]
CRT5_Raw <- df[, 174]
CRT6_Raw <- df[, 126]
CRT7_Raw <- df[, 111]


### If-else scoring, identify answers that count as correct, code as 1, otherwise, code as 0. 

CRT1 <- ifelse(CRT1_Raw == "5c"| CRT1_Raw == "0.05" | CRT1_Raw == "0,05€" | CRT1_Raw == "0.05" |
                 CRT1_Raw == "0.05 Euros" | CRT1_Raw == "5 cents" | CRT1_Raw == "$0.05"
               | CRT1_Raw == "0,05" | CRT1_Raw == "0.05 euros" | CRT1_Raw == "0.05 EUROS" | CRT1_Raw == "0.5" | CRT1_Raw == "5 cent" | 
                 CRT1_Raw == "5c\\n(I have been asked this, and the lily pad one, before)", 1, 0)

#CRT2_Raw == ""|
CRT2 <- ifelse(CRT2_Raw == "5"|CRT2_Raw == "5 min"|CRT2_Raw == "5 mins"|CRT2_Raw == "5 minutes"|
                 CRT2_Raw == "5 MINUTES"|CRT2_Raw == "5 minutes because the machines stil produce 1 widget each and the number of machines have not changed"
               |CRT2_Raw == "5mins", 1, 0)

#CRT_Raw == ""|
CRT3 <- ifelse(CRT3_Raw == "47" | CRT3_Raw == "47 days" | CRT3_Raw == "47 Days" | CRT3_Raw == "47 DAYS" | 
                 CRT3_Raw == "47days" | 
                 CRT3_Raw == "47 days because it doules is size everyday so it has to cover half of the ake the day before it covers the wohle", 1, 0)

#CRT4_Raw == ""|

CRT4 <- ifelse(CRT4_Raw == "1/6+1/12 = 1/4   barrel/day  Total 4 days"| CRT4_Raw == "4"| CRT4_Raw == "4 and half days"| CRT4_Raw == "4 days"|
                 CRT4_Raw == "4 Days"| CRT4_Raw == "4 days (assuming they drink at a constant rate per day)"| CRT4_Raw == "4 days?"| CRT4_Raw == "4 days."| 
                 CRT4_Raw == "Mary half in 6 days\\nMary 1/4 in 3 days\\nJohn 1/2 in 3 days\\nJohn 4/6 in 4 days and Mary "|
                 CRT4_Raw == "within 4 days"| CRT4_Raw == "4\\n", 1, 0)

CRT5 <- ifelse(CRT5_Raw == "20"| CRT5_Raw == "20 dollars"| CRT5_Raw == "20 dollars."| CRT5_Raw == "20$"|
                 CRT5_Raw == "$20 "| CRT5_Raw == "20 $"| CRT5_Raw == "20 Dollars"| CRT5_Raw == "20\\n", 1, 0)

CRT6 <- ifelse(CRT6_Raw == "28+1=29"| CRT6_Raw == "29"| CRT6_Raw == "29 students"| CRT6_Raw == "29\\n", 1, 0)

CRT7 <- ifelse(CRT7_Raw == "c. has lost money", 1, 0)


#CREATE DATAFRAME TO PERFORM OPERATIONS ON CRT COLUMNS

CRT <- data.frame(CRT1, CRT2, CRT3, CRT4, CRT5, CRT6, CRT7)


#REMOVE ALL NA ANSWERS, CODE AS 0
CRT[is.na(CRT)] <- 0

#RENAME THE COLUMN NAMES 
names(CRT) <- c('CRT1', 'CRT2', 'CRT3', 'CRT4', 'CRT5', 'CRT6', 'CRT7')

#SCORE THE CRT_OVERALL, CRT_3, CRT_4
CRT_Overall <- CRT$CRT1 + CRT$CRT2 + CRT$CRT3 + CRT$CRT4 + CRT$CRT5 + CRT$CRT6 + CRT$CRT7
CRT_Original <- CRT$CRT1 + CRT$CRT2 + CRT$CRT3
CRT_New <- CRT$CRT4 + CRT$CRT5 + CRT$CRT6 + CRT$CRT7

##adding columns to df

describe(data)
df$CRT_Overall <- CRT_Overall
df$CRT_Original <- CRT_Original
df$CRT_New <- CRT_New

#Mean response per video

df_video_sr <- data.frame(df[, c(116, 120, 121, 122, 123, 124, 125,
                                 132, 133, 134, 135, 136, 137,
                                 144, 145, 146, 147, 148, 149, 
                                 156, 157, 158, 159, 160, 161,
                                 168, 169, 170, 171, 172, 173,
                                 180, 181, 182, 183, 184, 185)])



###Try
# Create a dataframe with the required columns
#Create a function, that for each column, value in column, that you run through and convert NA to NA, NAM to NA

df_recordings <-  select(df, TS_Audio_Recording...131, TS_Audio_Recording...143, LK_Recording, WD_Audio_Recording, HS_Audio_Recording, AN_Audio, 
                         AN_Video, AWD_Video, `HMS _Video`, LK_Video, SSL_Video, TS_Video)

na_strings <- c("NA", "NAM", "N/A")

df <- replace_with_na_at(data = df,
                         .vars = c("TS_Audio_Recording...131", "TS_Audio_Recording...143", "LK_Recording", "WD_Audio_Recording", "HS_Audio_Recording", "AN_Audio", 
                                   "AN_Video", "AWD_Video", "`HMS _Video`", "LK_Video", "SSL_Video", "TS_Video"),
                         condition = ~.x %in% na_strings)

##Create tall column, if NA exists in any of these columns, then output a column saying true

df <- df %>% 
  mutate(audio_analysis_possible = case_when(is.na(TS_Audio_Recording...131) == TRUE | is.na(TS_Audio_Recording...143) == TRUE |  is.na(WD_Audio_Recording) == TRUE | is.na(HS_Audio_Recording) == TRUE | is.na(LK_Recording) == TRUE | is.na(AN_Audio) == TRUE ~ "No",
                                             is.na(TS_Audio_Recording...131) == FALSE & is.na(TS_Audio_Recording...143) == FALSE & is.na(WD_Audio_Recording) == FALSE & is.na(HS_Audio_Recording) == FALSE & is.na(LK_Recording) == FALSE & is.na(AN_Audio) == FALSE ~ "Yes")
  )



df <- df %>% 
  mutate(video_analysis_possible = case_when(is.na(AN_Video) == TRUE | is.na(AWD_Video) == TRUE |  is.na(`HMS _Video`) == TRUE | is.na(LK_Video) == TRUE | is.na(SSL_Video) == TRUE | is.na(TS_Video) == TRUE ~ "No",
                                             is.na(AN_Video) == FALSE & is.na(AWD_Video) == FALSE & is.na(`HMS _Video`) == FALSE & is.na(LK_Video) == FALSE & is.na(SSL_Video) == FALSE & is.na(TS_Video) == FALSE ~ "Yes")
  )       



### Create new dataframe

#Change Ethnicity to Gender, native_language to education



df_clean <- dplyr::select(df, Age, Gender, Country_Birth, Nationality, Education, English_Level, Language, Publication, video_analysis_possible, audio_analysis_possible,
                          Openness_to_Experience, Openness, Intellect, 
                          Conscientiousness, Industriousness, Orderliness,
                          Extraversion, Assertiveness, Enthusiasm,
                          Agreeableness, Compassion, Politeness,
                          Neuroticism, Withdrawal, Volatility,
                          Anger_baseline:CRT_New)

#Check whether multi-modal analysis possible

df_clean <- df_clean %>% 
  mutate(multi_modal_possible = case_when(video_analysis_possible == "No" | audio_analysis_possible == "No" ~ "No",
                                          video_analysis_possible == "Yes" & audio_analysis_possible == "Yes" ~ "Yes")
  ) 


df_clean <- dplyr::select(df_clean, Age, Gender, Country_Birth, Nationality, Education, English_Level, Language, Publication, video_analysis_possible, audio_analysis_possible, multi_modal_possible,
                          Openness_to_Experience, Openness, Intellect, 
                          Conscientiousness, Industriousness, Orderliness,
                          Extraversion, Assertiveness, Enthusiasm,
                          Agreeableness, Compassion, Politeness,
                          Neuroticism, Withdrawal, Volatility,
                          Anger_baseline:CRT_New)



#RECODE Answers for publication

#"Yes, I consent to potentially having my recorded video/audio included in such publications."
#"No, I do not consent to having any of my recorded video/audio included in such publications."


df_clean$Publication <- ifelse(df_clean$Publication ==  "Yes, I consent to potentially having my recorded video/audio included in such publications.", "Yes", "No")


df_clean$Publication <- replace_na(df_clean$Publication, "No")

### PART 3 - COMPUTE DESCRIPTIVE STATISTICS 

data <- df_clean

describe(df_clean$Age)

#FREQUENCY ANALYSIS

freq_variable_list <- c(data$Gender, data$Country_Birth, data$Nationality, data$Education, data$Education, 
                        data$English_Level, data$Language, data$Publication, data$video_analysis_possible,
                        data$audio_analysis_possible, data$multi_modal_possible)

freq_Gender <- tab1(data$Gender, sort.group = "decreasing", cum.percent = TRUE)
tab1(data$Country_Birth, sort.group = "decreasing", cum.percent = TRUE)
freq_Nationality <- tab1(data$Nationality, sort.group = "decreasing", cum.percent = TRUE)
freq_Education <- tab1(data$Education, sort.group = "decreasing", cum.percent = TRUE)
freq_English <- tab1(data$English_Level, sort.group = "decreasing", cum.percent = TRUE)
freq_Language <- tab1(data$Language, sort.group = "decreasing", cum.percent = TRUE)
freq_Publication <- tab1(data$Publication, sort.group = "decreasing", cum.percent = TRUE)
freq_Video <- tab1(data$video_analysis_possible, sort.group = "decreasing", cum.percent = TRUE)
freq_Audio <- tab1(data$audio_analysis_possible, sort.group = "decreasing", cum.percent = TRUE)
freq_Multi <- tab1(data$multi_modal_possible, sort.group = "decreasing", cum.percent = TRUE)

freq_Gender


#CRONBACH ALPHA ANALYSIS 


ca_ote <- data.frame(df[, c(50, 12, 23, 34, 45, 56, 67, 78,
                            89, 100, 6, 18,
                            29, 40, 51, 62,
                            73, 84, 95, 7), na.rm = TRUE])


cronbach.alpha(ca_ote)


ca_intellect <- data.frame(df[, c(50, 12, 23, 34, 45, 56, 67, 78,
                                  89, 100), na.rm = TRUE])

cronbach.alpha(ca_intellect)


ca_Openness <- data.frame(df[, c(6, 18, 29, 40, 51, 62,
                                 73, 84, 95, 7), na.rm = TRUE])

cronbach.alpha(ca_Openness)                               

### Conscientiousness 

ca_Conscientiousness <- data.frame(df[, c(28, 10, 21, 32,
                                          43, 54, 65, 76,
                                          87, 98,
                                          83, 15,
                                          26, 37, 48, 59,
                                          70, 81, 92, 103), na.rm = TRUE])

cronbach.alpha(ca_Conscientiousness)  


ca_Industriousness <- data.frame(df[, c(28, 10, 21, 32,
                                        43, 54, 65, 76,
                                        87, 98), na.rm = TRUE])

cronbach.alpha(ca_Industriousness)  


ca_Ord <- data.frame(df[, c(
  83, 15,
  26, 37, 48, 59,
  70, 81, 92, 103), na.rm = TRUE])

cronbach.alpha(ca_Ord)  






##


ca_ext <- data.frame(df[, c(39, 11, 22, 33, 44, 55, 66, 77, 88, 99,
                            94, 16, 27, 38, 49, 60, 71, 82, 93, 104), na.rm = TRUE])

cronbach.alpha(ca_ext)  


ca_ent <- data.frame(df[, c(39, 11, 22, 33, 44, 55, 66, 77, 88, 99), na.rm = TRUE])

cronbach.alpha(ca_ent)  


ca_ast <- data.frame(df[, c(94, 16, 27, 38, 49, 60, 71, 82, 93, 104), na.rm = TRUE])

cronbach.alpha(ca_ast)  



## Agreeableness ##

ca_Agr <- data.frame(df[, c(17, 9, 20, 31, 42, 53, 64, 75, 86, 97,
                            72, 14, 25, 36, 47, 58, 69, 80, 91, 102), na.rm = TRUE])

cronbach.alpha(ca_Agr)


ca_com <- data.frame(df[, c(17, 9, 20, 31, 42, 53, 64, 75, 86, 97), na.rm = TRUE])

cronbach.alpha(ca_com)

ca_pol <- data.frame(df[, c(72, 14, 25, 36, 47, 58, 69, 80, 91, 102), na.rm = TRUE])

cronbach.alpha(ca_pol)


## Neuroticism ##


ca_neu <- data.frame(df[, c(5, 8, 19, 30, 41, 52, 63, 74, 85, 96,
                            61, 13, 24, 35, 46, 75, 68, 79, 90, 10), na.rm = TRUE])
cronbach.alpha(ca_neu)

ca_with <- data.frame(df[, c(5, 8, 19, 30, 41, 52, 63, 74, 85, 96), na.rm = TRUE])

cronbach.alpha(ca_with)

ca_vol <- data.frame(df[, c(61, 13, 24, 35, 46, 75, 68, 79, 90, 101), na.rm = TRUE])

cronbach.alpha(ca_vol)

###Only Baseline

df$Anger_baseline <-  rowMeans(df[, c(105), na.rm = TRUE])
df$Disgust_baseline <-  rowMeans(df[, c(106), na.rm = TRUE])
df$Fear_baseline <-  rowMeans(df[, c(107), na.rm = TRUE])
df$Joy_baseline <-  rowMeans(df[, c(108), na.rm = TRUE])
df$Sadness_baseline <-  rowMeans(df[, c(109), na.rm = TRUE])
df$Surprise_baseline <-  rowMeans(df[, c(110), na.rm = TRUE])


###Emotions with baseline

caa <-  data.frame(df[, c(105, 120, 132, 144, 156, 168, 180), na.rm = TRUE])
cad <-  data.frame(df[, c(106, 121, 133, 145, 157, 169, 181), na.rm = TRUE])
caf <-  data.frame(df[, c(107, 122, 134, 146, 158, 170, 182), na.rm = TRUE])
caj <-  data.frame(df[, c(108, 123, 135, 147, 159, 171, 183), na.rm = TRUE])
cas <-  data.frame(df[, c(109, 124, 136, 148, 160, 172, 184), na.rm = TRUE])
casu <-  data.frame(df[, c(110, 125, 137, 149, 161, 173, 185), na.rm = TRUE])

cronbach.alpha(caa)
cronbach.alpha(cad)
cronbach.alpha(caf)
cronbach.alpha(caj)
cronbach.alpha(cas)
cronbach.alpha(casu)


#CRT CRONBACH 

str(CRT)

CRT3_CA <- data.frame(CRT[, c("CRT1", "CRT2", "CRT3")])
CRT4_CA <- data.frame(CRT[, c("CRT4", "CRT5", "CRT6")])
CRT7_CA <- data.frame(CRT[, c("CRT1", "CRT2", "CRT3", "CRT4", "CRT5", "CRT6")])

cronbach.alpha(CRT3_CA)
cronbach.alpha(CRT4_CA)
cronbach.alpha(CRT7_CA)

#Descriptive Statistics Personality

self_report_descriptives <- describe(data)

write.csv(self_report_descriptives, "output/self_report_descriptives.csv", row.names = TRUE)

#Mean emotional response per video

sr_video_descriptives <- describe(df_video_sr)
write.csv(sr_video_descriptives, "output/sr_video_descriptives.csv", row.names = TRUE)

shapiro.test()

colnames(data)

variables <- data[, c(12:47)]


lshap <- lapply(data[, c(12:47)], shapiro.test)
lshap[[1]] ## look at the first column results
lres <- sapply(lshap, `[`, c("statistic","p.value"))

shapiro_df <- t(lres)

shapiro_df <- print(shapiro_df, digits = 2, nsmall = 2)


write.csv(shapiro_df, "output/shapiro-wilks.csv")

## Part 4 - Correlation between Self Reported Emotions and Personality Traits

df.spearman_self_report_r =data.frame(df_cor$r)
df.spearman_self_report_P =data.frame(df_cor$P)

write_xlsx(df.spearman_self_report_r, "output/correlation_matrix_self_report_R_values_v3.xlsx")
write_xlsx(df.spearman_self_report_P, "output/correlation_matrix_self_report_P_values_v3.xlsx")

#Correlation between Baseline and Self-Reported Emotions 

#Within-sample tests comparing difference between baseline and reactions

#Correlation for MultiModal Dataset

#Correlation between Self-Reported Emotions 



#Selecting dataset 

multi_data <- filter(data, multi_modal_possible == "Yes")


multi_correlation <- multi_data[, c(12:47)]

df_multi_cor <- rcorr(as.matrix(multi_correlation), type = "spearman")

df_multi_cor

### Correlation between Orderliness + TS_Disgust + SL_Disgust 

write.csv(data, "output/clean_data.csv")
write.csv(df_video_sr, "output/self-report_reactions.csv")

library(readxl)
df_emotionresponses <- read_excel("src/input_datasets/digust.xlsx")

moral_disgust <-cor.test(df_emotionresponses$Orderliness, df_emotionresponses$SL_Disgust,  method = "spearman")
moral_disgust

pathogen_disgust <-cor.test(df_emotionresponses$Orderliness, df_emotionresponses$TS_Disgust,  method = "spearman")
pathogen_disgust

sexual_disgust_orderliness <-cor.test(df_emotionresponses$Orderliness, df_emotionresponses$HS_Disgust,  method = "spearman")
sexual_disgust_orderliness


### POST HOC - Conscientiousness

moral_disgust_cons <-cor.test(df_emotionresponses$Conscientiousness, df_emotionresponses$SL_Disgust,  method = "spearman")
moral_disgust_cons

pathogen_disgust_cons <-cor.test(df_emotionresponses$Conscientiousness, df_emotionresponses$TS_Disgust,  method = "spearman")
pathogen_disgust_cons

sexual_disgust_cons <-cor.test(df_emotionresponses$Conscientiousness, df_emotionresponses$HS_Disgust,  method = "spearman")
sexual_disgust_cons


##INDUSTRIOUSNESS

moral_disgust_ind<-cor.test(df_emotionresponses$Industriousness, df_emotionresponses$SL_Disgust,  method = "spearman")
moral_disgust_ind

pathogen_disgust_ind<-cor.test(df_emotionresponses$Industriousness, df_emotionresponses$TS_Disgust,  method = "spearman")
pathogen_disgust_ind

sexual_disgust_ind<-cor.test(df_emotionresponses$Industriousness, df_emotionresponses$HS_Disgust,  method = "spearman")
sexual_disgust_ind


### POST HOC - OPENNESS TO EXPERIENCE

sexual_disgust_OTE <-cor.test(df_emotionresponses$Openness_to_Experience, df_emotionresponses$HS_Disgust,  method = "spearman")
sexual_disgust_OTE


moral_disgust_OTE <-cor.test(df_emotionresponses$Openness_to_Experience, df_emotionresponses$SL_Disgust,  method = "spearman")
moral_disgust_OTE

pathogen_disgust_OTE <-cor.test(df_emotionresponses$Openness_to_Experience, df_emotionresponses$TS_Disgust,  method = "spearman")
pathogen_disgust_OTE

## POST HOC - OPENNESS

sexual_disgust_Openness <-cor.test(df_emotionresponses$Openness, df_emotionresponses$HS_Disgust,  method = "spearman")
sexual_disgust_Openness


moral_disgust_Openness <-cor.test(df_emotionresponses$Openness, df_emotionresponses$SL_Disgust,  method = "spearman")
moral_disgust_Openness

pathogen_disgust_Openness <-cor.test(df_emotionresponses$Openness, df_emotionresponses$TS_Disgust,  method = "spearman")
pathogen_disgust_Openness

## post hoc - intellect

sexual_disgust_Intellect <-cor.test(df_emotionresponses$Intellect, df_emotionresponses$HS_Disgust,  method = "spearman")
sexual_disgust_Intellect


moral_disgust_Intellect <-cor.test(df_emotionresponses$Intellect, df_emotionresponses$SL_Disgust,  method = "spearman")
moral_disgust_Intellect

pathogen_disgust_Intellect <-cor.test(df_emotionresponses$Intellect, df_emotionresponses$TS_Disgust,  method = "spearman")
pathogen_disgust_Intellect

##### SURPRISE

cor.test(df_emotionresponses$Intellect, df_emotionresponses$WD_Surprise,  method = "spearman")

## AGREEABLENESS ## 

#Correlation Agreeableness with Emotional Responses

agreeableness_correlations <- rcorr(as.matrix(data_agreeableness), type = "spearman")

agreeableness_correlations_r =data.frame(agreeableness_correlations$r)
agreeableness_correlations_P =data.frame(agreeableness_correlations$P)

write_xlsx(agreeableness_correlations_r, "output/agreeableness_correlations_R_values.xlsx")
write_xlsx(agreeableness_correlations_P, "output/agreeableness_correlations_P_values.xlsx")


### PART 5 - MODALITIES  

###MEAN SCORES PER MODALITY PER BASIC EMOTION


### NORMALISATION + CORRELATION BETWEEN MODALITIES 


###PART 6 - Mutual Informational Analysis

#Clean up - figured out how to put spaces with column names, not sure if good idea, so will create a 
# dataset that is a copy of data. This copycat, data.2, will be used to generate MIA plots

data.2 <- dplyr::rename(data, c("Anger\nReaction" = "Anger_reaction", 
                                "Disgust\nReaction" = "Disgust_reaction",
                                "Fear\nReaction" = "Fear_reaction", 
                                "Joy\nReaction" = "Joy_reaction",
                                "Sadness\nReaction" = "Sadness_reaction", 
                                "Surprise\nReaction" = "Surprise_reaction",
                                "Anger\nBaseline" = "Anger_baseline", 
                                "Disgust\nBaseline" = "Disgust_baseline",
                                "Fear\nBaseline" = "Fear_baseline", 
                                "Joy\nBaseline" = "Joy_baseline",
                                "Sadness\nBaseline" = "Sadness_baseline", 
                                "Surprise\nBaseline" = "Surprise_baseline"))

#First run the Mutual Information Analysis 


df_Openness_to_Experience <- data.2 %>% 
  dplyr::select("Openness_to_Experience", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")


MIA_OIE <- varrank(df_Openness_to_Experience,
                   method = "peng",
                   variable.important = "Openness_to_Experience",
                   discretization.method = "sturges", 
                   algorithm = "forward",
                   scheme="mid",
                   verbose = FALSE) 


plot_mia_ope <- plot(MIA_OIE, main = "Openness to Experience", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)


df_Openness <- data.2 %>% 
  dplyr::select("Openness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")


MIA_OPEN <- varrank(df_Openness,
                    method = "peng",
                    variable.important = "Openness",
                    discretization.method = "sturges", 
                    algorithm = "forward",
                    scheme="mid",
                    verbose = FALSE) 


plot(MIA_OPEN, main = "Openness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)


df_Intellect <- data.2 %>% 
  dplyr::select("Intellect", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Intellect <- varrank(df_Intellect,
                         method = "peng",
                         variable.important = "Intellect",
                         discretization.method = "sturges", 
                         algorithm = "forward",
                         scheme="mid",
                         verbose = FALSE) 


plot(MIA_Intellect, main = "Intellect", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)

# Conscientiousness


df_Conscientiousness <- data.2 %>% 
  dplyr::select("Conscientiousness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Conscientiousness <- varrank(df_Conscientiousness,
                                 method = "peng",
                                 variable.important = "Conscientiousness",
                                 discretization.method = "sturges", 
                                 algorithm = "forward",
                                 scheme="mid",
                                 verbose = FALSE) 


plot(MIA_Conscientiousness, main = "Conscientiousness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)


#Industriousness

df_Industriousness <- data.2 %>% 
  dplyr::select("Industriousness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Industriousness <- varrank(df_Industriousness,
                               method = "peng",
                               variable.important = "Industriousness",
                               discretization.method = "sturges", 
                               algorithm = "forward",
                               scheme="mid",
                               verbose = FALSE) 


plot(MIA_Industriousness, main = "Industriousness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Orderliness

df_Orderliness <- data.2 %>% 
  dplyr::select("Orderliness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Orderliness <- varrank(df_Orderliness,
                           method = "peng",
                           variable.important = "Orderliness",
                           discretization.method = "sturges", 
                           algorithm = "forward",
                           scheme="mid",
                           verbose = FALSE) 


plot(MIA_Orderliness, main = "Orderliness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



## Agreeableness

df_Agreeableness <- data.2 %>% 
  dplyr::select("Agreeableness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Agreeableness <- varrank(df_Agreeableness,
                             method = "peng",
                             variable.important = "Agreeableness",
                             discretization.method = "sturges", 
                             algorithm = "forward",
                             scheme="mid",
                             verbose = FALSE) 


plot(MIA_Agreeableness, main = "Agreeableness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Compassion

df_Compassion <- data.2 %>% 
  dplyr::select("Compassion", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Compassion <- varrank(df_Compassion,
                          method = "peng",
                          variable.important = "Compassion",
                          discretization.method = "sturges", 
                          algorithm = "forward",
                          scheme="mid",
                          verbose = FALSE) 


plot(MIA_Compassion, main = "Compassion", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Politeness

df_Politeness <- data.2 %>% 
  dplyr::select("Politeness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Politeness <- varrank(df_Politeness,
                          method = "peng",
                          variable.important = "Politeness",
                          discretization.method = "sturges", 
                          algorithm = "forward",
                          scheme="mid",
                          verbose = FALSE) 


plot(MIA_Politeness, main = "Politeness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Extra

df_Extraversion <- data.2 %>% 
  dplyr::select("Extraversion", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Extraversion <- varrank(df_Extraversion,
                            method = "peng",
                            variable.important = "Extraversion",
                            discretization.method = "sturges", 
                            algorithm = "forward",
                            scheme="mid",
                            verbose = FALSE) 


plot(MIA_Extraversion, main = "Extraversion", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Enthusiasm 

df_Enthusiasm <- data.2 %>% 
  dplyr::select("Enthusiasm", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Enthusiasm <- varrank(df_Enthusiasm,
                          method = "peng",
                          variable.important = "Enthusiasm",
                          discretization.method = "sturges", 
                          algorithm = "forward",
                          scheme="mid",
                          verbose = FALSE) 


plot(MIA_Enthusiasm, main = "Enthusiasm", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Assertiveness

df_Assertiveness <- data.2 %>% 
  dplyr::select("Assertiveness", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Assertiveness <- varrank(df_Assertiveness,
                             method = "peng",
                             variable.important = "Assertiveness",
                             discretization.method = "sturges", 
                             algorithm = "forward",
                             scheme="mid",
                             verbose = FALSE) 


plot(MIA_Assertiveness, main = "Assertiveness", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Neuro

df_Neuroticism <- data.2 %>% 
  dplyr::select("Neuroticism", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Neuroticism <- varrank(df_Neuroticism,
                           method = "peng",
                           variable.important = "Neuroticism",
                           discretization.method = "sturges", 
                           algorithm = "forward",
                           scheme="mid",
                           verbose = FALSE) 


plot(MIA_Neuroticism, main = "Neuroticism", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#Withdrawal

df_Withdrawal <- data.2 %>% 
  dplyr::select("Withdrawal", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Withdrawal <- varrank(df_Withdrawal,
                          method = "peng",
                          variable.important = "Withdrawal",
                          discretization.method = "sturges", 
                          algorithm = "forward",
                          scheme="mid",
                          verbose = FALSE) 


plot(MIA_Withdrawal, main = "Withdrawal", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



#volatiltiy 

df_Volatility <- data.2 %>% 
  dplyr::select("Volatility", "Anger\nReaction":"Surprise\nReaction", "Anger\nBaseline":"Surprise\nBaseline")

MIA_Volatility <- varrank(df_Volatility,
                          method = "peng",
                          variable.important = "Volatility",
                          discretization.method = "sturges", 
                          algorithm = "forward",
                          scheme="mid",
                          verbose = FALSE) 


plot(MIA_Volatility, main = "Volatility", maincex = 1.25, digitcell = 2, notecex = 2.25, labelscex = 1.5)



### PART 6 Regression Modelling ###


#### Openness to Experience, Intellect, and Openness ####

#Openness to Experience: 
#Linear = NA, Nonlinear = Disgust Reaction, Disgust Baseline




OTE_lm_SRO <- lm(Openness_to_Experience ~ ., data = data)
summary(lm_Openness_to_Experience) #NO predictors
confint(lm_Openness_to_Experience)
plot(lm_Openness_to_Experience)

OTE_lm_MIA <- lm(Openness_to_Experience ~ Sadness_reaction + Surprise_baseline + Sadness_baseline, data = data)
OTE_gam_MIA <- gam(Openness_to_Experience ~ s(as.numeric(Sadness_reaction), k = 4) + s(as.numeric(Surprise_baseline), k = 4) + s(as.numeric(Sadness_baseline), k = 4), data = data)

summary(OTE_lm_MIA)
AIC(OTE_lm_MIA)
summary(OTE_gam_MIA)
anova(OTE_lm_MIA, OTE_gam_MIA, test = "Chisq")


#Nonlinear = Disgust Reaction, Disgust Baseline


OTE_lm_MIA <- lm(Openness_to_Experience ~ Disgust_reaction + Disgust_baseline, data = data)
OTE_gam_MIA <- gam(Openness_to_Experience ~ s(as.numeric(Disgust_reaction), k = 4) + s(as.numeric(Disgust_baseline), k = 4) + s(as.numeric(Sadness_baseline), k = 4), data = data)

summary(OTE_lm_MIA)
AIC(OTE_lm_MIA)
summary(OTE_lm_MIA)$r.sq

summary(OTE_gam_MIA)
AIC(OTE_gam_MIA)
summary(OTE_gam_MIA)$r.sq

anova(OTE_lm_MIA, OTE_gam_MIA, test = "Chisq")

#Intellect: 
#Linear = Surprise Reaction,


Intellect_SRO_lm<- lm(Intellect ~ Surprise_reaction, data = data)
Intellect_SRO_gam <- gam(Intellect ~ s(Surprise_reaction, k = 4), data = data)

summary(Intellect_SRO_lm) #R2 = 0.0183 2ND
AIC(Intellect_SRO_lm) 


summary(Intellect_SRO_gam)  #R2 = 0.0184 1ST
AIC(Intellect_SRO_gam)



anova(Intellect_SRO_lm, Intellect_SRO_gam, test = "Chisq")



#Nonlinear = Joy Reaction, Fear Reaction, Disgust Baseline 

Intellect_MIA_lm <- lm(Intellect ~ Joy_reaction + Fear_reaction + Disgust_baseline, data = data)

Intellect_MIA_gam <- gam(Intellect ~ s(Joy_reaction, k = 4) + s(Fear_reaction, k = 4) + s(Disgust_baseline, k = 4), data = data)


summary(Intellect_MIA_lm) #R2 = 0.001 4TH
AIC(Intellect_MIA_lm)  



AIC(Intellect_MIA_gam) #R2 = 0.0176 3RD
summary(Intellect_MIA_gam) 

anova(Intellect_MIA_lm, Intellect_MIA_gam, test = "Chisq")

#Openness: 
#Linear = Sadness Baseline, 


Openness_SRO_lm <- lm(Openness ~ Sadness_baseline, data = data)
Openness_SRO_gam <- gam(Openness ~ s(Sadness_baseline, k = 4), data = data)

summary(Openness_SRO_lm) #
AIC(Openness_SRO_lm)


AIC(Openness_SRO_gam)
summary(Openness_SRO_gam) #


anova(Openness_SRO_lm, Openness_SRO_gam, test = "Chisq")


#Nonlinear = Sadness Reaction, Surprise Baseline, Sadness Baseline

Openness_MIA_lm <- lm(Openness ~ Sadness_reaction + Surprise_baseline + Sadness_baseline, data = data)
Openness_MIA_gam <- gam(Openness ~ s(Sadness_reaction, k = 4)+ s(Surprise_baseline, k = 4) + s(Sadness_baseline, k = 4), data = data)

summary(Openness_MIA_lm)
AIC(Openness_MIA_lm)


summary(Openness_MIA_gam)
AIC(Openness_MIA_gam)


anova(Openness_MIA_lm, Openness_MIA_gam, test = "Chisq")


#Conscientiousness: 
#Linear = Sadness Baseline, Joy Reaction, Disgust Baseline, Fear Baseline, Anger Baseline, 

Conscientiousness_SRO_lm <- lm(Conscientiousness ~ Sadness_baseline + Disgust_baseline + Fear_baseline, data = data)
Conscientiousness_SRO_gam <- gam(Conscientiousness ~ s(Sadness_baseline, k = 4)+ s(Disgust_baseline, k = 4) + s(Fear_baseline, k = 4), data = data)

summary(Conscientiousness_SRO_lm)
AIC(Conscientiousness_SRO_lm)


summary(Conscientiousness_SRO_gam)
AIC(Conscientiousness_SRO_gam)


anova(Conscientiousness_SRO_lm, Conscientiousness_SRO_gam, test = "Chisq")


#Nonlinear = Sadness Reaction, Fear Baseline, Joy Reaction

Conscientiousness_MIA_lm <- lm(Conscientiousness ~ Sadness_reaction + Fear_baseline + Joy_reaction, data = data)
Conscientiousness_MIA_gam <- gam(Conscientiousness ~ s(Sadness_reaction, k = 4)+ s(Fear_baseline, k = 4) + s(Joy_reaction, k = 4), data = data)

summary(Conscientiousness_MIA_lm) #0.059
AIC(Conscientiousness_MIA_lm)


summary(Conscientiousness_MIA_gam) #0.06
AIC(Conscientiousness_MIA_gam)


anova(Conscientiousness_MIA_lm, Conscientiousness_MIA_gam, test = "Chisq")

#Industriousness: 
#Linear = Sadness Baseline, Fear Baseline, Anger Baseline, Disgust Baseline, Joy Reaction, 


Industriousness_SRO_lm <- lm(Industriousness ~ Sadness_baseline + Fear_baseline + Anger_baseline, data = data)
Industriousness_SRO_gam <- gam(Industriousness ~ s(Sadness_baseline, k = 4)+ s(Fear_baseline, k = 4) + s(Anger_baseline, k = 4), data = data)

summary(Industriousness_SRO_lm)
AIC(Industriousness_SRO_lm)


summary(Industriousness_SRO_gam)
AIC(Industriousness_SRO_gam)


anova(Industriousness_SRO_lm, Industriousness_SRO_gam, test = "Chisq")


#Nonlinear = Fear Reaction, Joy Reaction, Disgust Baseline


Industriousness_MIA_lm <- lm(Industriousness ~ Fear_reaction + Joy_reaction + Disgust_baseline, data = data)
Industriousness_MIA_gam <- gam(Industriousness ~ s(Fear_reaction, k = 4)+ s(Joy_reaction, k = 4) + s(Disgust_baseline, k = 4), data = data)

summary(Industriousness_MIA_lm)
AIC(Industriousness_MIA_lm)

summary(Industriousness_MIA_gam)
AIC(Industriousness_MIA_gam)

anova(Industriousness_MIA_lm, Industriousness_MIA_gam, test = "Chisq")


#Orderliness: 
#Linear = Joy Reaction, Sadness Baseline, 

Orderliness_SRO_lm <- lm(Orderliness ~ Joy_reaction + Sadness_baseline, data = data)
Orderliness_SRO_gam <- gam(Orderliness ~ s(Joy_reaction, k = 4)+ s(Sadness_baseline, k = 4), data = data)

summary(Orderliness_SRO_lm)
AIC(Orderliness_SRO_lm)

summary(Orderliness_SRO_gam)
AIC(Orderliness_SRO_gam)


anova(Orderliness_SRO_lm, Orderliness_SRO_gam, test = "Chisq")



#Nonlinear = Anger Reaction, Joy Reaction, Sadness Baseline


Orderliness_MIA_lm <- lm(Orderliness ~ Anger_reaction + Joy_reaction + Sadness_baseline, data = data)
Orderliness_MIA_gam <- gam(Orderliness ~ s(Anger_reaction, k = 4)+ s(Joy_reaction, k = 4) + s(Sadness_baseline, k = 4), data = data)

AIC(Orderliness_MIA_lm)
summary(Orderliness_MIA_lm)


AIC(Orderliness_MIA_gam)
summary(Orderliness_MIA_gam)

anova(Orderliness_MIA_lm, Orderliness_MIA_gam, test = "Chisq")


#Extraversion: 
#Linear = Joy Baseline, Joy Reaction, 

Extraversion_SRO_lm <- lm(Extraversion ~ Joy_baseline + Joy_reaction, data = data)
Extraversion_SRO_gam <- gam(Extraversion ~ s(Joy_baseline, k = 4)+ s(Joy_reaction, k = 4), data = data)

summary(Extraversion_SRO_lm) #0.0783
AIC(Extraversion_SRO_lm)


summary(Extraversion_SRO_gam) #0.0783
AIC(Extraversion_SRO_gam)


anova(Extraversion_SRO_lm, Extraversion_SRO_gam, test = "Chisq")





#Nonlinear = Surprise Reaction, Joy Baseline, Sadness Baseline

Extraversion_MIA_lm <- lm(Extraversion ~ Surprise_reaction + Joy_baseline + Sadness_baseline, data = data)
Extraversion_MIA_gam <- gam(Extraversion ~ s(Surprise_reaction, k = 4)+ s(Joy_baseline, k = 4) + s(Sadness_baseline, k = 4), data = data)

summary(Extraversion_MIA_lm) #0.069
AIC(Extraversion_MIA_lm)


summary(Extraversion_MIA_gam) #0.075
AIC(Extraversion_MIA_gam)

anova(Extraversion_MIA_lm, Extraversion_MIA_gam, test = "Chisq")


#Assertiveness: 
#Linear = Fear Baseline, Joy Reaction, 

Assertiveness_SRO_lm <- lm(Assertiveness ~ Fear_baseline + Joy_reaction, data = data)
Assertiveness_SRO_gam <- gam(Assertiveness ~ s(Fear_baseline, k = 4)+ s(Joy_reaction, k = 4), data = data)

summary(Assertiveness_SRO_lm)
AIC(Assertiveness_SRO_lm)


summary(Assertiveness_SRO_gam)
AIC(Assertiveness_SRO_gam)

anova(Assertiveness_SRO_lm, Assertiveness_SRO_gam, test = "Chisq")




#Nonlinear = Anger Reaction, Disgust Baseline, Joy Baseline

Assertiveness_MIA_lm <- lm(Assertiveness ~ Anger_reaction + Disgust_baseline + Joy_baseline, data = data)
Assertiveness_MIA_gam <- gam(Assertiveness ~ s(Anger_reaction, k = 4)+ s(Disgust_baseline, k = 4) + s(Joy_baseline, k = 4), data = data)

summary(Assertiveness_MIA_lm)
AIC(Assertiveness_MIA_lm)


summary(Assertiveness_MIA_gam)
AIC(Assertiveness_MIA_gam)

anova(Assertiveness_MIA_lm, Assertiveness_MIA_gam, test = "Chisq")


#Enthusiasm: 
#Linear = Joy Baseline+ Fear Reaction+Sadness Reaction = Anger Reaction = Disgust Reaction  

Enthusiasm_SRO_lm <- lm(Enthusiasm ~ Joy_baseline + Fear_reaction + Sadness_reaction, data = data)
Enthusiasm_SRO_gam <- gam(Enthusiasm ~ s(Joy_baseline, k = 4)+ s(Fear_reaction, k = 4) + s(Sadness_reaction, k = 4), data = data)

summary(Enthusiasm_SRO_lm) #0.10
AIC(Enthusiasm_SRO_lm) #381.907


summary(Enthusiasm_SRO_gam) #0.121  
AIC(Enthusiasm_SRO_gam) #379.54

anova(Enthusiasm_SRO_lm, Enthusiasm_SRO_gam, test = "Chisq")




#Nonlinear = Disgust Reaction, Surprise Baseline, Sadness Baseline


Enthusiasm_MIA_lm <- lm(Enthusiasm ~ Disgust_reaction + Surprise_baseline + Sadness_baseline, data = data)
Enthusiasm_MIA_gam <- gam(Enthusiasm ~ s(Disgust_reaction, k = 4)+ s(Surprise_baseline, k = 4) + s(Sadness_baseline, k = 4), data = data)

summary(Enthusiasm_MIA_lm) #0.01731 
AIC(Enthusiasm_MIA_lm) #400.7508


summary(Enthusiasm_MIA_gam) #0.0173
AIC(Enthusiasm_MIA_gam) #400.7508

anova(Enthusiasm_MIA_lm, Enthusiasm_MIA_gam, test = "Chisq")

#Agreeableness: 
#Linear = Fear Reaction + Sadness Reaction + Disgust Reaction, 

Agreeableness_SRO_lm <- lm(Agreeableness ~ Fear_reaction + Sadness_reaction + Disgust_reaction, data = data)
Agreeableness_SRO_gam <- gam(Agreeableness ~ s(Fear_reaction, k = 4)+ s(Sadness_reaction, k = 4) + s(Disgust_reaction, k = 4), data = data)

summary(Agreeableness_SRO_lm) #0.07005
AIC(Agreeableness_SRO_lm)


summary(Agreeableness_SRO_gam)
AIC(Agreeableness_SRO_gam)

anova(Agreeableness_SRO_lm, Agreeableness_SRO_gam, test = "Chisq")


#Nonlinear = Surprise reaction + Disgust Baseline + Joy Baseline

Agreeableness_MIA_lm <- lm(Agreeableness ~ Surprise_reaction + Disgust_baseline + Joy_baseline, data = data)
Agreeableness_MIA_gam <- gam(Agreeableness ~ s(Surprise_reaction, k = 4)+ s(Disgust_baseline, k = 4) + s(Joy_baseline, k = 4), data = data)

summary(Agreeableness_MIA_lm) #0.07105
AIC(Agreeableness_MIA_lm)


summary(Agreeableness_MIA_gam) #0.105
AIC(Agreeableness_MIA_gam)
 
anova(Agreeableness_MIA_lm, Agreeableness_MIA_gam, test = "Chisq")



#Compassion: 
#Linear = Fear Reaction + Sadness Reaction + Disgust Reaction, 

Compassion_SRO_lm <- lm(Compassion ~ Fear_reaction + Sadness_reaction + Disgust_reaction, data = data)
Compassion_SRO_gam <- gam(Compassion ~ s(Fear_reaction, k = 4)+ s(Sadness_reaction, k = 4) + s(Disgust_reaction), data = data)

summary(Compassion_SRO_lm) #0.1071
AIC(Compassion_SRO_lm)


summary(Compassion_SRO_gam) #0.198 
AIC(Compassion_SRO_gam)

anova(Compassion_SRO_lm, Compassion_SRO_gam, test = "Chisq")




#Nonlinear = Sadness Reaction + Surprise Baseline + Joy Reaction

Compassion_MIA_lm <- lm(Compassion ~ Sadness_reaction + Surprise_baseline + Joy_baseline, data = data)
Compassion_MIA_gam <- gam(Compassion ~ s(Sadness_reaction, k = 4)+ s(Surprise_baseline, k = 4) + s(Joy_baseline, k = 4), data = data)

summary(Compassion_MIA_lm)
AIC(Compassion_MIA_lm)


summary(Compassion_MIA_gam)
AIC(Compassion_MIA_gam)

anova(Compassion_MIA_lm, Compassion_MIA_gam, test = "Chisq")



#Politeness: 
#Linear = Anger Baseline + Disgust Baseline + Joy Reaction, 

Politeness_SRO_lm <- lm(Politeness ~ Anger_baseline + Disgust_baseline + Joy_reaction, data = data)
Politeness_SRO_gam <- gam(Politeness ~ s(Anger_baseline, k = 4) + s(Disgust_baseline, k = 4) + s(Joy_reaction, k = 4), data = data)

summary(Politeness_SRO_lm) #0.08007
AIC(Politeness_SRO_lm) #332.7053

 
summary(Politeness_SRO_gam) #0.0947
AIC(Politeness_SRO_gam) #330.2346

anova(Politeness_SRO_lm, Politeness_SRO_gam, test = "Chisq")


#Nonlinear = Surprise Reaction + Disgust Baseline + Fear Baseline

Politeness_MIA_lm <- lm(Politeness ~ Surprise_reaction + Disgust_baseline + Fear_reaction, data = data)
Politeness_MIA_gam <- gam(Politeness ~ s(Surprise_reaction, k = 4) + s(Disgust_baseline, k = 4) + s(Fear_reaction, k = 4), data = data)

summary(Politeness_MIA_lm) #0.06168
AIC(Politeness_MIA_lm) #336.7223


summary(Politeness_MIA_gam) #0.0891
AIC(Politeness_MIA_gam) #332.383

anova(Politeness_MIA_lm, Politeness_MIA_gam, test = "Chisq")


#Neuroticism: 
#Linear = Fear Baseline + Sadness Baseline + Anger Baseline = Fear Reaction, Sadness Reaction + Surprise Reaction, 

Neuroticism_SRO_lm <- lm(Neuroticism ~ Fear_baseline + Sadness_baseline + Fear_reaction, data = data)
Neuroticism_SRO_gam <- gam(Neuroticism ~ s(Fear_baseline, k = 4)+ s(Sadness_baseline, k = 4) + s(Fear_reaction, k = 4), data = data)

summary(Neuroticism_SRO_lm) #0.1521
AIC(Neuroticism_SRO_lm)


summary(Neuroticism_SRO_gam) #0.153
AIC(Neuroticism_SRO_gam)

anova(Neuroticism_SRO_lm, Neuroticism_SRO_gam, test = "Chisq")


#Nonlinear = Fear Reaction + Joy Reaction + Anger Baseline

Neuroticism_MIA_lm <- lm(Neuroticism ~ Fear_reaction + Joy_reaction + Anger_baseline, data = data)
Neuroticism_MIA_gam <- gam(Neuroticism ~ s(Fear_reaction, k = 4)+ s(Joy_reaction, k = 4) + s(Anger_baseline, k = 4), data = data)

summary(Neuroticism_MIA_lm) # 0.1069 
AIC(Neuroticism_MIA_lm)


summary(Neuroticism_MIA_gam) #0.117 
AIC(Neuroticism_MIA_gam)

anova(Neuroticism_MIA_lm, Neuroticism_MIA_gam, test = "Chisq")

#Volatility: 
#Linear = Anger Baseline + Sadness Baseline + Fear Reaction + Fear Baseline = Sadness Reaction, 


Volatility_SRO_lm <- lm(Volatility ~ Anger_baseline + Sadness_baseline + Fear_reaction, data = data)
Volatility_SRO_gam <- gam(Volatility ~ s(Anger_baseline, k = 4)+ s(Sadness_baseline, k = 4) + s(Fear_reaction, k = 4), data = data)

summary(Volatility_SRO_lm) # 0.1064 
AIC(Volatility_SRO_lm)


summary(Volatility_SRO_gam) #0.106
AIC(Volatility_SRO_gam)

anova(Volatility_SRO_lm, Volatility_SRO_gam, test = "Chisq")



#Nonlinear = Fear Reaction + Joy Reaction + Anger Baseline

Volatility_MIA_lm <- lm(Volatility ~ Fear_reaction + Joy_reaction + Anger_baseline, data = data)
Volatility_MIA_gam <- gam(Volatility ~ s(Fear_reaction, k = 4)+ s(Joy_reaction, k = 4) + s(Anger_baseline, k = 4), data = data)

summary(Volatility_MIA_lm) #0.1098,
AIC(Volatility_MIA_lm)


summary(Volatility_MIA_gam) #0.102
AIC(Volatility_MIA_gam)

anova(Volatility_MIA_lm, Volatility_MIA_lm, test = "Chisq")





#Withdrawal: 
#Linear = Fear Baseline + Sadness Baseline + Anger Baseline + Fear Reaction + Sadness Reaction + Surprise Reaction, 

Withdrawal_SRO_lm <- lm(Withdrawal ~ Fear_baseline + Sadness_baseline + Anger_baseline, data = data)
Withdrawal_SRO_gam <- gam(Withdrawal ~ s(Fear_baseline, k = 4)+ s(Sadness_baseline, k = 4) + s(Anger_baseline, k = 4), data = data)

summary(Withdrawal_SRO_lm) #0.1798 
AIC(Withdrawal_SRO_lm)


summary(Withdrawal_SRO_gam) #0.184
AIC(Withdrawal_SRO_gam)

anova(Withdrawal_SRO_lm, Withdrawal_SRO_gam, test = "Chisq")


#Nonlinear = Surprise Reaction + Fear Baseline + Joy Baseline

Withdrawal_MIA_lm <- lm(Withdrawal ~ Surprise_reaction + Fear_baseline + Joy_baseline, data = data)
Withdrawal_MIA_gam <- gam(Withdrawal ~ s(Surprise_reaction, k = 4)+ s(Fear_baseline, k = 4) + s(Joy_baseline, k = 4), data = data)

summary(Withdrawal_MIA_lm) #0.1855 
AIC(Withdrawal_MIA_lm)



summary(Withdrawal_MIA_gam) #0.189
AIC(Withdrawal_MIA_gam)

anova(Withdrawal_MIA_lm, Withdrawal_MIA_gam, test = "Chisq")


### PLOTS ##

# linear = S
# nonlinear = NL

NL1 <- ggplot(data, aes(x = Disgust_reaction, y = Openness_to_Experience)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Reaction)', y = "Openness to Experience", title = "Openness to Experience ~ Disgust (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL2 <- ggplot(data, aes(x = Disgust_baseline, y = Openness_to_Experience)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Openness to Experience", title = "Openness to Experience ~ Disgust (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))
  

NL3 <- ggplot(data, aes(x = Joy_reaction, y = Openness_to_Experience)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Openness to Experience", title = "Openness to Experience ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S1 <- ggplot(data, aes(x = Surprise_reaction, y = Intellect)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Reaction)', y = "Intellect", title = "Intellect ~ Surprise (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL4 <- ggplot(data, aes(x = Joy_reaction, y = Intellect)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Intellect", title = "Intellect ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL5 <- ggplot(data, aes(x = Fear_reaction, y = Intellect)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Intellect", title = "Intellect ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL6 <- ggplot(data, aes(x = Disgust_baseline, y = Intellect)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Intellect", title = "Intellect ~ Disgust (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S2 <- ggplot(data, aes(x = Sadness_baseline, y = Openness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Openness", title = "Openness ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL7 <- ggplot(data, aes(x = Sadness_reaction, y = Openness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Reaction)', y = "Openness", title = "Openness ~ Sadness (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL8 <- ggplot(data, aes(x = Surprise_baseline, y = Openness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Baseline)', y = "Openness", title = "Openness ~ Surprise (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))



S3 <- ggplot(data, aes(x = Sadness_baseline, y = Conscientiousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Conscientiousness", title = "Conscientiousness ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S4 <- ggplot(data, aes(x = Joy_reaction, y = Conscientiousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Conscientiousness", title = "Conscientiousness ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S5 <- ggplot(data, aes(x = Joy_reaction, y = Conscientiousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Conscientiousness", title = "Conscientiousness ~ Disgust (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL9 <- ggplot(data, aes(x = Sadness_reaction, y = Conscientiousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Reaction)', y = "Conscientiousness", title = "Conscientiousness ~ Sadness (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL10 <- ggplot(data, aes(x = Fear_reaction, y = Conscientiousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Conscientiousness", title = "Conscientiousness ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))


S6 <- ggplot(data, aes(x = Sadness_baseline, y = Industriousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Industriousness", title = "Industriousness ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S7 <- ggplot(data, aes(x = Fear_reaction, y = Industriousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Industriousness", title = "Industriousness ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S8 <- ggplot(data, aes(x = Anger_baseline, y = Industriousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Baseline)', y = "Industriousness", title = "Industriousness ~ Anger (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL11 <- ggplot(data, aes(x = Fear_reaction, y = Industriousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Industriousness", title = "Industriousness ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL12 <- ggplot(data, aes(x = Joy_reaction, y = Industriousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Industriousness", title = "Industriousness ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL13 <- ggplot(data, aes(x = Disgust_baseline, y = Industriousness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Industriousness", title = "Industriousness ~ Joy (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S9 <- ggplot(data, aes(x = Joy_baseline, y = Extraversion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Baseline)', y = "Extraversion", title = "Extraversion ~ Joy (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S9_a <- ggplot(data, aes(x = Joy_reaction, y = Extraversion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Extraversion", title = "Extraversion ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S10 <- ggplot(data, aes(x = Disgust_reaction, y = Extraversion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Reaction)', y = "Extraversion", title = "Extraversion ~ Disgust (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL14 <- ggplot(data, aes(x = Surprise_baseline, y = Extraversion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Baseline)', y = "Extraversion", title = "Extraversion ~ Surprise (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL15 <- ggplot(data, aes(x = Sadness_baseline, y = Extraversion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Extraversion", title = "Extraversion ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S11 <- ggplot(data, aes(x = Fear_baseline, y = Assertiveness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Baseline)', y = "Assertiveness", title = "Assertiveness ~ Fear (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S11_b <- ggplot(data, aes(x = Joy_reaction, y = Assertiveness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Assertiveness", title = "Assertiveness ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))


NL16 <- ggplot(data, aes(x = Anger_reaction, y = Assertiveness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Reaction)', y = "Assertiveness", title = "Assertiveness ~ Anger (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))
  
NL17 <- ggplot(data, aes(x = Disgust_baseline, y = Assertiveness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Assertiveness", title = "Assertiveness ~ Disgust (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL18 <- ggplot(data, aes(x = Joy_baseline, y = Assertiveness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Baseline)', y = "Assertiveness", title = "Assertiveness ~ Joy (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S12 <- ggplot(data, aes(x = Joy_baseline, y = Enthusiasm)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Baseline)', y = "Enthusiasm", title = "Enthusiasm ~ Joy (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))


S13 <- ggplot(data, aes(x = Fear_reaction, y = Enthusiasm)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Enthusiasm", title = "Enthusiasm ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S14 <- ggplot(data, aes(x = Sadness_reaction, y = Enthusiasm)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Reaction)', y = "Enthusiasm", title = "Enthusiasm ~ Sadness (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL19 <- ggplot(data, aes(x = Disgust_reaction, y = Enthusiasm)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Reaction)', y = "Enthusiasm", title = "Enthusiasm ~ Disgust (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL20 <- ggplot(data, aes(x = Surprise_baseline, y = Enthusiasm)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Baseline)', y = "Enthusiasm", title = "Enthusiasm ~ Surprise (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL21 <- ggplot(data, aes(x = Sadness_baseline, y = Enthusiasm)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Enthusiasm", title = "Enthusiasm ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S15 <- ggplot(data, aes(x = Fear_reaction, y = Agreeableness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Agreeableness", title = "Agreeableness ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S16 <- ggplot(data, aes(x = Sadness_reaction, y = Agreeableness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Reaction)', y = "Agreeableness", title = "Agreeableness ~ Sadness (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S17 <- ggplot(data, aes(x = Disgust_reaction, y = Agreeableness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Reaction)', y = "Agreeableness", title = "Agreeableness ~ Disgust (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL22 <- ggplot(data, aes(x = Surprise_reaction, y = Agreeableness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Reaction)', y = "Agreeableness", title = "Agreeableness ~ Surprise (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL23 <- ggplot(data, aes(x = Disgust_baseline, y = Agreeableness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Agreeableness", title = "Agreeableness ~ Disgust (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL24 <- ggplot(data, aes(x = Joy_baseline, y = Agreeableness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Baseline)', y = "Agreeableness", title = "Agreeableness ~ Joy (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))



S18 <- ggplot(data, aes(x = Fear_reaction, y = Compassion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Compassion", title = "Compassion ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))


S19 <- ggplot(data, aes(x = Sadness_reaction, y = Compassion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Reaction)', y = "Compassion", title = "Compassion ~ Sadness (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S20 <- ggplot(data, aes(x = Disgust_reaction, y = Compassion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Reaction)', y = "Compassion", title = "Compassion ~ Disgust (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL25 <- ggplot(data, aes(x = Surprise_baseline, y = Compassion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Baseline)', y = "Compassion", title = "Compassion ~ Surprise (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL26 <- ggplot(data, aes(x = Joy_reaction, y = Compassion)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Compassion", title = "Compassion ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))



S21 <- ggplot(data, aes(x = Anger_baseline, y = Politeness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Baseline)', y = "Politeness", title = "Politeness ~ Anger (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S22 <- ggplot(data, aes(x = Disgust_baseline, y = Politeness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Disgust (Baseline)', y = "Politeness", title = "Politeness ~ Disgust (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S23 <- ggplot(data, aes(x = Joy_reaction, y = Politeness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Politeness", title = "Politeness ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL27 <- ggplot(data, aes(x = Anger_baseline, y = Politeness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Baseline)', y = "Politeness", title = "Politeness ~ Anger (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL28 <- ggplot(data, aes(x = Fear_baseline, y = Politeness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Baseline)', y = "Politeness", title = "Politeness ~ Fear (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))


S24 <- ggplot(data, aes(x = Fear_baseline, y = Neuroticism)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Baseline)', y = "Neuroticism", title = "Neuroticism ~ Fear (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S25 <- ggplot(data, aes(x = Sadness_baseline, y = Neuroticism)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Neuroticism", title = "Neuroticism ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S26 <- ggplot(data, aes(x = Fear_reaction, y = Neuroticism)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Reaction)', y = "Neuroticism", title = "Neuroticism ~ Fear (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL29 <- ggplot(data, aes(x = Fear_baseline, y = Neuroticism)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Baseline)', y = "Neuroticism", title = "Neuroticism ~ Fear (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL30 <- ggplot(data, aes(x = Sadness_baseline, y = Neuroticism)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Neuroticism", title = "Neuroticism ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))



S27 <- ggplot(data, aes(x = Anger_baseline, y = Volatility)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Baseline)', y = "Volatility", title = "Volatility ~ Anger (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S28 <- ggplot(data, aes(x = Sadness_baseline, y = Volatility)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Volatility", title = "Volatility ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S29 <- ggplot(data, aes(x = Fear_baseline, y = Volatility)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Baseline)', y = "Volatility", title = "Volatility ~ Fear (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL31 <- ggplot(data, aes(x = Joy_reaction, y = Volatility)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Volatility", title = "Volatility ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

S30 <- ggplot(data, aes(x = Fear_baseline, y = Withdrawal)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Fear (Baseline)', y = "Withdrawal", title = "Withdrawal ~ Fear (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))


S31 <- ggplot(data, aes(x = Sadness_baseline, y = Withdrawal)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Withdrawal", title = "Withdrawal ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))



S32 <- ggplot(data, aes(x = Anger_baseline, y = Withdrawal)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Baseline)', y = "Withdrawal", title = "Withdrawal ~ Anger (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red"))

NL32 <- ggplot(data, aes(x = Surprise_reaction, y = Withdrawal)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Surprise (Reaction)', y = "Withdrawal", title = "Withdrawal ~ Surprise (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red")) 
NL33 <- ggplot(data, aes(x = Joy_baseline, y = Withdrawal)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Baseline)', y = "Withdrawal", title = "Withdrawal ~ Joy (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red")) 

S33 <- ggplot(data, aes(x = Joy_reaction, y = Orderliness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Joy (Reaction)', y = "Orderliness", title = "Orderliness ~ Joy (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red")) 

S34 <- ggplot(data, aes(x = Sadness_baseline, y = Orderliness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Sadness (Baseline)', y = "Orderliness", title = "Orderliness ~ Sadness (Baseline)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red")) 

NL34 <- ggplot(data, aes(x = Anger_reaction, y = Orderliness)) + 
  geom_point() +
  xlim(1, 5) + 
  ylim(1, 5) +
  labs(x = 'Anger (Reaction)', y = "Orderliness", title = "Orderliness ~ Anger (Reaction)") +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 4), aes(colour = "GAM"), se = FALSE, linetype="dotdash") + theme_cowplot(16) + 
  geom_smooth(method = "lm", aes(colour = "LM"), se = FALSE) + theme_cowplot(16) +
  scale_color_manual(name = "Model Fit",
                     breaks = c("GAM", "LM"),
                     values = c("GAM" = "Navy", "LM" = "Red")) 



#test1
plot_grid(NL1, NL2, NL3, S1, NL4, NL5, NL6, S2, NL7, 
          ncol = 3,
          labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
          label_size = 18)

#
plot_grid(NL8, S3, S4, S5, NL9, NL10, S6, S7, S8, 
          ncol = 3,
          labels = c("10", "11", "12", "13", "14", "15", "16", "17", "18"),
          label_size = 18)

#
plot_grid(NL11, NL12, NL13, S33, S34, NL34, S9, S10, S11, 
          ncol = 3,
          labels = c("19", "20", "21", "22", "23", "24", "25", "26", "27"),
          label_size = 18)

#
plot_grid(S11_b, NL16, NL17, NL18, S12, S13, S14, NL19, NL20,
          ncol = 3,
          labels = c("28", "29", "30", "31", "32", "33", "34", "35", "36"),
          label_size = 18)

#
plot_grid(S15, S16, S17, NL22, NL23, NL24, S18, S19, S20, 
          ncol = 3,
          labels = c("37", "38", "39", "40", "41", "42", "43", "44", "45"),
          label_size = 18)

#
plot_grid(NL25, NL26, S21, S22, S23, NL27, NL28, S24, S25, 
          ncol = 3,
          labels = c("46", "47", "48", "49", "50", "51", "52", "53", "54"),
          label_size = 18)

#
plot_grid(NL29, NL30, S27, S28, S29, NL31, S30, S31, S32, NL32, NL33, 
          ncol = 3,
          labels = c("55", "56", "57", "58", "59", "60", "61", "62", "63", "64", "65"),
          label_size = 18)

