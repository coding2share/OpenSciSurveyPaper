# Prolog 

# Project Name: coding2share
# Purpose: Increase the use of reproducible research practices
# Data: 
# Code: 
# Authors: Jenine K. Harris^1^, Kim Johnson^1^, Bobbi Carothers^1,2^, Todd Combs^1,2^, Doug A. Luke^1,2^, Xiaoyan Wang^1,2^
# Affiliations: ^1^Brown School, Washington University in St. Louis
#               ^2^Center for Public Health Systems Science, Brown School, Washington University in St. Louis

#### Corresponding author

#Jenine K. Harris  
#Brown School  
#Washington University in St. Louis  
#One Brookings Drive  
#St. Louis, MO 63130  
#314-935-3522 (phone)  
#harrisj@wustl.edu  

library(haven)
library(tidyverse)
library(magrittr)
library(labelled)
library(stringr)
library(knitr)
library(kableExtra)
library(magick)
library(reshape2)
library(scales)

# Read data file
r <- read_spss(file="G:/CPHSS/OpenScience/WriteUps/aim1SurveyPaper/OpenScienceAim1.sav")

fills <- c("Yes" = "#1f78b4", "No" = "#a6cee3")

# for each of the 8 demographics/characteristics, 
# remove NAs as to have maximum reporting
# clean and add labels

#gender
gender <- r %>%
  select(Q43) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  group_by(Q43) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  rename(key=Q43) %>%
  mutate(key= factor(key, levels=1:4, 
                     labels=c("Male", "Female",  
                              "Transgender", "Other"))) %>%
  arrange(pc) %>%
  mutate(cat = "Gender", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)

#race/ethnicity
race <- r %>%
  select(Q44_1:Q44_8) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  mutate(tot= rowSums(.))

#recode two or more races + others to other
race <- race %>%
  mutate_at(vars(Q44_1:Q44_8),funs(replace(.,tot>1,0))) %>%
  mutate(mr = ifelse(tot>1,1,0)) %>%
  mutate(oth = ifelse(rowSums(.[5:8])>0,1,0)) %>%
  select(-(Q44_5:Q44_8), -tot)

raceLabs <- r %>%
  select(Q44_1:Q44_4) %$%
  var_label(.)  

mynames <- c("White", "Hispanic", "Black/African-American", "Asian", 
             "Two or more", "Other")
names(race) <- mynames

raceTot <- race %>%
  summarise_all(sum) %>%
  gather(value=n)

race2 <- race %>%
  summarise_all(.funs=function(x) round(sum(x)/length(x)*100,1)) %>%
  gather(value=pc) %>%
  left_join(raceTot) %>%
  arrange(pc) %>%
  select(key, n, pc) %>%
  mutate(cat = "Race/ethnicity", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)


#age
age <- r %>%
  select(Q45) %>%
  na.omit() %>%
  mutate(key = ifelse(Q45 %in% 20:35, "20-35",
                      ifelse(Q45 %in% 36:50, "36-50",
                             ifelse(Q45 %in% 51:65, "51-65",
                                    ifelse(Q45>65, "66+", NA))))) %>%
  select(-Q45) %>%
  group_by(key) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  mutate(cat = "Age", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)


#degree
r <- r %>%
  mutate(deg = "PhD") %>%
  mutate(deg = ifelse(Q38_TEXT== "MA, MPH" | Q38==7, "MPH", deg)) %>%
  mutate(deg = ifelse(Q38_TEXT=="I have an MD degree and an MS in statistics"
                      | Q38==2, "MD",deg)) %>%
  mutate(deg = ifelse(Q38_TEXT %in% c("Master in Public Administration (MPA)", "MPA"),
                      "MPA", deg)) %>%
  mutate(deg = ifelse(Q38_TEXT=="Currently completing BS", "None", deg)) %>%
  mutate(deg = ifelse(is.na(Q38), NA, deg)) %>%
  mutate(deg = ifelse(Q38 ==9, "MS", deg)) %>%
  mutate(deg = ifelse(Q38==11, "MA", deg)) %>%
  mutate(deg = ifelse(Q38==3, "MD/PhD", deg)) %>%
  mutate(deg = ifelse(Q38==4, "DrPH", deg)) %>%
  mutate(deg = ifelse(Q38==5, "DSc", deg)) %>%
  mutate(deg = ifelse(Q38==12, "MBA", deg)) %>%
  mutate(deg = ifelse(Q38 %in% 14:15, "BS/BA", deg))

degree <- r %>% 
  select(deg) %>%
  na.omit() %>%
  group_by(deg) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  rename(key=deg) %>%
  arrange(pc)

r <- r %>%
  mutate(deg2 = "PhD/DSc") %>%
  mutate(deg2 = ifelse(Q38_TEXT== "MA, MPH" | Q38==7, "MPH", deg2)) %>%
  mutate(deg2 = ifelse(Q38_TEXT=="I have an MD deg2ree and an MS in statistics"
                       | Q38 %in% 2:3, "MD",deg2)) %>%
  mutate(deg2 = ifelse(Q38_TEXT %in% c("Master in Public Administration (MPA)", "MPA")
                       | Q38 %in% c(9,11,12),
                       "MA/MBA/MPA/MS", deg2)) %>%
  mutate(deg2 = ifelse(Q38_TEXT=="Currently completing BS", "None", deg2)) %>%
  mutate(deg2 = ifelse(is.na(Q38), NA, deg2)) %>%
  mutate(deg2 = ifelse(Q38==4, "DrPH", deg2)) %>%
  mutate(deg2 = ifelse(Q38 %in% 14:15, "BA/BS", deg2))

degree2 <- r %>% 
  select(deg2) %>%
  na.omit() %>%
  group_by(deg2) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  rename(key=deg2) %>%
  arrange(pc) %>%
  mutate(cat = "Highest degree", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)


#organization type

r <- r %>%
  mutate( Q42 = ifelse(Q42_TEXT %in% c("Clinical practice and internet startup", "Employee-owned",
                                       "Independent Consultant working with Nonprofits and Govt. Agencies",
                                       "Medical center affiliated with University med school",             
                                       "Self-employed" ), 2,
                       ifelse(Q42_TEXT == "local government and a university",3,Q42)))


orgType <- r %>%
  select(Q42) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  group_by(Q42) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  rename(key=Q42) %>%
  mutate(key= factor(key, levels=1:4, 
                     labels=c("Academic", "For-profit", 
                              "Government","Nonprofit"))) %>%  
  arrange(pc) %>%
  mutate(cat = "Type of organization", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)

#create academic v. non-academic
orgType2 <- orgType %>%
  filter(key != "Academic") %>%
  select(-key, -cat) %>%
  summarise_all(sum) %>%
  mutate(key = "Non-academic") 

orgType3 <- orgType %>%
  filter(key=="Academic") %>%
  select( -cat) %>%
  bind_rows(orgType2)

#most-used software
softw <- r %>%
  select(Q36) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  mutate(Q36 = ifelse(Q36>1 & Q36<5, 9, Q36)) %>%
  group_by(Q36) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  rename(key=Q36) %>%
  mutate(key= factor(key, levels=c(1,5:9), 
                     labels=c("Excel", "R",  
                              "SAS","SPSS","Stata", "Other"))) %>%
  arrange(pc) %>%
  mutate(cat = "Most-used software", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)


#years in current position
mynames <- r %$% val_labels(Q41)

yrs <- r %>%
  select(Q41) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  rename(key = Q41) %>%
  group_by(key) %>%
  count() %>%
  ungroup() %>%
  mutate(key = r %$% names(val_labels(Q41))) %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  arrange(pc) %>%
  mutate(key = ifelse(key=="Less than 1 year", 
                      "<1 years", 
                      ifelse(key=="More than 10 years", 
                             "10+ years",key))) %>%
  mutate(key = str_sub(key, end=-6)) %>%
  mutate(key = factor(key)) %>%
  mutate(key = factor(key, levels=levels(key)[c(1:2,4,3)])) %>%
  arrange(key) %>%
  mutate(cat = "Years in current job", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)



#discipline

# Clinical Medicine
cm <-c("Medical","medicine", "MD PhD candidate","Nursing","Medicine","Physiotherapy",
       "Clinical Psychology")

# Public Health
ph <- c("MPH","Mph","PH","Pubklic Health","public health",
        "Public Health - Community Health Practice", 
        "Public health", "Public Health Education", 
        "Public Health Promotion", "Global health", "Global health",
        "Environmental Health Sciences","Spatial Analysis for Public Health",
        "spatial analytics", "Urban Spatial Analytics", 
        "Spatial Analytics","Health services resrsrch",
        "Social and Behavioral Sciences (Public Health)","Health Policy",
        "Health Services","HEALTH SERVICES RESEARCH","Health",
        "Health Policy & Management", "Health Services Research",
        "Healthcare","Healthcare Administration",
        "Public health, political science")


# Epi/biostat
matstat <- c("Applied Mathematics and Statistics", "Mathematics","Statistics",
             "Research Design & Statistics","Biostat/Epi","biostatistics", 
             "Biostatistics", "epi", "Epidemiology","Epidemiology & Biostatistics",
             "Epidemiology and biostatistics","Epidemiology/Biostatistics",
             "Public Health - Epi", "Public Health (Epidemiology)",
             "Public Health: Epidemiology", "Biostatistics/Epidemiology",
             "Spatial Analytics", "Community Health (Epidemiology)")

# Social Science
ss <- c("psychology","Quantitative psychology", "Educational Psychology","Ed Psych",
        "Sociology","Human Resource Studies","Social Work","Business Administration",
        "Economics","economics","Social and behavioral sciences","public policy",
        "Public Administration","Public Affairs", "Public Policy","Policy Analysis",
        "Public Administration and Policy","Criminal justice","Psychology",
        "Human Biology; Sociology","social psychology")

#Other
oth<- c("Oceanography","Operations Research","education","Toxicology","Masters",
        "Communication", "Geography","Nutrition","Neuroscience","Biological Science",
        "Biomedical Engineering","Biochemistry","Education")

disp <- r %>%
  select(Q39) %>%
  na.omit() %>%
  mutate(key = Q39) %>%
  mutate(key = ifelse(Q39 %in% cm, "Clinical Medicine",
                      ifelse(Q39 %in% ph, "Public Health",
                             ifelse(Q39 %in% matstat,
                                    "Biostatistics/Epidemiology",
                                    ifelse(Q39 %in% ss,
                                           "Social Science",
                                           ifelse(Q39 %in% oth, "Other",key)))))) %>%
  filter(key !="")

disp <- disp %>%
  select(key) %>%
  group_by(key) %>%
  count() %>%
  ungroup() %>%
  mutate(pc = round(n/sum(n)*100,1)) %>%
  arrange(pc) %>%
  mutate(cat = "Discipline", cat2 = paste0("(n=",sum(n), ")")) %>%
  mutate(cat = paste(cat, cat2)) %>%
  select(-cat2)


disp <- disp[c(1:3,5,4),]

dems4 <- bind_rows(gender, race2, age, degree2, disp, orgType,yrs, softw)
theme_set(theme_minimal())

#org type (recode in original data)
r <- r %>%
  mutate( Q42 = ifelse(Q42_TEXT %in% c("Clinical practice and internet startup", "Employee-owned",
                                       "Independent Consultant working with Nonprofits and Govt. Agencies",
                                       "Medical center affiliated with University med school",             
                                       "Self-employed" ), 2,
                       ifelse(Q42_TEXT == "local government and a university",3,Q42))) %>%
  rename(org = Q42) %>%
  mutate(org= factor(org, levels=1:4, 
                     labels=c("Academic", "For-profit", 
                              "Government","Nonprofit"))) %>%
  mutate(org2 = ifelse(as.numeric(org)==1, "Academic",
                       "Non-academic")) %>%
  mutate(org2 = factor(org2))

#recode software in original data
r <- r %>%
  mutate_at(vars(Q36), .funs=function(x) replace(x, which(x==-99), NA)) %>%
  mutate(Q36 = ifelse(Q36>1 & Q36<5, 9, Q36)) %>%
  rename(sftw=Q36) %>%
  mutate(sftw= factor(sftw, levels=c(1,5:9), 
                      labels=c("Excel", "R",  
                               "SAS","SPSS","Stata", "Other"))) %>%
  mutate(sftw = factor(sftw, levels=levels(sftw)[c(3:5,2,1,6)])) %>%
  mutate(Q26 = ifelse(Q25_2_2==1,1,Q26)) #include those in skip logic from previous question for ever sharing code

#recode gender in original data
r <- r %>%
  mutate_at(vars(Q43), .funs=function(x) replace(x, which(x==-99), NA)) %>%
  rename(gender=Q43) %>%
  mutate(gender= factor(gender, levels=1:4, 
                        labels=c("Male", "Female",  
                                 "Transgender", "Other"))) 

#ever made code available?
cdsh <- r %>%
  select(Q26, sftw) %>%
  mutate(Q26 = ifelse(Q26==2, 0,Q26) ) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  group_by(sftw) %>%
  mutate(tot=length(Q26) ) %>%
  mutate_at(vars(Q26), .funs = function(x) sum(x, na.rm=T)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Yes=Q26) %>%
  mutate(No = tot-Yes) %>%
  gather(key=yn, val, -tot, -sftw) %>%
  select(-tot) %>%
  mutate(key="Have you EVER made your code publicly available?")

#required to make data available?
req <- r %>%
  select(Q28_1_1:Q28_1_4, sftw) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  mutate(rq = ifelse(rowSums(.[,1:4], na.rm=T) %in% 1:4,1,0)) %>%
  group_by(sftw) %>%
  mutate(tot=length(rq) ) %>%
  mutate_at(vars(rq), .funs = function(x) sum(x, na.rm=T)) %>%
  select(-c(Q28_1_1:Q28_1_4)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Yes=rq) %>%
  mutate(No = tot-Yes) %>%
  gather(key=yn, val, -tot, -sftw) %>%
  select(-tot) %>%
  mutate(key="Were you required to make your data available?")

#make code or data from pub available to public?
lp <- r %>%
  select(sftw,Q25_1_1, Q25_1_2, Q25_2_1, Q25_2_2 ) %>%
  rename(crD = Q25_1_1, crC = Q25_1_2, shD =Q25_2_1, shC =Q25_2_2) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  group_by(sftw) %>%
  mutate_at(vars(crD:shC),.funs = function(x) sum(x, na.rm=T)) %>%
  mutate(tot = n()) %>%
  distinct(.) %>%
  na.omit() %>%
  select(-crD, -crC) %>%
  arrange(tot) %>% 
  mutate(totD = tot-shD, totC = tot-shC) %>%
  select(-tot) %>%
  gather(key, val, -sftw) %>%
  mutate(yn = ifelse(key %in% c("totD", "totC"), "No","Yes")) %>%
  mutate(key = ifelse(key=="totD", "shD",
                      ifelse(key=="totC","shC",key))) %>%
  ungroup() %>%
  mutate(key = ifelse(key=="shC", "Did you make your code publicly available?", "Did you make your data publicly available?")) %>%
  full_join(cdsh) %>%
  full_join(req) %>%
  mutate(sftw=factor(sftw, levels=rev(levels(sftw)))) %>%
  mutate(key = factor(key)) %>%
  mutate(key= factor(key, levels=levels(key)[c(2,1,4,3)]))


lp2 <- lp %>%
  group_by(key,yn) %>%
  summarise(tot= sum(val)) %>%
  ungroup() %>%
  group_by(key) %>%
  mutate(ovtot=sum(tot)) %>%
  filter(yn=="Yes") %>%
  mutate(pc = round(tot/ovtot*100)) %>%
  mutate(xl = 51,xr=79.99,yb=1,yt=2.99) %>%
  mutate(txt = paste0("Overall:\nYes = ", tot, " (",pc,"%)") )%>%
  mutate(xx = 2,yy=55)

#check sharing by academic v. non-academic
#ever made code available?
cdsh <- r %>%
  select(Q26, org2) %>%
  mutate(Q26 = ifelse(Q26==2, 0,Q26) ) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  group_by(org2) %>%
  mutate(tot=length(Q26) ) %>%
  mutate_at(vars(Q26), .funs = function(x) sum(x, na.rm=T)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Yes=Q26) %>%
  mutate(No = tot-Yes) %>%
  gather(key=yn, val, -tot, -org2) %>%
  select(-tot) %>%
  mutate(key="Have you EVER made your code publicly available?")

#required to make data available?
req <- r %>%
  select(Q28_1_1:Q28_1_4, org2) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  mutate(rq = ifelse(rowSums(.[,1:4], na.rm=T) %in% 1:4,1,0)) %>%
  group_by( org2) %>%
  mutate(tot=length(rq) ) %>%
  mutate_at(vars(rq), .funs = function(x) sum(x, na.rm=T)) %>%
  select(-c(Q28_1_1:Q28_1_4)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Yes=rq) %>%
  mutate(No = tot-Yes) %>%
  gather(key=yn, val, -tot, - org2) %>%
  select(-tot) %>%
  mutate(key="Were you required to make your data available?")

#make code or data from pub available to public?
lp <- r %>%
  select( org2,Q25_1_1, Q25_1_2, Q25_2_1, Q25_2_2 ) %>%
  rename(crD = Q25_1_1, crC = Q25_1_2, shD =Q25_2_1, shC =Q25_2_2) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  group_by( org2) %>%
  mutate_at(vars(crD:shC),.funs = function(x) sum(x, na.rm=T)) %>%
  mutate(tot = n()) %>%
  distinct(.) %>%
  na.omit() %>%
  select(-crD, -crC) %>%
  arrange(tot) %>% 
  mutate(totD = tot-shD, totC = tot-shC) %>%
  select(-tot) %>%
  gather(key, val, -org2) %>%
  mutate(yn = ifelse(key %in% c("totD", "totC"), "No","Yes")) %>%
  mutate(key = ifelse(key=="totD", "shD",
                      ifelse(key=="totC","shC",key))) %>%
  ungroup() %>%
  mutate(key = ifelse(key=="shC", "Did you make your code publicly available?", "Did you make your data publicly available?")) %>%
  full_join(cdsh) %>%
  full_join(req) 

#and check for sharing women v. men

#ever made code available?
cdsh <- r %>%
  select(Q26, gender) %>%
  mutate(Q26 = ifelse(Q26==2, 0,Q26) ) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  group_by(gender) %>%
  mutate(tot=length(Q26) ) %>%
  mutate_at(vars(Q26), .funs = function(x) sum(x, na.rm=T)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Yes=Q26) %>%
  mutate(No = tot-Yes) %>%
  gather(key=yn, val, -tot, -gender) %>%
  select(-tot) %>%
  mutate(key="Have you EVER made your code publicly available?")

#required to make data available?
req <- r %>%
  select(Q28_1_1:Q28_1_4, gender) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  mutate(rq = ifelse(rowSums(.[,1:4], na.rm=T) %in% 1:4,1,0)) %>%
  group_by( gender) %>%
  mutate(tot=length(rq) ) %>%
  mutate_at(vars(rq), .funs = function(x) sum(x, na.rm=T)) %>%
  select(-c(Q28_1_1:Q28_1_4)) %>%
  distinct() %>%
  na.omit() %>%
  rename(Yes=rq) %>%
  mutate(No = tot-Yes) %>%
  gather(key=yn, val, -tot, - gender) %>%
  select(-tot) %>%
  mutate(key="Were you required to make your data available?")

#make code or data from pub available to public?
lp <- r %>%
  select( gender,Q25_1_1, Q25_1_2, Q25_2_1, Q25_2_2 ) %>%
  rename(crD = Q25_1_1, crC = Q25_1_2, shD =Q25_2_1, shC =Q25_2_2) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  group_by( gender) %>%
  mutate_at(vars(crD:shC),.funs = function(x) sum(x, na.rm=T)) %>%
  mutate(tot = n()) %>%
  distinct(.) %>%
  na.omit() %>%
  select(-crD, -crC) %>%
  arrange(tot) %>% 
  mutate(totD = tot-shD, totC = tot-shC) %>%
  select(-tot) %>%
  gather(key, val, -gender) %>%
  mutate(yn = ifelse(key %in% c("totD", "totC"), "No","Yes")) %>%
  mutate(key = ifelse(key=="totD", "shD",
                      ifelse(key=="totC","shC",key))) %>%
  ungroup() %>%
  mutate(key = ifelse(key=="shC", "Did you make your code publicly available?", "Did you make your data publicly available?")) %>%
  full_join(cdsh) %>%
  full_join(req) 

# demographics for results first paragraph

# mean and sd age
meanAge <- round(mean(r$Q45, na.rm=T), 1)
sdAge <- round(sd(r$Q45, na.rm=T), 1)

# public health discipline percents 
ph.disp <- disp[disp$key == "Public Health" | disp$key == "Biostatistics/Epidemiology", ]
perc.disp <- sum(ph.disp$pc) 

# sex percents
n.female <- gender[gender$key == "Female",]$n
n.male <- gender[gender$key == "Male",]$n
perc.female <- gender[gender$key == "Female",]$pc
perc.male <- gender[gender$key == "Male",]$pc

# phd or dsc degree percent
perc.phdDsc <- degree2[degree2$key == "PhD/DSc",]$pc

# software use percents 
perc.sas <- softw[softw$key == "SAS",]$pc
perc.spss <- softw[softw$key == "SPSS",]$pc
perc.stata <- softw[softw$key == "Stata",]$pc
perc.r <- softw[softw$key == "R",]$pc

# white race percents
perc.white <- race2[race2$key == "White",]$pc

# academic percent
perc.academic <- orgType3[orgType3$key == "Academic",]$pc

# length in field
perc.years <-yrs[yrs$key == "4-10",]$pc

# section on making data and code available

# recode data and code available variables
r$Q25_2_1[r$Q25_2_1==1] <- "Yes"
r$Q25_2_2[r$Q25_2_2==1] <- "Yes"
r$Q25_2_1[r$Q25_2_1==0]<-"No"
r$Q25_2_2[r$Q25_2_2==0]<-"No"
r$Q25_2_1[r$Q25_2_1==-99] <- NA
r$Q25_2_2[r$Q25_2_2==-99] <- NA

# stats on reproducibility practices for paragraph

# table for code vs. data available
codeDataAvail <- data.frame(table(data = r$Q25_2_1, code = r$Q25_2_2))

n.codeOnlyAvail <- codeDataAvail[codeDataAvail$code == "Yes" & 
                                   codeDataAvail$data == "No", ]$Freq 
n.dataOnlyAvail <- codeDataAvail[codeDataAvail$data == "Yes" & 
                                   codeDataAvail$code == "No", ]$Freq 

n.bothAvail <- codeDataAvail[codeDataAvail$code == "Yes" & 
                               codeDataAvail$data == "Yes", ]$Freq
perc.bothAvail <- round(100*(n.bothAvail/sum(codeDataAvail$Freq)), 1)

n.oneOrBothAvail <- sum(codeDataAvail[codeDataAvail$code == "Yes" | 
                                        codeDataAvail$data == "Yes", ]$Freq)
perc.oneOrBothAvail <- round(100*(n.oneOrBothAvail/sum(codeDataAvail$Freq)), 1)

# ever shared code
r$Q26[r$Q26 == -99] <- NA
r$Q26[r$Q26 == 1] <- "Yes"
r$Q26[r$Q26 == 2] <- "No"
everSharedCode <- data.frame(table(code = r$Q25_2_2, everCode = r$Q26))

# didn't share code recent publication but shared code sometime
n.everSharedCode <- everSharedCode[everSharedCode$everCode == "Yes" & 
                                     everSharedCode$code == "No",]$Freq
perc.everSharedCode <- round(100*n.everSharedCode/sum(codeDataAvail$Freq[codeDataAvail$code == "No"]), 1)

n.noCurrentCodeShare <- sum(codeDataAvail$Freq[codeDataAvail$code == "No"])

# sharing by job & sex 

# total academics and non
n.academic <- orgType3[orgType3$key == "Academic",]$n
n.nonAcademic <- orgType3[orgType3$key == "Non-academic",]$n

# data and code sharing by academics
academicsShareCode <- data.frame(table(academics = r$org, code = r$Q25_2_2))
academicsShareData <- data.frame(table(academics = r$org, data = r$Q25_2_1))
n.acadShareCode <- academicsShareCode[academicsShareCode$code == "Yes" & academicsShareCode$academics == "Academic",]$Freq
perc.acadShareCode <- round(100*(n.acadShareCode/sum(academicsShareCode[academicsShareCode$academics == "Academic",]$Freq)), 1)

n.acadShareData <- academicsShareData[academicsShareData$data == "Yes" & academicsShareData$academics == "Academic",]$Freq
perc.acadShareData <- round(100*(n.acadShareData/sum(academicsShareData[academicsShareData$academics == "Academic",]$Freq)), 1)

# sharing non-academics
n.nonacadShareCode <- sum(academicsShareCode[academicsShareCode$code == "Yes" & academicsShareCode$academics != "Academic",]$Freq)
perc.nonacadShareCode <- round(100*(n.acadShareCode/sum(academicsShareCode[academicsShareCode$academics != "Academic",]$Freq)), 1)

n.nonacadShareData <- sum(academicsShareData[academicsShareData$data == "Yes" & academicsShareData$academics != "Academic",]$Freq)
perc.nonacadShareData <- round(100*(n.acadShareData/sum(academicsShareData[academicsShareData$academics != "Academic",]$Freq)), 1)

# data and code sharing by sex
sexShareCode <- data.frame(table(sex = r$gender, code = r$Q25_2_2))
sexShareData <- data.frame(table(sex = r$gender, data = r$Q25_2_1))
n.maleShareCode <- sexShareCode[sexShareCode$code == "Yes" & sexShareCode$sex == "Male",]$Freq
perc.maleShareCode <- round(100*(n.maleShareCode/sum(sexShareCode[sexShareCode$sex == "Male",]$Freq)), 1)

n.femaleShareCode <- sexShareCode[sexShareCode$code == "Yes" & sexShareCode$sex == "Female",]$Freq
perc.femaleShareCode <- round(100*(n.maleShareCode/sum(sexShareCode[sexShareCode$sex == "Female",]$Freq)), 1)

n.maleShareData <- sexShareData[sexShareData$data == "Yes" & sexShareData$sex == "Male",]$Freq
perc.maleShareData <- round(100*(n.maleShareData/sum(sexShareData[sexShareData$sex == "Male",]$Freq)), 1)

n.femaleShareData <- sexShareData[sexShareData$data == "Yes" & sexShareData$sex == "Female",]$Freq
perc.femaleShareData <- round(100*(n.maleShareData/sum(sexShareData[sexShareData$sex == "Female",]$Freq)), 1)

# other sex shared code or data
code_avail <- subset(r, Q25_2_2=="Yes")
n.otherShareCode <- nrow(code_avail) - sum(n.maleShareCode, n.femaleShareCode)


n.codeAvail <- sum(n.codeOnlyAvail, n.bothAvail)
n.dataAvail <- sum(n.dataOnlyAvail, n.bothAvail)

data_avail <- subset(r, Q25_2_1=="Yes")
data_avail$required <- data_avail$Q28_1_1 + 
  data_avail$Q28_1_2 +
  data_avail$Q28_1_3 +
  data_avail$Q28_1_4
data_avail$required[data_avail$required > 0]  <- "Yes"
data_avail$required[data_avail$required == 0] <- "No"
n.requiredData <- sum(data_avail$required == "Yes")
perc.requiredData <- round(100*n.requiredData/nrow(data_avail), 1)

data_avail$Q28_1_1[data_avail$Q28_1_1==1]<-"Required to make public"
data_avail$Q28_1_2[data_avail$Q28_1_2==1]<-"Required to make public"
data_avail$Q28_1_3[data_avail$Q28_1_3==1]<-"Required to make public"
data_avail$Q28_1_4[data_avail$Q28_1_4==1]<-"Required to make public"
data_avail$Q28_1_1[data_avail$Q28_1_1==0]<-"Did not require"
data_avail$Q28_1_2[data_avail$Q28_1_2==0]<-"Did not require"
data_avail$Q28_1_3[data_avail$Q28_1_3==0]<-"Did not require"
data_avail$Q28_1_4[data_avail$Q28_1_4==0]<-"Did not require"
data_avail$Q28_1_1[data_avail$Q28_1_1==-99]<-NA
data_avail$Q28_1_2[data_avail$Q28_1_2==-99]<-NA
data_avail$Q28_1_3[data_avail$Q28_1_3==-99]<-NA
data_avail$Q28_1_4[data_avail$Q28_1_4==-99]<-NA

#check required vs. source of required
#table(data_avail$Q25_2_1, data_avail$Q28_1_1) #funder
#table(data_avail$Q25_2_1, data_avail$Q28_1_2) #journal
#table(data_avail$Q25_2_1, data_avail$Q28_1_3) #employer
#table(data_avail$Q25_2_1, data_avail$Q28_1_4) #team

#recode code required to make code available
code_avail <- subset(r, Q25_2_2=="Yes")
code_avail$required <- code_avail$Q28_2_1 + 
                           code_avail$Q28_2_2 +
                           code_avail$Q28_2_3 +
                           code_avail$Q28_2_4
code_avail$required[code_avail$required > 0]  <- "Yes"
code_avail$required[code_avail$required == 0] <- "No"

# number required 
n.requiredCode <- sum(code_avail$required == "Yes")
perc.requiredCode <- round(100*n.requiredCode/nrow(code_avail), 1)

code_avail$Q28_2_1[code_avail$Q28_2_1==1]<-"Required to make public"
code_avail$Q28_2_2[code_avail$Q28_2_2==1]<-"Required to make public"
code_avail$Q28_2_3[code_avail$Q28_2_3==1]<-"Required to make public"
code_avail$Q28_2_4[code_avail$Q28_2_4==1]<-"Required to make public"
code_avail$Q28_2_1[code_avail$Q28_2_1==0]<-"Did not require"
code_avail$Q28_2_2[code_avail$Q28_2_2==0]<-"Did not require"
code_avail$Q28_2_3[code_avail$Q28_2_3==0]<-"Did not require"
code_avail$Q28_2_4[code_avail$Q28_2_4==0]<-"Did not require"

#format into data frame for figure
data_code_avail_req <- cbind(table(data_avail$Q28_1_1),table(data_avail$Q28_1_2),
                             table(data_avail$Q28_1_3),table(data_avail$Q28_1_4),
                             table(code_avail$Q28_2_1),table(code_avail$Q28_2_2),
                             table(code_avail$Q28_2_3),table(code_avail$Q28_2_4))
colnames(data_code_avail_req) <- c("Funder","Journal","Employer",
                                   "Research team","Funder","Journal","Employer",
                                   "Research team")
data_code_avail_req <- melt(data_code_avail_req)
colnames(data_code_avail_req) <- c("req","who_req","number")
data_code_avail_req$data_code <- c("Data","Data","Data","Data",
                                   "Data","Data","Data","Data",
                                   "Code","Code","Code","Code",
                                   "Code","Code","Code","Code")

# barriers data management

barr1 <- r %>%
  select(Q31_1:Q31_5, org2) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  gather(key,val, -org2) %>%
  group_by(org2, key) %>%
  mutate(tot = n(), Yes=sum(val)) %>%
  select(-val) %>%
  distinct() %>%
  mutate(No=tot-Yes) %>%
  select(-tot) %>%
  rename(qq = key) %>%
  gather(key, val, -org2, -qq) %>%
  mutate(cat = ifelse(qq=="Q31_1", "Additional human and financial resources",
                      ifelse(qq=="Q31_2", "Training on reproducible research practices",
                             ifelse(qq=="Q31_3",
                                    "Requirements by funders to disseminate data and statistical code",
                                    ifelse(qq=="Q31_4",
                                           "Requirements by journals to include access to data and statistical code",
                                           ifelse(qq=="Q31_5",
                                                  "Workplace incentives (e.g., pay increases or credit toward tenure)",qq)))))) %>%
  ungroup() %>%
  select(-qq)

#total n for each facilitator
barr1tot <- barr1[2:4]
barr1tot <- subset(barr1tot, key=="Yes")
barr1tot <- barr1tot[2:3]
barr1tot <- barr1tot %>% group_by(cat) %>% summarize_all(sum)

barr2 <- r %>%
  select(Q32_1:Q32_7, org2) %>%
  mutate_all(.funs=function(x) replace(x, which(x==-99), NA)) %>%
  na.omit() %>%
  gather(key,val, -org2) %>%
  group_by(org2, key) %>%
  mutate(tot = n(), Yes=sum(val)) %>%
  select(-val) %>%
  distinct() %>%
  mutate(No=tot-Yes) %>%
  select(-tot) %>%
  rename(qq = key) %>%
  gather(key, val, -org2, -qq) %>%
  mutate(cat = ifelse(qq=="Q32_1", "Lack of time", qq)) %>%
  mutate(cat = ifelse(qq=="Q32_2", "Lack of knowledge or training on reproducible research practices",cat)) %>%
  mutate(cat = ifelse(qq=="Q32_3","Data privacy", cat)) %>%
  mutate(cat = ifelse(qq=="Q32_4","Intellectual property concerns", cat)) %>%
  mutate(cat = ifelse(qq=="Q32_5","Professional competition", cat)) %>%
  mutate(cat = ifelse(qq=="Q32_6","Concerns of errors being discovered", cat)) %>%
  mutate(cat = ifelse(qq=="Q32_7","Lack of incentive", cat)) %>%
  ungroup() %>%
  select(-qq) %>%
  #bind_rows(barr1) %>%
  group_by(org2, cat) %>%
  mutate(tot = sum(val)) %>%
  filter(key=="Yes") %>%
  mutate(pcYes = val/tot*100)

#total n for each barrier
barr2tot <- barr2[3:4]
barr2tot <- barr2tot %>% group_by(cat) %>% summarize_all(sum)
barr2tot$pcYes <- (barr2tot$val/211)*100
barr2tot$org2 <- "Total"

#combine barr2 and barr2tot
vars <- c("org2","val","cat","pcYes")
barr2small <- barr2[vars]
barr2small$org2 <- as.character(barr2small$org2)
barr2all <- (rbind(data.frame(barr2tot),data.frame(barr2small)))
barr2all$pcYes <- barr2all$pcYes/100

#no barriers
n.noBarriers <- sum(r$Q32_9, na.rm = T)

#have  not tried to public the data or code
n.notTried <- sum(r$Q32_10, na.rm = T)

#open-ended responses
n.text <- sum(r$Q32_8_TEXT != "")

# data privacy n and percent
n.dataPrivacy <- barr2tot[barr2tot$cat == "Data privacy", ]$val
perc.dataPrivacy <- round(barr2tot[barr2tot$cat == "Data privacy", ]$pcYes, 1)

n.lackTime <- barr2tot[barr2tot$cat == "Lack of time", ]$val
perc.lackTime <- round(barr2tot[barr2tot$cat == "Lack of time", ]$pcYes, 1)

n.intellProp <- barr2tot[barr2tot$cat == "Intellectual property concerns", ]$val
perc.intellProp <- round(barr2tot[barr2tot$cat == "Intellectual property concerns", ]$pcYes, 1)


#make data frame of included details
include_manu <- cbind(table(r$Q24_1),table(r$Q24_2),table(r$Q24_3),
                      table(r$Q24_4),table(r$Q24_5),table(r$Q24_6),
                      table(r$Q24_7),table(r$Q24_8),table(r$Q24_9),
                      table(r$Q24_10),table(r$Q24_11))
colnames(include_manu) <- c("Software",
                            "Units of analysis",
                            "Details on missing data handling",
                            "Variable recoding details",
                            "The name, units, and types of variable analyzed",
                            "The statistical approaches used",
                            "The type of test statistics computed",
                            "The value of test statistics",
                            "The specific variables in each statistical model",
                            "Sample sizes for each analysis",
                            "Precise p-values when possible")
include_manu <- melt(include_manu)
include_manu$Var1[include_manu$Var1==0] <- "No"
include_manu$Var1[include_manu$Var1==1] <- "Yes"
colnames(include_manu) <- c("included","detail","number")

# not including sample sizes
perc.noSampleSize <- 100 - round(100*sum(!is.na(r$Q24_10[r$Q24_10 == 1]))/sum(!is.na(r$Q24_10)), 1) 

# ease for data (n=221)
perc.vEasyData <- round(100*sum(!is.na(r$Q11_1[r$Q11_1 == 1]))/sum(!is.na(r$Q11_1)), 1)
perc.sEasyData <- round(100*sum(!is.na(r$Q11_1[r$Q11_1 == 2]))/sum(!is.na(r$Q11_1)), 1)

perc.diffData <- 100 - (perc.vEasyData + perc.sEasyData)

# ease for code (n=199)
r$Q11_2[r$Q11_2==-99] <- NA
perc.vEasyCode <- round(100*sum(!is.na(r$Q11_2[r$Q11_2 == 1]))/sum(!is.na(r$Q11_2)), 1)
perc.sEasyCode <- round(100*sum(!is.na(r$Q11_2[r$Q11_2 == 2]))/sum(!is.na(r$Q11_2)), 1)
perc.diffCode <- 100 - (perc.vEasyCode + perc.sEasyCode)

# codebook
perc.codebook <- round(100*sum(!is.na(r$Q14[r$Q14 == 1]))/sum(!is.na(r$Q14)), 1)

# followed guidelines?
n.followed <- sum(!is.na(r$Q19[r$Q19 < 3]))
perc.partialFollowed <- round(100*sum(!is.na(r$Q19[r$Q19 == 2]))/sum(!is.na(r$Q19)), 1)
perc.closelyFollowed <- round(100*sum(!is.na(r$Q19[r$Q19 == 1]))/sum(!is.na(r$Q19)), 1)
perc.notFollowed <- 100 - (perc.partialFollowed + perc.closelyFollowed)

#why followed guidelines
n.policy <- sum(!is.na(r$Q21_1[r$Q21_1 == 1])) #11
n.requiredForPub <- sum(!is.na(r$Q21_2[r$Q21_2 == 1])) #6
n.lifeEasier <- sum(!is.na(r$Q21_3[r$Q21_3 == 1])) #50
n.betterCode <- sum(!is.na(r$Q21_4[r$Q21_4 == 1])) #41
n.taught <- sum(!is.na(r$Q21_5[r$Q21_5 == 1])) #38
n.collaboration <- sum(!is.na(r$Q21_6[r$Q21_6 == 1])) #28
n.reproducibility <- sum(!is.na(r$Q21_7[r$Q21_7 == 1])) #37
n.other <- sum(!is.na(r$Q21_8[r$Q21_8 == 1])) #4

perc.lifeEasier <- round(100*n.lifeEasier/n.followed, 1)

#make data frame of included details
code_format <- cbind(table(r$Q22_1),table(r$Q22_2),table(r$Q22_3),
                     table(r$Q22_4),table(r$Q22_5),table(r$Q22_6),
                     table(r$Q22_7),table(r$Q22_8),table(r$Q22_9),
                     table(r$Q22_10))
colnames(code_format) <- c("Used nouns for variables and/or verbs for functions",
                           "Limited lines of code to a certain length",
                           "Included metadata, such as the date or project name, in the file title",
                           "Included seed values for analyses that included randomness",
                           "Used a consistent way to name variables and functions",
                           "Separated analysis steps with white space or blank lines",
                           "Used indentation to group lines of code within procedures/functions",
                           "Wrote functions for tasks repeated multiple times",
                           "Included some results within the annotation ",
                           "Integrated code with text and results (literate programming)")
code_format <- melt(code_format)
code_format$Var1[code_format$Var1==0] <- "No"
code_format$Var1[code_format$Var1==1] <- "Yes"
code_format <- subset(code_format, Var1!=-99)
colnames(code_format) <- c("used","format","number")

#why followed guidelines
n.additionalHuman <- sum(!is.na(r$Q31_1[r$Q31_1 == 1]))
perc.additionalHuman <- round(100*n.additionalHuman/sum(!is.na(r$Q31_1)), 1)
n.training <- sum(!is.na(r$Q31_2[r$Q31_2 == 1]))
perc.training <- round(100*n.training/sum(!is.na(r$Q31_2)), 1)
n.requireFunders <- sum(!is.na(r$Q31_3[r$Q31_3 == 1])) 
perc.requireFunders <- round(100*n.requireFunders/sum(!is.na(r$Q31_3)) ,1)
n.requireJournals <- sum(!is.na(r$Q31_4[r$Q31_4 == 1])) 
perc.requireJournals <- round(100*n.requireJournals/sum(!is.na(r$Q31_4)), 1)
n.workplace <- sum(!is.na(r$Q31_5[r$Q31_5 == 1])) 
perc.workplace <- round(100*n.workplace/sum(!is.na(r$Q31_5)), 1)




#plot who required to make data or code available
theme_set(theme_minimal())
fig1 <- ggplot(data_code_avail_req, 
               aes(x = who_req, y = number, fill = req)) + 
  facet_grid(~data_code) + 
  geom_bar(stat = "identity", position="dodge") +
  coord_flip() + 
  theme(legend.position = 'top') + 
  labs(y="Number of participants who made public", x="", fill="") +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values = c("#a6cee3", "#1f78b4"))
fig1

# graph of total barriers experienced
theme_set(theme_minimal())
fig2 <- ggplot(barr2all, 
               aes(x=reorder(cat,val), y=pcYes, fill=org2)) + 
  geom_bar(stat = "identity", position="dodge") +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  coord_flip() +
  theme(legend.position = 'top') + 
  labs(y="Percent of participants", x="", fill="") +
  scale_x_discrete(labels = wrap_format(30)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  scale_fill_manual(values=c("#a6cee3", "#1f78b4", "#b2df8a"))
fig2

# plot of percent including details in publication
theme_set(theme_minimal())
fig3 <- ggplot(subset(include_manu, included=="Yes"), 
               aes(x=reorder(detail,number), y=number/sum(!is.na(r$Q24_1)))) + 
  geom_bar(stat = "identity", position="dodge", fill = "#a6cee3", width = 0.5) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) +
  coord_flip() + 
  labs(y="Percent of participants", x="") +
  scale_x_discrete(labels = wrap_format(30))
fig3

#plot included details
theme_set(theme_minimal())
fig4 <- ggplot(subset(code_format, used=="Yes"), 
               aes(x=reorder(format, number), y=(number/214))) + 
  geom_bar(stat = "identity", position="dodge", fill = "#a6cee3", width = 0.5) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(0,1)) + 
  coord_flip() + 
  theme(legend.position = 'top') + 
  labs(y="Percent of participants", x="") +
  scale_x_discrete(labels = wrap_format(30))
fig4