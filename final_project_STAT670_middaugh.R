#Esme Middaugh
#STATS 670
#Final Project
#2018DEC05


library(ade4)
library(magrittr)
library(tidyverse)
library(broom)
library(ggplot2)
#install.packages("ggthemes") 
library(ggthemes)
library(viridis)
#install.packages("ggfortify")
library(ggfortify)
library(devtools)
install_github("vqv/ggbiplot")
library(ade4)
library(ggbiplot)
library(GGally)
library(dplyr)

#Lots of Data Prep & Exploration 
drugs_full = read.csv("drug_consumption.data.csv")
summary(drugs_full)

colnames(drugs_full) = c( 'id', 'age', 'gender', 'education', 'country', 
                     'ethnicity', 'neuroticism', 'extraversion', 
                     'openness', 'agreeableness', 'conscientiousness', 
                     'impulsiveness', 'sensation', 'alcohol', 'amphet', 
                     'amyl', 'benzos', 'caffeine', 'cannabis', 'chocolate', 'cocaine', 
                     'crack', 'ecstasy', 'heroin', 'ketamine', 'legal_hi', 
                     'lsd', 'meth','mushrooms', 'nicotine', 'semer', 'vsa')



#First need to Recode
recode = function(x) {
  if(is.na(x)) {
    return(0)
  } else if(x == "CL0") {
    return(0)
  } else if(x == "CL1") {
    return(1)
  } else if(x == "CL2") {
    return(2)
  } else if(x == "CL3") {
    return(3)
  } else if(x == "CL4") {
    return(4)
  } else if(x == "CL5") {
    return(5)
  } else if(x == "CL6") {
    return(6)
  } 
  else{
    return (x)
  }
}

numify = function(x) {
    return (as.numeric(x))
}

drugs_full.r = apply(drugs_full, 1:2, recode)
drugs_full.num = apply(drugs_full.r, 1:2, numify)
drugs_full.pca_scaled = prcomp(drugs_full.num, scale. = TRUE)

#Appendix, Figure 1 
ggpairs(drugs_full.num, columns=c('age', 'gender', 'education', 'ethnicity', 'heroin')) +
  theme_hc() + 
  ggtitle('Demographics and Heroin Use')


#FIGURE 1
#PCA FOR FULL DATASET 
autoplot(drugs_full.pca_scaled, data = drugs_full.num, alpha=.08,
         loadings = TRUE, loadings.colour = 'seagreen3',
         loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour='black')+
  theme_hc()+ 
  ggtitle("PCA of Personality Traits and Drug & Alcohol Use")


#Didn't use
#drops <- c("id", "education", "country", "ethnicity",
#           "amphet", "amyl", "benzos", "cocaine", "crack",
#           "ecstasy", "ketamine", "legal_hi", "lsd", "meth",
#           "mushrooms", "nicotine", "semer", "vsa")
#drugs = drugs_full[ , !(names(drugs_full) %in% drops)]
#drugs_r = apply(drugs, 1:2, recode) #Recoding 
#drugs_num= apply(drugs_r, 1:2, numify) #Coverting to numeric for use in PCA 
#drugs.pca_scaled= prcomp(drugs_num, scale. = TRUE) #Taking PCA
#drugs_recoded = as.data.frame(drugs_full.num)
#drugs_recoded %>%
#  select(gender, heroin) %>%
#  group_by(gender) %>%
#  summarize( heroin_use = mean(heroin, na.rm = T))

#autoplot(drugs.pca_scaled, data = drugs_num, alpha=.08,
#         loadings = TRUE, loadings.colour = 'seagreen3',
#         loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour='black')+
#  theme_hc()+ 
#  ggtitle("PCA of Personality Traits and Drug & Alcohol Use")


## JUST LOOKING AT HEROIN 
h_drops = c("id", "age", "gender", "education", "country", "ethnicity",
            "amphet", "amyl", "benzos", "cocaine", "crack",
            "ecstasy", "ketamine", "legal_hi", "lsd", "meth",
            "mushrooms", "nicotine", "semer", "vsa", "alcohol", "caffeine", "cannabis", "chocolate")
h = drugs_full[ , !(names(drugs_full) %in% h_drops)]
h_r = apply(h, 1:2, recode) #Recoding 
h_num = apply(h_r, 1:2, numify) #Coverting to numeric for use in PCA 
h.pca_noscale = prcomp(h_num, scale. = FALSE) #Taking PCA

##FIGURE 2
#PCA OF PERSONALITY TRAITS AND HEROIN USE
autoplot(h.pca_noscale, data = h_num, alpha=.08,
         loadings = TRUE, loadings.colour = 'seagreen3',
         loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour='black')+
  theme_hc()+ 
  ggtitle("PCA of Personality Traits and Heroin Use")


### PCA Among Heroin Users: Didn't end up using in presentation
#h_users = h %>%
#  filter(heroin != 'CL0' || heroin != "CL1")
#h_users.r = apply(h_users, 1:2, recode) #Recoding 
#h_users.num = apply(h_users.r, 1:2, numify) #Coverting to numeric for use in PCA 
#h_users.pca_noscale = prcomp(h_users.num, scale. = FALSE) #Taking PCA

#autoplot(h_users.pca_noscale, data = h_users.num, alpha=.08,
#         loadings = TRUE, loadings.colour = 'seagreen3',
#         loadings.label = TRUE, loadings.label.size = 3, loadings.label.colour='black')+
#  theme_hc()+ 
#  ggtitle("PCA of Personality Traits among Heroin Users")

## Lets move on to Ordered Categorical Responses 
##Didn't Use, just distribution
#ggplot(h_users, aes(x = impulsiveness, y = heroin, color = heroin)) +
#  geom_jitter(width = .2, height = 0.2) + ggtitle("Heroin Use by Impulsiveness Score") + 
#  theme_hc()

h_factored = h 
h_factored$Heroin_Use = recode_factor(h$heroin,
                                      "CL0" = "Never Used", 
                                      "CL1" = "Used Over a Decade Ago", 
                                      "CL2" = "Used in Last Decade",
                                      "CL3" = "Used in Last Year",
                                      "CL4"= "Used in Last Month",
                                      "CL5"= "Used in Last Week",
                                      "CL6"= "Used in Last Day", 
                                      .ordered = TRUE)

#FIGURE 3
#Conditional Density Plot: Likelihood of Heroin Ue
ggplot(h_factored, aes(x = impulsiveness + neuroticism + sensation, ..count.., fill = Heroin_Use)) +
  geom_density(position = "fill") +
  ggtitle("Likelihood of Heroin Use") 


######################################
### Creating another dataset to include the demographic variables
comp_drugs = drugs_full %>%
  select(c("gender", "education", "neuroticism", "impulsiveness", "sensation",
           "crack", "heroin", "meth"))

#Creating New Variables for users and non users
comp_drugs = comp_drugs %>%
  mutate(
    heroin_user = ifelse(heroin %in% c("CL0", "CL1"), 0, 1),
    meth_user = ifelse(meth %in% c("CL0", "CL1"), 0, 1),
    crack_user = ifelse(crack %in% c("CL0", "CL1"), 0, 1)
  )
#Now creating variables to check for multiple drug use possibilities 
comp_drugs = comp_drugs %>%
  mutate(
    heroin_meth_user = ifelse(heroin_user == 1 && meth_user == 1, 1, 0),
    heroin_crack_user = ifelse(heroin_user == 1 && crack_user == 1, 1, 0),
    meth_crack_user= ifelse(meth_user == 1 && crack_user == 1, 1, 0),
    meth_crack_heroin_user = ifelse(heroin_user == 1 & meth_user == 1 && crack_user == 1, 1, 0)
  )

#Getting the numeric value for heroin as well
comp_drugs.r = apply(comp_drugs, 1:2, recode) ## Getting heroin as a numeric variable
comp_drugs.r = as.data.frame(comp_drugs.r)
comp_drugs$heroin_num = comp_drugs.r$heroin

comp_drugs$heroin = recode_factor(comp_drugs$heroin,
              "CL0" = "Never Used", 
              "CL1" = "Used Over a Decade Ago", 
              "CL2" = "Used in Last Decade",
              "CL3" = "Used in Last Year",
              "CL4"= "Used in Last Month",
              "CL5"= "Used in Last Week",
              "CL6"= "Used in Last Day", 
              .ordered = TRUE)

## And lets also add a column for our combined personality traits
comp_drugs = comp_drugs %>%
  mutate(
    h_traits = impulsiveness + sensation + neuroticism
  )

## And fix the values for education and gender so that they mean something when we use them for faceting 
comp_drugs$education_labels = recode_factor(comp_drugs$education,
                                     "-2.43591" = "Left school before 16 years",
                                     "-1.73790" = "Left school at 16 years",
                                     "-1.43719" = "Left school at 17 years",
                                     "-1.22751" = "Left school at 18 years",
                                     "-0.61113" = "Some college or university",
                                     "-0.05921" = "Professional certificate/ diploma",
                                     "0.45468" = "University degree",
                                     "1.16365" =  "Masters degree" ,
                                     "1.98437" =  "Doctorate degree", 
                                     .ordered = TRUE)

comp_drugs$gender = recode_factor(comp_drugs$gender,
                                  "0.48246" = "Female",
                                  "-0.48246" = "Male",
                                  .ordered = FALSE)

##Lets repeat the same thing, but instead only look at heroin users and then compare
#Preliminary; cleaned up later
#ggplot(comp_drugs, aes(x = h_traits, y = heroin_user)) +
#  geom_jitter(height = 0.1, width = 0.25) +
#  geom_smooth(method = "glm") + 
#  theme_hc()+
#  xlab('neuroticism + sensation + impulsiveness') + 
#  ggtitle("Heroin Use vs Personality Traits at Different Education Levels")+ 
#  facet_wrap(~education)


#Heroin Use vs Personality Traits broken by Gender
#Using the glm binomial model
#Not used in presentation
#ggplot(comp_drugs, aes(x = h_traits, y = heroin_user, colour=h_traits) ) +
#  geom_jitter(height = 0.1, width = 0.25) +
#  geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
#  theme_hc()+
#  xlab('neuroticism + sensation + impulsiveness') + 
#  ggtitle("Heroin Use vs Personality Traits ")+ 
#  scale_color_viridis()+
#  facet_wrap(~gender)

##FIGURE 5
#Heroin Use vs Personality Traits broken by Education
## Using the glm binomial model
ggplot(comp_drugs, aes(x = h_traits, y = heroin_user, colour=h_traits) ) +
  geom_jitter(height = 0.1, width = 0.25, alpha=.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
  theme_hc()+
  xlab('neuroticism + sensation + impulsiveness') + 
  ggtitle("Heroin Use vs Personality Traits Faceted by Education Level ")+ 
  scale_color_viridis()+
  facet_wrap(~education_labels)


#Necessary for Legend 
cols <- c("heroin"="yellow","meth"="lightskyblue","crack"="seagreen")

#Not used in paper but interesting 
##### Heroin, Meth, and Crack Use vs Personality Traits
#ggplot(comp_drugs, aes(x = h_traits, y = heroin_user) ) +
#  geom_jitter(height = 0.1, width = 0.25) +
#  geom_smooth(aes(colour="heroin"), method = "glm", method.args = list(family = "binomial")) + 
#  geom_smooth(aes(x=h_traits, y=crack_user, colour="crack"), method = "glm", method.args = list(family = "binomial")) +
#  geom_smooth(aes(x=h_traits, y=meth_user, colour="meth") , method = "glm", method.args = list(family = "binomial")) +
##  theme_hc()+
#  xlab('neuroticism + sensation + impulsivenes score') + 
#  ylab('Likelihood of Drug Use') +
#  ggtitle("Heroin, Meth, and Crack Use vs Personality Traits")+ 
#  facet_wrap(~education_labels) + 
#  scale_colour_manual(name="Legend",values=cols) + 
#  scale_fill_manual(name="Legend",values=cols) 


##FIGURE 4 
### HEROIN METH AND CRACK USE VS EDUCATION FACETED BY GENDER
ggplot(comp_drugs, aes(x = education, y = heroin_user) ) +
  #geom_jitter(height = 0.1, width = 0.25) +
  geom_smooth(aes(colour="heroin"), method = "glm", method.args = list(family = "binomial")) + 
  geom_smooth(aes(x=education, y=crack_user, colour="crack"), method = "glm", method.args = list(family = "binomial")) +
  geom_smooth(aes(x=education, y=meth_user, colour="meth") , method = "glm", method.args = list(family = "binomial")) +
  theme_hc()+
  xlab('Education') + 
  ylab('Likelihood of Drug Use') +
  ggtitle("Heroin, Meth, and Crack Use vs Education faceted by Gender")+ 
  facet_wrap(~gender) + 
  scale_colour_manual(name="Legend",values=cols) + 
  scale_fill_manual(name="Legend",values=cols) + 
  xlim(-2.43591, 1.98437)+ 
  ylim(0, 1)


#Combining groups to work for final graph  
comp_drugs = comp_drugs %>%
  mutate(
    education_group = ifelse(
      education_labels %in% c("Masters degree", "University Degree","Doctorate degree"), "Completed University", no="Does not have University Degree")
  )

####FIGURE 6
#Heroin Use vs Personality Traits Faceted by Education Level and Lines based off Gender
ggplot(comp_drugs, aes(x = h_traits, y = heroin_user, colour=gender) ) +
  geom_jitter(height = 0.1, width = 0.25, alpha=.2) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) + 
  theme_hc()+
  xlab('neuroticism + sensation + impulsiveness') + 
  ylab('Likelihood of Heroin Use') +
  ggtitle("Heroin Use vs Personality Traits faceted by Education Level")+ 
  scale_color_viridis_d()+
  facet_wrap(~education_group)

