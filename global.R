library(shiny)
library(bslib)
library(ggplot2)
library(ROCR)
library(rpart) #for tree method
library(rpart.plot)
set.seed(613) #for train/test split


df = read.csv("https://raw.githubusercontent.com/lmartinez25/DS501/refs/heads/main/case3_data_clean.csv")

vars = c('ESR',names(df[,c(2:7,9:14,17:21)]))

df$ILF = as.factor(df$ESR)
#levels(df$ILF) = c("L","N","L")
df$ILF <- ifelse(df$ILF == "Not in labor force", "N","L")
df$ILF = factor(df$ILF, c("N","L")) #switch levels so that 0 is N and 1 is L when used in models


df$total = c(rep(1,dim(df)[1])) #for first plot
df$workingage = df$AGEP<65 #for 2nd plot
df$workingage[df$workingage == TRUE] = 'U65'
df$workingage[df$workingage == FALSE] = 'O65'

###CUTTTT
#list of number of categories in each var
#lenCats = c()
#n=1;for (i in vars) { p = table(df[,i]); lenCats[n] = length(p); n = n+1} 

introduction = "In this case study, we use Census Bureau data from 2023 in Massachusetts (MA) to overview the demographic population and determine which demographic factors play an important role in determining employment status."

dataoverview = "Our data comes from the American Community Survey (ACS) Public Microdata Sample (PUMS) and can be found at: 
https://www2.census.gov/programs-surveys/acs/data/pums/2023/1-Year/  
We used the 1-year person level data in 2023 whose documentation can be found at:
https://www.census.gov/programs-surveys/acs/microdata/documentation.html
This dataset includes many demographic characteristics such as race, disability, employment, and income." 

Motivations = "The ACS data can help us better understand the demographic groups in MA and their economic experiences. It can help us better target programming for groups that may be struggling more than others financially or in finding employment. For example, are different racial groups struggling more in finding employment even after accounting for age and disability factors. In addition, while we may be aware that disabilities make it harder for individuals to find employment, understanding what the disparity is can allow us to set a benchmark for what success looks like. "

Methods1 = "Our approach begins with an exploratory data analysis to understand the demographics of the population in MA in 2023. We look at demographic proportions of the adult population. In addition, we consider these proportions for the working aged population exclusively. This allows us to take a closer look at this population who are far more likely to be in the labor force. For each demographic group, we consider their proportion within each employment subgroup and compare this to their proportion of the population by calculating the residual. " 

Methods2 = p("After this initial exploratory analysis, we apply complex algorithms to identify the most important factors to employment status and attempt to predict whether an individual is in the labor force. We use both decision trees and logistic regression for both the general population and the working aged population.")

TOC = p(br(),strong('Shiny App Navigation:'),br(),em('Sidebar: '), 'Use the tools in the sidebar to alter the parameters used to create visualizations and change the results of the machine learning models',br(),em('Visualize: '), 'See the results of interactive plots and basic statistics of the data for the general population.',br(),em('Data Details: '),'Learn about our data and its variables and their proportions of the general and working aged population.',br(),em('EDA: '), 'A discussion of the results of our exploration of the residuals for our demographic variables.',br(),em('Models and Working Aged Models: '), 'The results of our statistical models and their assessment.',br(),em('Methods details: '),'Read more about the machine learning algorithms we chose and how we use them in this study.')




#----------------Next tab

Datadetails = "The original 2023 MA ACS data set includes 73126 individuals and 287 demographic variables. For our analysis we are only considering adults (60833) and we limit the demographic variables to the categorical variables described in the following table."


variables = data.frame()
variables[1:18,1] = c("ESR","agecat","SEX","Race","HISP","VET","Marital","inSchool","SCHL","birth","ENG","citizen","self-care","hearing","seeing","living","walking","cogni")

variables[,2] = c("Employment Status", "Age Category", "Sex","Race","Hispanic", "Veteran","Marital Status","In School","Level of schooling","Gave birth in past 12 months","Speaks English","Citzenship status","Self-care Difficulty","Difficulty hearing","Difficulty seeing","Independent living Difficulty","Walking Difficulty","Cognitive Difficulty")


variables[,3] = c("Employed, Not in Labor Force, Unemployed","18-24, 25-34, 35-49, 50-64, 65-74, 75+","Male (M), Female (F)","White, Black, Asian, Two+ races, Other race","Hispanic, Not Hispanic","vet, not vet","Spouse present, Spouse absent, Widowed, Divorced, Separated, Never married","In school, Not in school","1 no HS (high school), 2 HS, 3 Some college, 4 Associates,5 Bachelors, 6 Masters/Prof, 7 PhD","Yes birth, no birth","Only English, Fluent English, Little/no English","Born citizen, Naturalized, Not a citizen","Diff care, care","Diff hearing, hearing","Diff seeing, seeing", "Diff living, living", "Diff walking, walking","Diff congit, cognit")

names(variables) = c("Name","Description","Values")


Datatables = "The following tables and short summary outline the proportions of each variable in the population and in the working aged population (defined as adults less than 65 years old in this study)."


ProportionsText = c("ESR - In our case study, we will be predicting employment status. The unemployment rate is low, around 2% while the employed make up the majority of the population with 'not in labor force' just over a third of the general population and down to 21% of working aged individuals.
Our analysis will explore what demographics are more likely to fall into each of these categories.", "Cit - The vast majority of the population are born citizens with only 11% naturalized and 7-9% 'Not a citizen.'",
                    rep("Disabilities - Our data includes 6 variables that identify if an individual has a disability. In the general population, 14% have at least one of these disabilities. This number drops to 6% for people less than 65. For each individual variable, over 90% of the population does not have that difficulty. Difficulty becomes even less common among the working aged group. Cognitive difficulty is the most common among this population at 6%.",6),
                    "ENG - The English speaking level variable is similar to the citizenship status variable with almost 80% of the population speaking only English and slightly less for working aged. However, a higher proportion are fluent in English (versus naturalized) at between 17-20% and only 4% with little or no English.",
                    "birth - Only 1% of the population had a baby in the last 12 months.",
                    rep("School - Almost 90% of the population is not in school and this decreases to 85% for working aged. As for the level of schooling, the highest proportions come from high school diploma, bachelors, and masters/professional degree with 'HS' at nearly 40%. PhDs are the smallest group at only about 3% while associates and 'no HS' are around 7%.",2),
                    "SEX - While genders are split almost in half, there is slightly more females especially among the older population.",                  
                    "HISP - About 10% of the population in MA is Hispanic.",
                    "Marital - Married with spouse present (48, 46%) and never married (33, 42%) are the most common categories in marital status with never married significantly more common among working aged. Divorced is the next largest (10, 8%) and widowed (6, 1%) which is much smaller in working aged. Separated (1%) and spouse absent (3, 2%) are the least common.",
                    "Race - White is the most common race by far (70, 75%). The other categories Asian, Black, Other race and Two+ races are between 5 and 9%.",                                    "agecat - The population age is pretty evenly spread with slightly more individuals being middle-aged.",                                "VET - Only about 6% of the population is a vet which drops to 3% for working aged.")



#Code For residuals master table
residuals = data.frame()
n = 1

for (i in vars) {
  #perc of general population
  perc = round(table(df[,i])/dim(df)[1],2) #calculate percent

  x = 'ESR'

  #table of employment portion for general pop
  perc2 = table(df[,c(i,x)])
  for (k in 1:3){perc2[,k] = round(perc2[,k]/sum(perc2[,k]),2)}

  for (k in 1:3){perc2[,k] = perc2[,k] - perc}


  residuals[n:(n-1+length(perc)),1] = names(perc)
  residuals[n:(n-1+length(perc)),2:4] = perc2
  residuals[n:(n-1+length(perc)),5] = perc
  
  #make working age variables for this demog    
  y2 = 'workingage'
 df[,paste0(i,"U65")] = interaction(df[,i],df[,y2])
  y = paste0(i,"U65")
  
  
  #make table working age population percent of demogs
  perc = table(df[,c(paste0(i,"U65"))])
  perc = round(perc/sum(df$workingage=="U65"),2)
  perc = perc[-c(1:dim(perc)[1]/2)]
  
  
  #set demogs interaction with age and see percent employment
  #then plot for working aged employment per demog
  x = 'ESR'

  perc2 = table(df[df$workingage=="U65",c(y,x)])
  for (k in 1:3){perc2[,k] = round(perc2[,k]/sum(perc2[,k]),2)}
  perc2 = perc2[-c(1:dim(perc2)[1]/2),]

  for (k in 1:3){perc2[,k] = perc2[,k] - perc}

  
  residuals[n:(n-1+length(perc)),6] = names(perc)
  residuals[n:(n-1+length(perc)),7:9] = perc2
  residuals[n:(n-1+length(perc)),10] = perc
  n = n+length(perc) 

  
}

colnames(residuals) = c('Demog','Employed','Not in Labor Force','Unemployed','Pop. prop','Under 65','EmployedU','Not in Labor ForceU','UnemployedU','Under 65 prop')

residuals = residuals[-c(1:3),]


#------NEXT TAB EDA
EDA1 = "Our aim in this study is to discover which demographic variables may best predict employment status. Our dataset includes many variables and possible interactions so it is difficult to see what might be making the biggest difference. To help us do a basic exploration, this section will compare the proportion of the population for each variable with its proportion for each employment subgroup If that variable does not make a difference to employment status, we would expect the proportions to be the same. For example, if females are 52% of the population, we would also expect them to be 52% of the employed population. However, if for some reason, females are less likely to be employed than males, we would expect their proportion to be less than 52%. For this reason we will be looking at the residuals between the proportion of the population and the proportion of the employment subgroup. A negative number means that in our data it is less likely for individuals in the group to have that employment status and the opposite is true for a positive residual. Another important note is that for groups with large proportions to begin with, a small residual only affects a small part of this group whereas groups that are already uncommon, say less than 20% of the population, a small residual like 2% actually affects a significant part of this group."

EDA2 = "The following tables show either the negative or positive residuals for each group for that particular employment status. They show results for both the general population and the working aged. First a table for demographics that make up less than 20% of the general population (this may be different in the working aged population) is shown followed by those with larger proportions of the populations."


EDA3="The negative residuals for the employed population show that, in our data, the smaller groups that are less likely to be employed include individuals:
  with any disability, that speak little/no English,  with no high school diploma or that are in school, that are divorced, widowed, or have a spouse that is absent, that are older (>64) or younger (<25), and that are veterans.
This changes for the working aged only in that marital status and being a vet doesn't make a difference and being Black becomes negative." 

EDA4 = "For the larger demographics, a small portion of them seem to be less likely to be employed for the following demographics: those that are born citizens, speak only English, have only a high school diploma, are female, or white. For the working aged, only the high school level continues to be less likely. In addition, being white actually switched to a positive residual when not considering the elderly. Being never married  and and being between 50-64 years old is also negative for this population but it is positive in the general population when including the elderly."

EDA5="The positive residuals for the employed population show that, in our data, the smaller groups that are more likely to be employed include individuals:
that are not citizens, are fluent in English, have higher level educations, or are between 25-34 years old. For the working aged, only a masters degree or being 25-34 years old continues to make a difference."

EDA6 = "For the larger demographics, there are far more variables that have a positive residual. These include individuals:
without disabilities, that are not in school or have a bachelors, that are male, that were never married or have a spouse present, between the ages of 35-64, or are not a vet. For the working aged, male and not being a vet are no longer positive. Being never married or 50-64 years old switches to negative. In addition, white which was negative for the general population becomes positive. "


EDA7="The negative residuals for the population that is not in the labor force show that, in our data, the smaller groups that are less likely to not be in the labor force (i.e. more likely to be in the labor force) include individuals:
that are naturalized or not a citizen, fluent in English, have higher level degrees, are Hispanic, Asian/Other race/Two+ races, or are between 25-34 years old. For the working aged having an associates degree is also negative and divorced is negative while in the general population it is positive."

EDA8 = "For the larger populations that are less likely to not be in the labor force, there are again for more variables that include individuals:
without disabilities, that are not in school, that have a bachelors, are Male, are never married or have a spouse present, are between 35-64, or are not a vet. For working aged not being a vet is not negative and never married switches to positive (more likely not to be in labor force). In addition speaking only English, being not Hispanic, or being white were positive for the general population but switch to negative for the working aged. Not giving birth didn't make a difference for the general but also became negative for the working aged."

EDA9 = "The positive residuals for the population that is not in the labor force show that, in our data, the smaller groups that are more likely to not be in the labor force include individuals:
  that have a any disability, speak little/no English, are in school or have no high school, are divorced or widowed, are between 18-24 or 65+ years old, or are a vet. For the working aged several demographics are negative in the general population and switch to positive (more likely to not be in the labor force) for the working aged including individuals:
  that are not citizens, are fluent in English, are Hispanic, or are Asian/other race/two+ races. In this group, being a vet no longer matters and being divorced switches to negative. In addition, giving birth, being separated or having a spouse absent, and being black become positive for the working aged. Thus, all non-white races are more likely to not be in the labor force for the working aged." 

EDA10="For the larger populations that are more likely to not be in the labor force, we have:
  born citizens, only speaks English, high school diploma, female, not Hispanic, or white. For the working aged, speaking only English, not being Hispanic, and white become negative. In addition, never married and 50-64 years old are negative for the general population and switch to positive for working aged. "

EDA11="Finally, we consider the residuals for the unemployed. The negative residuals for the unemployed population show that, in our data, the smaller groups that are less likely to be unemployed include individuals:
  that are naturalized, have any disability except seeing and cognitive, have no high school diploma or any college level degree or higher, have a spouse absent or are widowed, are older than 64 or are a vet. For the working aged, results are similar but difficulty seeing becomes negative while difficult hearing or living independently no longer matters, as well as associates degree, spouse absent or widowed."

EDA12="For the larger populations that are less likely to be unemployed, we have:
  born citizen, not have a cognitive disability, only speaking English, being not in school or have a bachelors, being female, not Hispanic, having a spouse present, being white, or being 50-64 years old. For the working aged, born citizen no longer matters and being 35-49 years old is positive for the general population but switches to negative for the working aged. "

EDA13="The positive residuals for the unemployed population show that, in our data, the smaller groups that are more likely to be unemployed include individuals:
  that are not a citizen, have a cognitive disability, are fluent in English, are in school, are Hispanic, are divorced or separated, are Black/Other race or Two+ races, or are between 18-34 years old. The working aged is similar except that having no high school degree, while negative for the general population, becomes positive. "

EDA14="For larger populations, the demographics that are more likely to be unemployed include:
  not having a disability other than cognitive or seeing, having a high school diploma, being male or never married with a very high residual, being between 35-49 years old, or not being a vet. For the working aged, no hearing or living independently disability no longer matters but seeing does and being between 35-49 becomes negative. "

EDA15="Another way to find the largest differences in these residuals is to consider the ratios with their proportion of the population which takes into account the size of the demographic group compared to the size of the residual. This would be useful for future work to help understand the many variables that may contribute to employment status."


#-----Next TAB: Explanation of Decision Tree and Logistic Regression

MethodDetails1 = p(br()," In our study, we asked: what demographic factors help predict employment status? With so many variables and so many complex interactions under the hood, it is hard to get a full picture of how these variables influence employment status. To help us answer this question we used two methods in our analysis: Decision trees and Logistic regression. This section discusses each of them in detail.", br(),br(),

strong("Overview:"),br(),
"Both algorithms are supervised learning algorithms that can be used for classification. While logistic regression can only be used to predict binary outcomes (e.g. 0 or 1, yes or no), decision trees can be somewhat more flexible to allow for more than two categories. However, we will limit our analysis to the two outcomes: In the labor force (L) or Not in the labor force (N).",br(), 

"Some binary applications include:",br(),  
"-Predicting if someone will default on a loan",br(),
"-Predicting if an email is spam or not",br(),
"-Predicting if someone is employed or not (our case)",br(),br(),

"While logistic regression models probabilities of an outcome, decision trees output the decision (what category is predicted it will be) but both can provide the opposite as well. Both can also work with input variables that are categorical and/or continuous. In our case, we only use categorical variables.",br(),br(),

strong("How they work"),br(),

em("Logistic Regression:"),br(),
"The model sets the log odds of the outcomes equal to a linear function of the predictors. The coefficients on these predictors are calculated to best fit the data and show the effect of each predictor on the log odds of the outcome which can be converted to the probability of the outcome for the given input. The model makes classifications by setting a threshold on the probability. Found in the 'models' tabs, the summary of the logistic regression model shows us the relative size and direction of impact of each predictor on the outcome.",br(),br(),

em("Tree:"),br(),
"Decision trees begin with all the training data in one node and then make decisions about each split based on an algorithm to maximize 'purity' (that observations within each class are as similar as possible to each other). It then splits into two 'child' nodes and continues the process for each node recursively until they no longer improve the model or another stopping criteria is set like max tree depth. Found in the 'models' tabs, the tree diagram is a flow chart that shows the number of splits for each node that were determined. One node means there were no splits and all predictions are of the more likely class. Each node in the diagram shows the class that is predicted if you stop at that node (in our case, 1 = 'L', 0 = 'N'), the predicted probability of each class, and the percentage of observations in that node.",br(),br(),

strong("Pros and Cons"),br(),
em("Logistic regression"),br(), 
"Pros:",br(),
"-Useful for understanding the relative impact of each variable on the outcome in a concise readable summary table",br(),
"-Redundant variables do not reduce the predictive power by much",br(),
"-Gives predicted scores for each observation",br(),br(),

"Cons:",br(),
"-Assumes each variable affects the probability (log-odds) of the outcome linearly and additively. It is better when the variables have a linear relationship with the outcome although transformations and modeling interactions can help if they do not.",br(),  
"-Does not handle missing values or discontinuous relationships well. In our case, an example of a discontinuous relationship could be age since when social security kicks in individuals may be much more likely to leave the work force and retire. Logistic regression will struggle to capture this since it assumes linearity. In our dataset, we create a categorical variable for age.",br(), 
"-Does not work well with categorical variables with many different categories. Combining categories can help.",br(),br(),

em("Trees"),br(),
"Pros:",br(),
"-Very easy to interpret and follow",br(),
"-Can better model non-linear, complex relationships and accounts for interactions between variables unlike logistic regression where they must be modeled directly",br(), br(),

"Cons:",br(),
"-Can lead to overfitting since trees can become very large and account for every situation. This can be limited by 'pruning' (limiting the growth of the tree)",br(),
"-Not ideal for continuous variables since these can typically be split many more times. Again setting limits on how the tree grows such as minsplits (minimum observations necessary for node to be split) or binning variables helps.",br(),br(), 

strong("Model Assessment:"),br(),
"In our 'models' tabs, we assess our results with the AUC which can only be used for binary outcomes so we limit our employment status to whether an individual is in the labor force. The AUC is the area under the ROC curve which plots the true positive rate (Percent of actual positives that the model predicted correctly) against the false positive rate (Percent of all the actual negatives that the model predicted positive). The AUC tells us how well the model predicts with 1 being the highest score, 0 the lowest, and .5 being no better than classifying randomly. We also consider the correct classification rate (CCR, also called accuracy) of the model which is the proportion of total predictions that were correct and is determined by the confusion matrix (also depicted) that shows the number of predictions that are correct (L/L or L/1) and incorrect (L/N or L/0).",br(),br(),

strong("Interactive exploration and some results:"),br(),
"To test out different models and see their results, refer to the 'models' tabs and the side bar. Many of our variables do help predict employment status. However, for the working aged, age by itself is not a useful predictor of employment status with our tree model performing no better than a random model would. Running the model that includes all variables shows that all 4 models perform reasonably with logistic regression for the general population performing particularly well (AUC: .87, CCR: .81). When using all variables, we also find that certain disabilities and schooling play the largest roles in employment status for all models with age being a large factor when including the elderly in our general population model. Removing disabilities shows again the importance of schooling. In addition, our logistic regression model highlights the importance of factors like race which we saw in our residuals EDA analysis. Further studies could expand this analysis by considering these demographics with the outcome of income bracket."
)
