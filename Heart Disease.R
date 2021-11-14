#Loading Packages
library(dplyr)
library(ggplot2)
library(corrplot)

#Importing the data
data <- read.csv("/Users/ismailshaikh/Documents/heart.csv")

#Exploring the data
head(data)

names(data)

glimpse(data)

summary(data) 

#Checking if we have missing values and if yes, what is the count of NULL values in the dataset
sum(is.na(data))

#Data types of the attributes
str(data)

#Performing data transformation for better representation of the data
newdata <- data%>%
  mutate(Sex = if_else(Sex == "M", 'Male', 'Female'),
         ChestPainType = if_else(ChestPainType == "TA","Typical Angina",
                                 if_else(ChestPainType == "ATA", "Atypical Angina",
                                         if_else(ChestPainType == "NAP", "Non-Anginal Pain","Asymptomatic"))),
         FastingBS = if_else(FastingBS == 1,">120","<=120"),
         ExerciseAngina = if_else(ExerciseAngina == "N","No","Yes"),
         HeartDisease = if_else(HeartDisease == 1,"Yes", "No")
  ) %>%
  mutate_if(is.character,as.factor)%>%
  dplyr::select(Age,Sex,ChestPainType,RestingBP,Cholesterol,FastingBS,RestingECG,MaxHR,ExerciseAngina,Oldpeak,ST_Slope,HeartDisease)


head(newdata)

#Number of people having Heart Disease
ggplot(newdata, aes(x = HeartDisease,fill = HeartDisease))+
  geom_bar()+
  ylab("Number of People")

#Number of Male and Female having Heart Disease and not having Heart Disease
ggplot(newdata,aes(x = Sex, y = ..count..))+
  geom_bar(aes(fill = HeartDisease))+
  facet_grid(.~HeartDisease)+
  ylab("Number of People")

#Number of Male and Female having Heart Disease
data2<- newdata%>%
  filter(HeartDisease == "Yes")

ggplot(data2,aes(x = Sex))+
  geom_bar(aes(fill = Sex))+
  ylab("Number of People")

#Age range of People with Heart Disease
data2<- newdata%>%
  filter(HeartDisease == "Yes")

ggplot(data2,aes(x = Age))+
  geom_bar(fill = "Purple")+
  ylab("Number of People")

#ChestPain V/S Number of People
ggplot(newdata,aes(x = ChestPainType,fill = ChestPainType))+
  geom_bar()+
  ylab("Number of People")+
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))

#Blood pressure for different categories of chest pain
ggplot(newdata,aes(x = FastingBS,fill = FastingBS))+
  geom_bar()+
  ylab("Number of People")+
  facet_grid(~ChestPainType)+
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))

#Blood Pressure for different categories of Chest Pain of people who have heart Disease
data2<- newdata%>%
  filter(HeartDisease == "Yes")

ggplot(data2,aes(x = FastingBS,fill = FastingBS))+
  geom_bar()+
  ylab("Number of People")+
  facet_grid(~ChestPainType)+
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5))

#Correlation

cr <- newdata[,c(1,4,5,8,10)] 
cor(cr, method = 'pearson')

cr <- newdata[,c(1,4,5,8,10)]  
cor(cr, method = 'kendall')

cr <- newdata[,c(1,4,5,8,10)] 
cor(cr, method = 'spearman')

#Correlation Plot

corrplot(cor(cr),method = "color")

#Correlation Heat Map
palette = colorRampPalette(c("green", "white", "Blue")) (20)
heatmap(x = cor(cr), col = palette, symm = TRUE)














