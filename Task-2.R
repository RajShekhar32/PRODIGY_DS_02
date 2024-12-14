#Task-02
#Perform data cleaning and exploratory data analysis (EDA) on a dataset of your choice, such as the Titanic dataset from Kaggle. Explore the relationships between variables and identify patterns and trends in the data.
#Sample Dataset :- https://www.kaggle.com/c/titanic/data

#Load the dataset
data<-read.csv("C:\\Users\\user\\OneDrive\\Desktop\\Raj\\train.csv");head(data)
#Data cleaning
#Inspect the dataset
str(data)
summary(data)
#check the missinf value
colSums(is.na(data))
# Fill missing values
data$Age[is.na(data$Age)]<-median(data$Age, na.rm = TRUE)
# Drop the Cabin column (too many missing values)
data<-data%>%select(-Cabin)
#convert relavent colunms to factor
data$Survived<-as.factor(data$Survived)
data$Pclass<-as.factor(data$Pclass)
data$Sex<-as.factor(data$Sex)
data$Embarked<-as.factor(data$Embarked)

#Exploratory Data Analysis
#Distribution of Survived
table(data$survived)
# Load the necessary library
library(ggplot2)
#Bar plot for survival counts
ggplot(data,aes(x=Survived))+geom_bar(fill="red")+labs(title="Distribution of Survival")
#Distribution Of Age
table(data$Age)
ggplot(data,aes(x=Age))+geom_histogram(bins=30,fill="red",color="black")+labs(title="Distribution of Age")
#Age distribution by survival
ggplot(data,aes(x=Age,fill=Survived))+geom_histogram(bins=30,position = "identity",alpha=0.6)+labs(title="Age Distribution by Survival",x="Age",y="Count")+scale_fill_manual(values=c("blue","red"))
#Fare distribution by Survival
ggplot(data,aes(x=Fare,fill=Survived))+geom_histogram(bins=30,position = "identity",alpha=0.6)+labs(title="Fare Distribution by Survival",x="Fare",y="Count")+scale_fill_manual(values=c("tomato","lightblue"))

#Bivariate Data Analysis
#Survival by Sex
ggplot(data,aes(x=Sex,fill=Survived))+geom_bar(position="dodge")+labs(title="Survival by Sex")
#Survival by Embarkation port
ggplot(data,aes(x=Embarked,fill=Survived))+geom_bar(position="dodge")+labs(title="Survival by Embarked")

#Survival by Pclass
ggplot(data,aes(x=Pclass,fill=Survived))+geom_bar(position="dodge")+labs(title="Survival by Passenger Class")
#Correlation between numerical variables
library(corrplot)
numeric_cols<-select(data,Age,Fare)
corrln_matrix<-cor(numeric_cols,use="complete.obs")
#plot the correlation matrix
corrplot(corrln_matrix,method="circle")
#Final summary after cleaning
summary(data)
#Boxplot to explore trends
#Boxplot of Fare by survival
ggplot(data,aes(x=Survived,y=Fare,fill=Survived))+geom_boxplot()+labs(title="Fare by Survival",x="Survived",y="Fare")
#Plotting Age,Passenger Class and Survival
ggplot(data,aes(x=Age,y=Pclass,color=Survived))+geom_point(alpha=0.7)+labs(title="Age vs Passanger Class by Survival",x="Age",y="Passenger Class")
#Cross-tab for Sex and Survived(Contingency table)
table(data$Sex,data$Survived)
#Cross-tab between Pclass and Survived
table(data$Pclass,data$Survived)


