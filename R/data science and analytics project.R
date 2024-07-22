data=read.csv(file.choose(),header=T)
head(data)

#removing the variables from the dataset
data=data[, !(names(data) %in% c('CustomerId','Surname'))]
#information
str(data)
summary=summary(data);summary
#check for missing values of the data frame
a=is.na(data)
# Counting  missing values in each column in the dataset
col_missing = colSums(a)
print(col_missing)

data= na.omit(data)

a=is.na(data)
col_missing=colSums(a)
print(col_missing)


#EDA
CreditScore=data$CreditScore

EstimatedSalary=data$EstimatedSalary


#Scatter plot and histogram

plot(EstimatedSalary,CreditScore, main = "Scatter Plot", xlab = "EstimatedSalary", ylab = "CreditScore")
corr=cor(EstimatedSalary,CreditScore);corr

hist(CreditScore , main="Histogram",xlab="CreditScore",col="lightblue")

hist(EstimatedSalary , main="Histogram",xlab="EstimatedSalary",col="lightblue")

hist(Balance , main="Histogram", xlab="Balance", col="lightblue")
str(data)
#Info
dimension=dim(data);dimension
columns=colnames(data);columns

Exited =data$Exited
Exited_counts =table(Exited);Exited_counts

Exited_prop =prop.table(Exited_counts)*100
print(Exited_prop)

#boxplot [Balance]
boxplot(data$Balance,col="yellow",main="box plot for Balance",ylab="Balance")
boxplot(Balance ~ Exited, data =data,main = "Balance Box plot", xlab = "Exited", ylab = "Balance")

#bar plot (Exited)
barplot(table(data$Exited),col="yellow",main="Exited count")

#variables to convert to categorical
#Loop through each variable and convert to factor
variables=c('HasCrCard')
for(variable in variables){
data[[variable]]=factor(data[[variable]],levels=c(0,1),labels=c("No","Yes"))
}
var=c('IsActiveMember')
#Loop through each variable and convert to factor
for(variable in var){
data[[variable]]=factor(data[[variable]],levels=c(0,1),labels=c("No","Yes"))
}
str(data)
x_factor=Filter(is.factor,data)
head(x_fact)
x_chac=Filter(is.character,data)
head(x_chac)
x_cat=cbind(x_factor,x_chac)
head(x_cat)
x_num=Filter(is.numeric, data)
head(x_num)

#loading the ggplot 2 package
install.packages("ggplot2")
library(ggplot2)

#ggplot for estimated salary by exited and gender
ggplot(data , aes(x=Exited,y=EstimatedSalary, fill=Gender))+geom_bar(stat="identity",position="dodge")+labs(title="estimated salary by exited and gender",x="Exited",y="EstimatedSalary")

str(data)

#load the dplyr and tidyr packages
install.packages("dplyr")
library(dplyr)
install.packages("tidyr")
library(tidyr)
#creating  dummy variables
dummy_encoded =model.matrix(~ . - 1, data =x_cat) %>% as.data.frame()
#displaying the top 5 rows of the dummy variables
head(dummy_encoded)
# Dropping the first column to remove a dummy variable
dummy_encoded=dummy_encoded[, -1]
head(dummy_encoded)
# Attaching the dummy variables to the original dataset
df_dummy = cbind(x_num, dummy_encoded)
head(df_dummy)
dim(df_dummy)

# Separate independent and dependent variables
X = df_dummy[, !(colnames(df_dummy) %in% c('Exited'))]
y = df_dummy[c('Exited')]
#set the seed for reproductibility
set.seed(1)

install.packages("caTools")
library(caTools)

# Split data into train data and test data
split_vector = caTools::sample.split(df_dummy$Exited, SplitRatio = 0.8)
X_train = subset(X, split_vector == TRUE)
X_test = subset(X, split_vector == FALSE)
y_train = subset(y, split_vector == TRUE)
y_test = subset(y, split_vector == FALSE)
head(X_train)
#logistical regression model
model=glm(Exited~ .,data=cbind(X_train,y_train),family=binomial)
summary(model)
#Accuracy using a threshold of 0.5
predictTest=predict(model,newdata=X_test,type="response")
table(y_test$Exited,predictTest>=0.5)
accuracy=(1523+70)/(1523+93+70+314);accuracy
