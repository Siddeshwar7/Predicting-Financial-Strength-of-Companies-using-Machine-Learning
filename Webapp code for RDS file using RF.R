library(caTools)
install.packages("randomForest")
library(randomForest)
library(caret)
# Connecting to the data set 
data <- read.csv("Data Set.csv",TRUE,",")
class(data)
# Removing Unwanted columns
data<-data[ ,-17:-19]
# Checking that the unwanted columns are removed 
names(data)
# Creating a new column to store the F score of a company and assigning it the value NA
data$F.score <- NA 
# Assigning score to a0 by Analyzing the Net income column if the net income of a company is +VE then the value is 1 else its assigned as 0
a0 <- ifelse(data$Net.Income>0,a0<-1,
             ifelse(data$Net.Income<0,a0<-0,NA_character_))
# Converting the a0 value to numeric from character type and assigning the value to variable - a
a<-as.numeric(a0)
# Assigning score to b0 by Analyzing the ROA column if the ROA of a company is +VE then the value is 1 else its assigned as 0
b0<- ifelse(data$ROA>0,b0<-1,
            ifelse(data$ROA<0,b0<-0,NA_character_))
# Converting the b0 value to numeric from character type and assigning the value to variable - b
b<-as.numeric(b0)
# Assigning score to c0 by Analyzing the operating cash flow column if the operating cash flow of a company is +VE then the value is 1 else its assigned as 0
c0<-ifelse(data$operating.cash.flow.2020>0,c0<-1,
           ifelse(data$operating.cash.flow.2020<0,c0<-0,NA_character_))
# Converting the c0 value to numeric from character type and assigning the value to variable - c
c<-as.numeric(c0)
# Replacing NA values with 1
c[which(is.na(c))]<-1
# Assigning score to d0 by comparing  the operating cash flow column with net income if the operating cash flow of a company is greater then the value is 1 else its assigned as 0
d0<-ifelse(data$operating.cash.flow.2020>data$Net.Income,d0<-1,
           ifelse(data$operating.cash.flow.2020<data$Net.Income,d0<-0,NA_character_))
# Converting the d0 value to numeric from character type and assigning the value to variable - d
d<-as.numeric(d0)
# Assigning score to e0 by comparing  the Long term debt of 2020 column with Long term debt 2019 if the Long term debt 2020 of a company is less than its previous year than the value is 1 else its assigned as 0
e0<-ifelse(data$Long.term.debt.2020<data$Long.term.debt.2019,e0<-1,
           ifelse(data$Long.term.debt.2020>data$Long.term.debt.2019,e0<-0,NA_character_))
# Converting the e0 value to numeric from character type and assigning the value to variable - e
e<-as.numeric(e0)
# Replacing NA values with 1
e[which(is.na(e))]<-1
# Assigning score to f0 by comparing  the current ratio of 2020 column with current ratio of 2019 if the current ratio of 2020 of a company is greater than its previous year than the value is 1 else its assigned as 0
f0<-ifelse(data$Current.Ratio.2020>data$Current.Ratio.2019,f0<-1,
           ifelse(data$Current.Ratio.2020<data$Current.Ratio.2019,f0<-0,NA_character_))
# Converting the f0 value to numeric from character type and assigning the value to variable - f
f<-as.numeric(f0)
# Assigning score to g0 by comparing  the Outstanding Shares of 2019 column with Outstanding Shares of 2020 if the Outstanding Shares of 2020 of a company is equal to its previous year than the value is 1 else its assigned as 0
g0<-ifelse(data$Outstanding.Shares.in.2019==data$Outstanding.Shares.in.2020,g0<-1,
           ifelse(data$Outstanding.Shares.in.2019!=data$Outstanding.Shares.in.2020,g0<-0,NA_character_))
# Converting the g0 value to numeric from character type and assigning the value to variable - g
g<-as.numeric(g0)
# Assigning score to h0 by comparing  the gross margin of 2020 column with gross margin of 2019 if the gross margin  of 2020 of a company is greater than its previous year than the value is 1 else its assigned as 0
h0<-ifelse(data$Gross.margin.2020>data$Gross.margin.2019,h0<-1,
           ifelse(data$Gross.margin.2020<data$Gross.margin.2019,h0<-0,NA_character_))
# Converting the h0 value to numeric from character type and assigning the value to variable - h
h<-as.numeric(h0)
# Assigning score to i0 by comparing  the Asset Turnover Ratio of 2020 column with Asset Turnover Ratio of 2019 if the Asset Turnover Ratio of 2020 of a company is greater than its previous year than the value is 1 else its assigned as 0
i0<-ifelse(data$Asset.Turnover.Ratio.2020>data$Asset.Turnover.Ratio.2019,i0<-1,
           ifelse(data$Asset.Turnover.Ratio.2020<data$Asset.Turnover.Ratio.2019,i0<-0,NA_character_))
# Converting the i0 value to numeric from character type and assigning the value to variable - i
i<-as.numeric(i0)
# Summing the values of a,b,c,d,e,f,g,h,i and populating the column F score
data$F.score <- a+b+c+d+e+f+g+h+i
data$F.score[which(is.na(data$F.score))]<-0
#Creating new Column that determines the status of every F score
for(i in 1:nrow(data)){
  if(data$F.score[i]>=7){
    data$status[i]<-"Financially Strong"
  }else if (data$F.score[i]<=3){
    data$status[i]<-"Financially weak"
  }else {
    data$status[i]<-"Financially Moderate"
  }
}
#Removing Rows containing NA values 
data1<-na.omit(data)
#Extracting Independent and dependent Variable  
x<-data1[ ,c("Net.Income","ROA","operating.cash.flow.2020","Long.term.debt.2019","Long.term.debt.2020","Current.Ratio.2019","Current.Ratio.2020","Outstanding.Shares.in.2019","Outstanding.Shares.in.2020","Gross.margin.2019","Gross.margin.2020","Asset.Turnover.Ratio.2019","Asset.Turnover.Ratio.2020")]
y<-data1[ ,c("status")]
dataset<-data.frame(x,y)
dataset$Net.Income<-as.numeric(dataset$Net.Income)
dataset$Outstanding.Shares.in.2019<-as.numeric(dataset$Outstanding.Shares.in.2019)
dataset$Outstanding.Shares.in.2020<-as.numeric(dataset$Outstanding.Shares.in.2020)
dataset$y=factor(dataset$y)
sapply(dataset,class)
table(dataset$y)

set.seed(123)
split<-sample.split(dataset,SplitRatio=0.7)
split
train<-subset(dataset,split=="TRUE")
test<-subset(dataset,split=="FALSE")

set.seed(222)
rf<-randomForest(y~.,data = train,
                 ntree=200,
                 mtry=1,
                 importance=TRUE,
                 proximity=TRUE)

# Save model to RDS file
saveRDS(rf, "Webapp.rds")
