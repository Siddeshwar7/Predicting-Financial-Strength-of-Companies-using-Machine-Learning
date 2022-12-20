library(plumber)
library(randomForest)
#* return the input
#* 
#* @get /patrol

function(messg = ""){
  list(messg = paste0("Hi I am listening '", messg, "'"))
}
## Load the model
modellist = readRDS("HH.rds")
## Lets make the predictions
#* @param Net.Income
#* @param ROA
#* @param operating.cash.flow.2020
#* @param Long.term.debt.2019
#* @param Long.term.debt.2020
#* @param Current.Ratio.2019
#* @param Current.Ratio.2020
#* @param Outstanding.Shares.in.2019
#* @param Outstanding.Shares.in.2020
#* @param Gross.margin.2019
#* @param Gross.margin.2020
#* @param Asset.Turnover.Ratio.2019
#* @param Asset.Turnover.Ratio.2020
#* @get /predict
predictions <- function(Net.Income, ROA, operating.cash.flow.2020, Long.term.debt.2019,Long.term.debt.2020,Current.Ratio.2019,Current.Ratio.2020,Outstanding.Shares.in.2019,Outstanding.Shares.in.2020,Gross.margin.2019,Gross.margin.2020,Asset.Turnover.Ratio.2019,Asset.Turnover.Ratio.2020){
  Net.Income <- as.numeric(Net.Income )
  ROA <-  as.numeric(ROA)
  operating.cash.flow.2020  <- as.numeric(operating.cash.flow.2020)
  Long.term.debt.2019  <- as.numeric(Long.term.debt.2019)
  Long.term.debt.2020  <- as.numeric(Long.term.debt.2020)
  Current.Ratio.2019  <- as.numeric(Current.Ratio.2019)
  Current.Ratio.2020  <- as.numeric(Current.Ratio.2020)
  Outstanding.Shares.in.2019  <- as.numeric(Outstanding.Shares.in.2019)
  Outstanding.Shares.in.2020  <- as.numeric(Outstanding.Shares.in.2020)
  Gross.margin.2019  <- as.numeric(Gross.margin.2019)
  Gross.margin.2020  <- as.numeric(Gross.margin.2020)
  Asset.Turnover.Ratio.2019  <- as.numeric(Asset.Turnover.Ratio.2019)
  Asset.Turnover.Ratio.2020 <-  as.numeric(Asset.Turnover.Ratio.2020)
  
  X.new <- data.frame(Net.Income= Net.Income,
                      ROA = ROA ,
                      operating.cash.flow.2020=operating.cash.flow.2020,
                      Long.term.debt.2019 = Long.term.debt.2019,
                      Long.term.debt.2020=Long.term.debt.2020,
                      Current.Ratio.2019=Current.Ratio.2019,
                      Current.Ratio.2020=Current.Ratio.2020,
                      Outstanding.Shares.in.2019=Outstanding.Shares.in.2019,
                      Outstanding.Shares.in.2020=Outstanding.Shares.in.2020,
                      Gross.margin.2019=Gross.margin.2019,
                      Gross.margin.2020=Gross.margin.2020,
                      Asset.Turnover.Ratio.2019=Asset.Turnover.Ratio.2019,
                      Asset.Turnover.Ratio.2020=Asset.Turnover.Ratio.2020)
  y.pred <- modellist$NewPredictions(modellist$modelobject,X.new)
  
  return(y.pred)
  
}

