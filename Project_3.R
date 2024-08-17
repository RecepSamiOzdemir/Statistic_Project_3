#211805048-Recep Sami Özdemir, 211805054-Cemre Polat
#Please change a directory of setwd
#Also please use given MultRegData.txt inside of .zip file
setwd("C://Users//recep//Documents//Ders Notları//4. Dönem//Statistical Programming//Project 3 Files//Project 3")
datas <- data.frame(read.table("MultRegData.txt"))
colNames <- c("IndNo","Y","X1" ,"X2", "X3", "X4", "X5", "X6", "X7")
colnames(datas) <- colNames
#sumFun is our summation function and it will sum all datas in given one data.frame
sumFun <- function(x) {
  vars <- 0
  for (i in 1:length(x)) {
    if(!(is.na(x[i]))) {
      vars <- vars + x[i]
    }
  }
  return(vars)
} 
#countNa function is counting how much NA data inside in a given data.frame This dataset doesnt have any NA variable but we add this function to any case
countNa <- function(x) {
  countNa <- 0
  for(i in 1:length(x)) {
    if(is.na(x[i])) {
      countNa <- countNa + 1
    }
  }
  return(countNa)
}
#meanFun is a function calculates mean of given one data.frame
meanFun <- function(x) {
  mean <- sumFun(x) / (length(x) - countNa(x))
  return(mean)
}
#varianceFun is a function will calculate given one data.frames variance 
varianceFun <- function(x) {
  variance <- 0
  for(i in 1:length(x)) {
    if(!is.na(x[i])) { 
      variance <- variance + ((x[i] - meanFun(x))^2)  
    }
  }
  variance <- variance / ((length(x) - countNa(x)) - 1)
  return(variance)
}
#covarianceFun is a function will calculate given data.frames covariance
covarianceFun <- function(x,y) {
  meanX <- meanFun(x)
  meanY <- meanFun(y)
  
  lengthX <- length(x) - countNa(x)
  lengthY <- length(y) - countNa(y)
  
  if(length(x) == length(y) && lengthX == lengthY) {
    return (sumFun((na.omit(x) - meanX) * (na.omit(y) - meanY)) / (lengthX - 1))
  } else {
    return ("Covariance is incalculable.")
  }
}
#linearRegressionFun is first questions function. It can calculate: Intercept,Regression Coefficents , Y estimates and e estimates, TSS's ,RMSS's , RSS's and R squares
#It gets to string inputs. "input"'s syntax must be like " input = "X1,X2,X3" " or just " "X1,X2,X3" ", output also has a same syntax but it has to be give what output you want to return, Like "RSS" or output = "R-square"
linearRegressionFun<- function(input, output) {
  if(missing(output)){
    cat("When you call a function please give which output you want as a output = ''  ''\nOutput options are:\nIntercept\tRegressionCoefficents\tY_estimate\te_estimate\tTSS\tRMSS\tRSS\tR_square")
  }
  
  Dependent <- c(datas$Y)
  input_list <- unlist(strsplit(input, ","))
  Independents <- c()
  
  if("X1" %in% input_list){
    Independents <- append(Independents, list(datas$X1))
  }
  if("X2" %in% input_list){
    Independents <- append(Independents, list(datas$X2))
  }
  if("X3" %in% input_list){
    Independents <- append(Independents, list(datas$X3))
  }
  if("X4" %in% input_list){
    Independents <- append(Independents, list(datas$X4))
  }
  if("X5" %in% input_list){
    Independents <- append(Independents, list(datas$X5))
  }
  if("X6" %in% input_list){
    Independents <- append(Independents, list(datas$X6))
  }
  if("X7" %in% input_list){
    Independents <- append(Independents, list(datas$X7))
  }
  
  i_index <- length(Dependent)
  j_index <- length(Independents)
  y_mean <- meanFun (datas$Y)
  
  Y_matrix <- matrix(data = datas$Y, nrow = i_index, ncol = 1)
  X_matrix <- matrix(data = c(rep(1, i_index), unlist(Independents)), nrow = i_index, ncol = (j_index + 1))
  B_matrix <- matrix(data = NA, nrow = i_index, ncol = 1)
  e_estimate <- matrix(data = NA, nrow = i_index, ncol = 1)
  Y_estimate <- matrix(data = NA, nrow = i_index, ncol = 1)
  TSS <- NULL
  RMSS <- NULL
  RSS <- NULL 
  R_square_matrix <- NULL
  
  B_matrix <- solve(t(X_matrix) %*% X_matrix) %*% t(X_matrix) %*% Y_matrix
  Intercept <- B_matrix[1,1]
  RegressionCoefficents <- B_matrix[-1, drop = FALSE]
  
  Y_estimate <- X_matrix %*% B_matrix
  
  e_estimate <- Y_matrix - Y_estimate
  
  TSS <- sumFun((Y_matrix - y_mean)^2)
  RMSS <- sqrt(sumFun(e_estimate^2) / i_index)
  RSS <- sumFun(e_estimate^2)
  R_square_matrix <- 1 - (RSS/TSS)
  
  result <- switch (output,
  "Intercept" = Intercept,
  "RegressionCoefficents" = RegressionCoefficents,
  "Y_estimate" = Y_estimate,
  "e_estimate" = e_estimate,
  "TSS" = TSS,
  "RMSS" = RMSS,
  "RSS" = RSS,
  "R_square" = R_square_matrix,
  cat("When you call a function please give which output you want as a output = '' ''\nOutput options are:\nIntercept\tRegressionCoefficents\tY_estimate\te_estimate\tTSS\tRMSS\tRSS\tR_square")
                    
  )
  return(result)
}

#ArrangedList function is a second questions function. It calculates all combinations of all variables and using linearRegressionFun to calculations and combine all of them in one matrix and print it
ArrangedList <- function(){
  
  colNames <- colnames(datas)
  X_columns <- colNames[grep("^X", colNames)]
  
  allCombinations <- c()
  NumberOfVariables <- vector("list", length = 0)
  
  for (i in 1:length(X_columns)) {
    combinations <- combn(X_columns, i)
    for (j in 1:ncol(combinations)) {
      combo <- paste(combinations[, j], collapse = ",")
      allCombinations <- c(allCombinations, combo)
      combo_list <- unlist(strsplit(combo, ","))
      NumberOfVariables[[combo]] <- length(combo_list)
      
    }
  }
  
  ListMatrix <- matrix(nrow = length(allCombinations), ncol = 7)
  colnames(ListMatrix) <- c("Model","Number Of Variables", "Variable (X) Name", "TSS", "RMSS", "RSS","R-Square")
  
  ListMatrix[, 1] <- 1:length(allCombinations)
  ListMatrix[, 2] <- unlist(sapply(NumberOfVariables, function(x) paste(x, collapse = ",")))
  ListMatrix[, 3] <- allCombinations
  for (i in 1:length(allCombinations)) {
    ListMatrix[i, 4] <- linearRegressionFun(allCombinations[i], output = "TSS")
    ListMatrix[i, 5] <- linearRegressionFun(allCombinations[i], output = "RMSS")
    ListMatrix[i, 6] <- linearRegressionFun(allCombinations[i], output = "RSS")
    ListMatrix[i, 7] <- linearRegressionFun(allCombinations[i], output = "R_square")
  }
  
  OrderColumn <- which(colnames(ListMatrix) == "R-Square")
  
  ArrangedIndex <- order(ListMatrix[, OrderColumn], decreasing = TRUE)
  
  ArrangedMatrix <- ListMatrix[ArrangedIndex, ]
  
  return(ArrangedMatrix)
}