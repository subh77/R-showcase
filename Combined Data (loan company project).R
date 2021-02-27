rm(list = ls())
train = read.csv("train.csv")
head(train)
summary(train)

test <- read.csv("test.csv")
head(test)
summary(test)

test <- data.frame(test,Loan_Status=NA)
#or use this way=test$Loan_Status <- NA
head(test)


homeloan <- rbind(train,test)
summary(homeloan)

levels(homeloan$Gender)[1]=NA
levels(homeloan$Married)[1]=NA
levels(homeloan$Self_Employed)[1]=NA
levels(homeloan$Dependents)[1]=NA
summary(homeloan)

homeloan$Credit_History=as.factor(homeloan$Credit_History)
summary(homeloan)

mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}

homeloan$Gender[is.na(homeloan$Gender)] <- mode(homeloan$Gender)
homeloan$Married[is.na(homeloan$Married)] <- mode(homeloan$Married)
homeloan$Self_Employed[is.na(homeloan$Self_Employed)] <- mode(homeloan$Self_Employed)
homeloan$Credit_History[is.na(homeloan$Credit_History)] <- mode(homeloan$Credit_History)
homeloan$Dependents[is.na(homeloan$Dependents)] <- mode(homeloan$Dependents)
homeloan$Dependents <- as.factor(ifelse(homeloan$Dependents == 0,0,1))
homeloan$LoanAmount[is.na(homeloan$LoanAmount)] <- median(homeloan$LoanAmount, na.rm = TRUE)
homeloan$Loan_Amount_Term[is.na(homeloan$Loan_Amount_Term)] <- median(homeloan$Loan_Amount_Term, na.rm = TRUE)
summary(homeloan)

