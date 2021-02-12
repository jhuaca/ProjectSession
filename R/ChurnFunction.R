ChurnFuntion <- function(merge_table, customerId){

customers <- fread("data_customer.csv")
personal <- fread("data_personal.csv")

merge_table <- merge(customers, personal, by="CustomerId")

merge_table[, Exited:=as.factor(Exited)]
merge_table[, Gender:=as.factor(Gender)]


formula_churn <- merge_table$Exited ~ merge_table$CreditScore + merge_table$Gender + merge_table$Tenure + merge_table$Balance +
    merge_table$NumOfProducts + merge_table$HasCrCard + merge_table$IsActiveMember + merge_table$EstimatedSalary

predictor <- glm(formula_churn, family="binomial")

#Predicting using predict() and add into new table
prediction <- predict(predictor, merge_table, type="response")

#adding in table column "Prediction"
merge_table[, Prediction:=prediction]


churn_prob <- merge_table[CustomerId==customerId, Prediction]

if(customerId == merge_table[, CustomerId]){
  return(churn_prob)
} else {
  print("No customer found")
}
}
