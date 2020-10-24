#Loading Libraries
library(dplyr)
library(arules)
library(ggplot2)
library(plotly)
library(arulesViz)
library(MASS)
library(caret)
library(visNetwork)
library(igraph)
library(reshape2)
library(moments)
library(GGally)
library(knitr)
library(stringr)
library(readxl)
library(scorecard)
library(tidyverse)
library(caTools)
library(e1071)
library(ROCit)


#Loading Data Set
Insurance_DataSet <- read_excel("C:/Users/Asus/Downloads/Insurance_DataSet.xlsx", 
                                col_types = c("numeric", "numeric", "text", 
                                                    "numeric", "numeric", "numeric", 
                                                    "text", "text", "text", "numeric", 
                                                    "text", "text", "numeric", "text", 
                                                    "text", "numeric", "text", "text", 
                                                    "text", "numeric", "text", "text", 
                                                    "text", "text", "text", "text", "text", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "text", "numeric", "numeric", "numeric", 
                                                     "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "text", "numeric", 
                                                    "numeric", "numeric", "text", "numeric", 
                                                     "text", "numeric", "numeric", "text", 
                                                    "text", "numeric", "numeric", "numeric"))

# Checking Data Frame

dim(Insurance_DataSet)
head(Insurance_DataSet)

#Checking Uniqueness of records and removing duplicates
Insurance_DataSet<-dplyr::distinct(Insurance_DataSet)

#Handling Missing Values

colSums(is.na(Insurance_DataSet))

Insurance_DataSet$Insurance_Description <- ifelse(is.na(Insurance_DataSet$Insurance_Description),"No Insurance",Insurance_DataSet$Insurance_Description)

Insurance_DataSet$code <- ifelse(is.na(Insurance_DataSet$code),0,Insurance_DataSet$code)

Insurance_DataSet$category <- ifelse(is.na(Insurance_DataSet$category),"No Insurance",Insurance_DataSet$category)

#Handling -VE Values (imputing by rounded mean value) & Outliers

# Handling -ve age values
Insurance_DataSet %>% dplyr::filter(.,Age>0) %>% dplyr::select(Age) %>% dplyr::summarise(.,mean_age=mean(Age))

Insurance_DataSet$Age<- ifelse(Insurance_DataSet$Age<0,44,Insurance_DataSet$Age)

#Handling -ve Mortage Outstanding Balance ; Assumed to be non negative
Insurance_DataSet %>% dplyr::filter(.,Total_outstanding_balance__mortg>=0) %>% dplyr::select(Total_outstanding_balance__mortg) %>% dplyr::summarise(.,mean_mort_bal=mean(Total_outstanding_balance__mortg))

Insurance_DataSet$Total_outstanding_balance__mortg<- ifelse(Insurance_DataSet$Total_outstanding_balance__mortg<0,65,Insurance_DataSet$Total_outstanding_balance__mortg)


#Handling -ve public Info CCJ Balance ; Assumed to be non negative
Insurance_DataSet %>% dplyr::filter(.,Total___Public_Info___CCJ____ban>=0) %>% dplyr::select(Total___Public_Info___CCJ____ban) %>% dplyr::summarise(.,mean_CCJ_bal=mean(Total___Public_Info___CCJ____ban))

Insurance_DataSet$Total___Public_Info___CCJ____ban<- ifelse(Insurance_DataSet$Total___Public_Info___CCJ____ban<0,0,Insurance_DataSet$Total___Public_Info___CCJ____ban)

#Handling -ve Value public Info CCJ  ; Assumed to be non negative

Insurance_DataSet$Total_value__Public_Info___CCJ__<- ifelse(Insurance_DataSet$Total_value__Public_Info___CCJ__<0,5,Insurance_DataSet$Total_value__Public_Info___CCJ__)

#Handling -ve Time since most recent public In  ; Assumed to be non negative

Insurance_DataSet$Time_since_most_recent_Public_In<- ifelse(Insurance_DataSet$Time_since_most_recent_Public_In<0,90,Insurance_DataSet$Time_since_most_recent_Public_In)

#Handling -ve Total Value CAIS_8_9  ; Assumed to be non negative

Insurance_DataSet$Total_value__CAIS_8_9s<- ifelse(Insurance_DataSet$Total_value__CAIS_8_9s<0,19,Insurance_DataSet$Total_value__CAIS_8_9s)

#Handling -ve `__of_status_3_s_L6m`  ; Assumed to be non negative

Insurance_DataSet$`__of_status_3_s_L6m`<- ifelse(Insurance_DataSet$`__of_status_3_s_L6m`<0,0,Insurance_DataSet$`__of_status_3_s_L6m`)


#Handling -ve Years_on_ER_for_SP  ; Assumed to be non negative

Insurance_DataSet$Years_on_ER_for_SP<- ifelse(Insurance_DataSet$Years_on_ER_for_SP<0,7,Insurance_DataSet$Years_on_ER_for_SP)


#Handling -ve Total___outstanding_CCJ_s  ; Assumed to be non negative

Insurance_DataSet$Total___outstanding_CCJ_s<- ifelse(Insurance_DataSet$Total___outstanding_CCJ_s<0,0,Insurance_DataSet$Total___outstanding_CCJ_s)

#Handling -ve Total_outstanding_balance___excl  ; Assumed to be non negative

Insurance_DataSet$Total_outstanding_balance___excl<- ifelse(Insurance_DataSet$Total_outstanding_balance___excl<0,138,Insurance_DataSet$Total_outstanding_balance___excl)


#Handling -ve Total___of_accounts ; Assumed to be non negative

Insurance_DataSet$Total___of_accounts<- ifelse(Insurance_DataSet$Total___of_accounts<0,6,Insurance_DataSet$Total___of_accounts)


#Handling -ve Time_since_most_recent_outstandi ; Assumed to be non negative

Insurance_DataSet$Time_since_most_recent_outstandi<- ifelse(Insurance_DataSet$Time_since_most_recent_outstandi<0,92,Insurance_DataSet$Time_since_most_recent_outstandi)

# Understanding Term (Good as per business rules)
boxplot(Insurance_DataSet$Term)
hist(Insurance_DataSet$Term)
skewness(Insurance_DataSet$Term)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Term)) #5,10,3,4,2,3 are most prominent terms


# Understanding Net Advance (Distribution skewed)
boxplot(Insurance_DataSet$Net_Advance)
hist(Insurance_DataSet$Net_Advance)
skewness(Insurance_DataSet$Net_Advance)

# Understanding APR
boxplot(Insurance_DataSet$APR)
hist(Insurance_DataSet$APR)
skewness(Insurance_DataSet$APR)

# Understanding Time At Address (Distribution Skewed)
boxplot(Insurance_DataSet$Time_at_Address)
hist(Insurance_DataSet$Time_at_Address)
skewness(Insurance_DataSet$Time_at_Address)

# Understanding Time in Employement (Distribution Skewed)
boxplot(Insurance_DataSet$Time_in_Employment)
hist(Insurance_DataSet$Time_in_Employment)
skewness(Insurance_DataSet$Time_in_Employment)


# Understanding Time with Bank (Distribution Skewed)
boxplot(Insurance_DataSet$Time_with_Bank)
hist(Insurance_DataSet$Time_with_Bank)
skewness(Insurance_DataSet$Time_with_Bank)

# Understanding Time with Bank (Distribution Very Skewed)
boxplot(Insurance_DataSet$Value_of_Property)
hist(Insurance_DataSet$Value_of_Property)
skewness(Insurance_DataSet$Value_of_Property)

# Understanding Outstanding_Mortgage_Bal (Distribution Very Skewed)
boxplot(Insurance_DataSet$Outstanding_Mortgage_Bal)
hist(Insurance_DataSet$Outstanding_Mortgage_Bal)
skewness(Insurance_DataSet$Outstanding_Mortgage_Bal)

# Understanding Total Outstanding Balances (Distribution Skewed)
boxplot(Insurance_DataSet$Total_Outstanding_Balances)
hist(Insurance_DataSet$Total_Outstanding_Balances)
skewness(Insurance_DataSet$Total_Outstanding_Balances)

# Understanding Bureau_Data___Monthly_Other_Co_R (Distribution Very Skewed)
boxplot(Insurance_DataSet$Bureau_Data___Monthly_Other_Co_R)
hist(Insurance_DataSet$Bureau_Data___Monthly_Other_Co_R)
skewness(Insurance_DataSet$Bureau_Data___Monthly_Other_Co_R)


#Converting to Factor

Insurance_DataSet$Final_Grade<- as.factor(Insurance_DataSet$Final_Grade)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Final_Grade)) #A,X and B are most prominent terms


Insurance_DataSet$Loan_Type<- as.factor(Insurance_DataSet$Loan_Type)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Loan_Type)) #Unsecured are most prominent terms

Insurance_DataSet$Mosaic_Class<- as.factor(Insurance_DataSet$Mosaic_Class)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Mosaic_Class)) # 4,8,7,2 are most prominent terms

Insurance_DataSet$Mosaic<- as.numeric(Insurance_DataSet$Mosaic)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Mosaic)) # 4,8,7,2 are most prominent terms


Insurance_DataSet$Residential_Status<- as.factor(Insurance_DataSet$Residential_Status)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Residential_Status)) #H,T are most prominent terms

Insurance_DataSet$Telephone_Indicator<- as.factor(Insurance_DataSet$Telephone_Indicator)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Telephone_Indicator)) # Y are most prominent terms


Insurance_DataSet$Number_of_Dependants<- as.factor(Insurance_DataSet$Number_of_Dependants)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Number_of_Dependants)) #Lesser the no of dependents is most prominent


Insurance_DataSet$Marital_Status<- as.factor(Insurance_DataSet$Marital_Status)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Marital_Status)) #Married,Single and Divorced are prominent in decreasing order


Insurance_DataSet$Gender<- as.factor(Insurance_DataSet$Gender)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Gender)) # Male followed by female but same ratio observed with non PPI group

Insurance_DataSet$Employment_Status<- as.factor(Insurance_DataSet$Employment_Status)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Employment_Status)) # Private, Gov and Salaried are most prominent terms


Insurance_DataSet$Full_Part_Time_Empl_Ind<- as.factor(Insurance_DataSet$Full_Part_Time_Empl_Ind)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Full_Part_Time_Empl_Ind)) # Full time are most prominent terms

Insurance_DataSet$Perm_Temp_Empl_Ind<- as.factor(Insurance_DataSet$Perm_Temp_Empl_Ind)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Perm_Temp_Empl_Ind)) # Permanent employees are most prominent terms

Insurance_DataSet$Income_Range<- as.factor(Insurance_DataSet$Income_Range)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Income_Range)) # 2 and above are most prominent terms


Insurance_DataSet$Current_Account<- ifelse(Insurance_DataSet$Current_Account=="FALS","FALSE",Insurance_DataSet$Current_Account)
Insurance_DataSet$Current_Account<- as.factor(Insurance_DataSet$Current_Account)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Current_Account)) # Current account holders are most prominent terms

Insurance_DataSet$ACCESS_Card<- as.factor(Insurance_DataSet$ACCESS_Card)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(ACCESS_Card)) # Non Access cards but also some people with access card are most prominent terms

Insurance_DataSet$VISA_Card<- as.factor(Insurance_DataSet$VISA_Card)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(VISA_Card)) #VISA Card holders and some non-Visa card holders are most prominent terms


Insurance_DataSet$American_Express<- as.factor(Insurance_DataSet$American_Express)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(American_Express)) #Non AMEX are most prominent terms


Insurance_DataSet$Diners_Card<- as.factor(Insurance_DataSet$Diners_Card)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Diners_Card)) #Non Diners are most prominent terms


Insurance_DataSet$Cheque_Guarantee<- ifelse(Insurance_DataSet$Cheque_Guarantee=="FALS","FALSE",Insurance_DataSet$Cheque_Guarantee)
Insurance_DataSet$Cheque_Guarantee<- as.factor(Insurance_DataSet$Cheque_Guarantee)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Cheque_Guarantee)) #Customers having Cheque Gurantee are most prominent terms


Insurance_DataSet$Other_Credit_Store_Card<- as.factor(Insurance_DataSet$Other_Credit_Store_Card)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Other_Credit_Store_Card)) #Non Other credit card holders + some credit card holders are most prominent terms


Insurance_DataSet$Payment_Method<- as.factor(Insurance_DataSet$Payment_Method)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Payment_Method)) #Payment method = D are most prominent terms


Insurance_DataSet$Worst_History_CT<-as.factor(Insurance_DataSet$Worst_History_CT) 
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Worst_History_CT)) #0,5 followed by 4 distantly are most prominent terms


Insurance_DataSet$Worst_status_L6m<- as.factor(Insurance_DataSet$Worst_status_L6m)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Worst_status_L6m)) #0,1 followed by 2 distantly are most prominent terms


Insurance_DataSet$Worst_CUrrent_Status<- as.factor(Insurance_DataSet$Worst_CUrrent_Status)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Worst_CUrrent_Status)) #0 followed by 1 distantly are most prominent terms


Insurance_DataSet$Bankruptcy_Detected__SP_<- as.factor(Insurance_DataSet$Bankruptcy_Detected__SP_)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(Bankruptcy_Detected__SP_)) #Non Bankrupt are most prominent terms


Insurance_DataSet$CIFAS_detected<- as.factor(Insurance_DataSet$CIFAS_detected)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(CIFAS_detected)) #Non CIFAS  are most prominent terms


Insurance_DataSet$category <- tolower(Insurance_DataSet$category)
Insurance_DataSet$category<- as.factor(Insurance_DataSet$category)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(category)) #Majority are single followed by LCI and Joint are most prominent terms


Insurance_DataSet$PPI_SINGLE<- as.factor(Insurance_DataSet$PPI_SINGLE)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(PPI_SINGLE)) #6264


Insurance_DataSet$PPI_JOINT<- as.factor(Insurance_DataSet$PPI_JOINT)
table(Insurance_DataSet %>% filter(.,PPI==1) %>% dplyr::select(PPI_JOINT)) #1225

Insurance_DataSet$PPI_LCI<- as.factor(Insurance_DataSet$PPI_LCI) #1972



#Finding Correlation
cor(Insurance_DataSet$PPI,Insurance_DataSet$Credit_Score)
df_insurance<-Insurance_DataSet[,c(2,4:6,10,16,28:32,35,36,38,39,40,48,51,53)]
cormat <- round(cor(df_insurance),2)

#x= ggpairs(df_insurance)

#Variable Importance


#Finding Relationships
# Change the description to lowercase
Insurance_DataSet$Insurance_Description <- tolower(Insurance_DataSet$Insurance_Description)

cols_remove <-c("Insurance_Description","code","prdt_desc","category","PPI_SINGLE","PPI_JOINT","PPI_LCI")

cols_remove_single <-c("Insurance_Description","code","prdt_desc","category","PPI","PPI_JOINT","PPI_LCI")

cols_remove_joint <-c("Insurance_Description","code","prdt_desc","category","PPI","PPI_SINGLE","PPI_LCI")

cols_remove_LCI <-c("Insurance_Description","code","prdt_desc","category","PPI","PPI_SINGLE","PPI_JOINT")


df_insurance_cleaned<- Insurance_DataSet %>% dplyr::select(-cols_remove)
df_insurance_cleaned_single<- Insurance_DataSet %>% dplyr::select(-cols_remove_single)
df_insurance_cleaned_joint<- Insurance_DataSet %>% dplyr::select(-cols_remove_joint)
df_insurance_cleaned_lci<- Insurance_DataSet %>% dplyr::select(-cols_remove_LCI)



# First let's create a copy of cleaned dataset
insurance_cleaned_test <- df_insurance_cleaned

# Change the levels to "good " and "bad"
insurance_cleaned_test$PPI <- ifelse(insurance_cleaned_test$PPI==1,"bad","good")

# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
insurance_cleaned_test$PPI <- ifelse(insurance_cleaned_test$PPI=="good",1,0)



#IV & WOE by new method

iv = iv(insurance_cleaned_test[,-1], y = 'PPI') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv %>%
  knitr::kable()

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(iv)){
  
  if (iv$info_value[i]<0.02){
    iv$feedback[i] = "Useless"
    
  } else if(iv$info_value[i]>=0.02& iv$info_value[i]<0.1){
    iv$feedback[i] = "Weak"
    
  } else if(iv$info_value[i]>=0.1 & iv$info_value[i]<0.3){
    iv$feedback[i] = "Medium"
    
  }else if(iv$info_value[i]>=0.3 & iv$info_value[i]<0.5){
    iv$feedback[i] = "Strong"
    
  }else if(iv$info_value[i]>=0.5){
    iv$feedback[i] = "Suspicious"
  }
}


#Important Variables

ggplot(data=iv %>% dplyr::filter(.,feedback=="Strong" | feedback=="Medium"), aes(x=reorder(variable,info_value),y=info_value,fill=feedback)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Important variables based on IV analysis") +xlab("Variables")

#Transforming data set with WOE values 

bins = woebin(insurance_cleaned_test[,-1], y = 'PPI')

bins$Bureau_Data___Monthly_Other_Co_R%>%
  knitr::kable()

woebin_plot(bins$Outstanding_Mortgage_Bal,show_iv = FALSE)



woebin_plot(bins$Credit_Score)


woebin_plot(bins$Bureau_Data___Monthly_Other_Co_R)

woebin_plot(bins$Time_with_Bank)

#Extracting Important Variables

# Based on feedback 
## Extract "Strong",Medium" and "Weak" variables  
imp_vars <- which(iv$feedback=="Strong"|iv$feedback=="Medium" )
k1 <- as.data.frame(iv[imp_vars, 1])
imp <- which(colnames(df_insurance_cleaned) %in% k1$variable)
df_insurance_important <- df_insurance_cleaned[, c(1,imp,52)]

insurance_woe = woebin_ply( df_insurance_important, bins ) %>%
  as_tibble()

#Model building


# Let's remove Ref ID from the data
insurance_woe_1 <- insurance_woe[, -1]

set.seed(11111)
split_indices <- sample.split(df_insurance_important$PPI, SplitRatio = 0.70)

# Train Dataset 
train <- df_insurance_important[split_indices, ]
sum(train$PPI)/nrow(train)

# Test Dataset
test <-df_insurance_important[!split_indices, ]
sum(test$PPI)/nrow(test)


# converting train and test into woe values
train_woe = woebin_ply(train, bins)
test_woe = woebin_ply(test, bins)


#  PPI= 1 implies Cross sell, 0 implies No Cross Sell

# Logistic model
initial_model = glm(PPI ~ ., data = train_woe[,-1], family = "binomial")

# Summary initial model
summary(initial_model)

# Run stepwise feature selection to remove the insignificant independent variables 

best_model_1 = step(initial_model, direction = "both")

summary(best_model_1)

# Note that the coefficients are negative because we have converted to woe values
# such that a higher woe value indicates 'good' customer, thus, p(bad) should go down 
# with increasing woe (or increasing good customers)

# Checking the variance inflation factor to detect the highly correlated independent variable.

vif(best_model_1)

best_model_2 <- glm(formula = PPI ~ Credit_Score_woe + Term_woe + Net_Advance_woe + 
                      APR_woe + Mosaic_woe  + Time_at_Address_woe + 
                      Income_Range_woe + Time_with_Bank_woe + 
                      Value_of_Property_woe + Bureau_Data___Monthly_Other_Co_R_woe + 
                      Total_value__CAIS_8_9s_woe , family = "binomial", data = train_woe[,-1])

# Summary "best_model_2"
summary(best_model_2)

vif(best_model_2)

#Evaluating the model on test data

# predicted proability
train_pred = predict.glm(best_model_2, train_woe, type='response')
test_pred = predict.glm(best_model_2, test_woe, type='response')
# performance
train_perf = perf_eva(train_pred,train$PPI, title = "train",confusion_matrix = TRUE,threshold = 0.57,show_plot = c("ks", "lift", "gain", "roc",
                                                                                                                    "density"))
test_perf = perf_eva(test_pred,test$PPI, title = "test",confusion_matrix = TRUE,threshold = 0.59,show_plot = c("ks", "lift", "gain", "roc",
                                                                                                               "density"))


# score ------
card = scorecard(bins, best_model_2)
# credit score
train_score = scorecard_ply(train, card, print_step=0,only_total_score = FALSE)
test_score = scorecard_ply(test, card, print_step=0,only_total_score = FALSE)

train$Score<-scorecard_ply(train, card, print_step=0,only_total_score = TRUE)
train$Scorecard <-train$Score$score
train<-train[,c(1:18,20)]

test$score<-scorecard_ply(test, card, print_step=0,only_total_score = TRUE)
test$Scorecard <-test$score$score
test<-test[,c(1:18,20)]

write.csv(train,file="C:/Users/Asus/Downloads/train.csv")
write.csv(test,file="C:/Users/Asus/Downloads/test.csv")

# psi
ps<-perf_psi(
  score = list(train = train_score, test = test_score),
  label = list(train = train$PPI, test = test$PPI),title = "Scorecard Distribution"
)
################################################################################


test$PPI <- as.factor(test$PPI) 

predictions_logit <- predict(best_model_2, newdata = test_woe, type = "response")

summary(predictions_logit)

# Let's find out the optimal probablility cutoff 

test_woe$PPI<-as.factor(test_woe$PPI)


perform_fn <- function(cutoff) {
  predicted_response <- as.factor(ifelse(predictions_logit>=cutoff, "1", "0"))
  
  conf <- confusionMatrix(predicted_response,test_woe$PPI, positive = "1")
  
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc)))
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#######################################################################################

#-------------------------------------------------------------------------------------

# Creating cutoff values from 0.014 to 0.11 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(0.01,1 ,length=100)

OUT = matrix(0,100,3)


for(i in 1:100){
  OUT[i,] = perform_fn(s[i])
}

# #######################################################################################
# 
# # plotting cutoffs 
# 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,0.2,length=5),seq(0,0.2,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.08,.70, col=c(1,"darkgreen", 2,"darkred"),lwd=c(1,1,1,1),c("Sensitivity","Specificity","Accuracy"))


cutoff_logistic<- s[which(abs(OUT[,1]-OUT[,2])<0.025)]

cutoff_logistic <- cutoff_logistic[1]
# the optimal cutoff seems to be somewhere around 0.58

#---------------------------------------------------------    
# ### Confusion Matrix
predicted_response <- as.factor(ifelse(predictions_logit >= cutoff_logistic, 1, 0))


conf_logistic <- confusionMatrix(predicted_response,test_woe$PPI, positive = "1")

conf_logistic #Logistic Model Confusion matrix

### Data preparation for modelling

RF_data <- insurance_woe


# Spliting the bank data in 70:30 ratio

RF_data$PPI <- as.factor(ifelse(RF_data$PPI==1,"yes","no"))

set.seed(1010)
split_indices <- sample.split(RF_data$PPI, SplitRatio = 0.70)

train_rf <- RF_data[split_indices, ]

test_rf <- RF_data[!split_indices, ]

#### Modelling ############################### 

library(randomForest)

rf_model <- randomForest( PPI~., data = train_rf, proximity = F, do.trace = T, mtry = 5,ntree=500)

rf_pred <- predict(rf_model, test_rf[, -1], type = "prob")

summary(rf_pred)
perform_fn_rf <- function(cutoff) {
  predicted_response <- as.factor(ifelse(rf_pred[, 2] >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test_rf$PPI, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  OUT_rf <- t(as.matrix(c(sens, spec, acc))) 
  colnames(OUT_rf) <- c("sensitivity", "specificity", "accuracy")
  return(OUT_rf)
}


s = seq(0.01,0.6 ,length=100)

OUT_rf = matrix(0,100,3)


for(i in 1:100){
  OUT_rf[i,] = perform_fn_rf(s[i])
}

#######################################################################################
# plotting cutoffs

plot(s, OUT_rf[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT_rf[,2],col="darkgreen",lwd=2)
lines(s,OUT_rf[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Let's find the cutoff 
cutoff_rf <- s[which(abs(OUT_rf[,1]-OUT_rf[,2])<0.045)]
cutoff_rf

predicted_response_rf <- as.factor(ifelse(rf_pred[, 2] >= 0.57, "yes", "no"))
conf_rf <- confusionMatrix(predicted_response_rf, test_rf$PPI, positive = "yes")

conf_rf 
#Random Forest was not much better than the Logistic Regression Model

#Generating Score Card

pred_logit = predict.glm(best_model_2,newdata = insurance_woe)
resp_logit = predict.glm(best_model_2,newdata = insurance_woe ,type = 'response')

res = tibble( logit = pred_logit
              , odds = exp(pred_logit)
              , prob = odds / (odds + 1)
              , prob_ctrl = resp_logit )

points0 = 600
odds0 = 20
pdo = 50

card = scorecard( bins = bins , best_model_2
                  , points0 = points0 
                  , odds0 = 1/odds0 # scorecard wants the inverse
                  , pdo = pdo 
)

sc = scorecard_ply(df_insurance_important,card)

df_insurance_scorecard<-cbind(df_insurance_important,sc)

res$score = sc[[1]]

card[[2]]

factor = pdo / log(2)
offset = points0 - factor * log( odds0 )

res$score_ctrl = offset - factor * res$logit

summary(res)


imp = tibble( variable = names( coef(best_model_2) )
              , coef = coef(best_model_2) ) %>%
  mutate( variable = map_chr( variable, function(x) unlist( str_split(x, '_woe') )[[1]]  ) ) %>%
  left_join( iv ) %>%
  mutate( imp = abs(coef) * info_value ) %>%
  arrange( desc(imp) ) 

knitr::kable( imp, align = 'lccc', digits = 2 )

#Interpreting Individual Predictions
data_relevant = insurance_woe[, names( coef(best_model_2) )[-1] ]

data_mult_logit = as_tibble( data_relevant * coef(best_model_2)[-1] ) 

#Data frame with Individual Scores

data_mult_score = data_mult_logit %>%
  mutate_all( function(x) - factor * x ) %>%
  mutate( intercept = coef(best_model_2)[1]
          , intercept = offset - factor * intercept )

score = apply( data_mult_score, 1, sum ) 

data_mult_score$score = score

data_mult_score$score_ctrl = res$score

data_mult_score %>%
  select(.,score,intercept, everything() ) %>%
  head(10) %>%
  knitr::kable()

#Second Approach


pred = predict(m)
resp = predict(m, type = 'response')

res = tibble( logit = pred
              , odds = exp(pred)
              , prob = odds / (odds + 1)
              , prob_ctrl = resp )

points0 = 600
odds0 = 20
pdo = 50

card = scorecard( bins , m
                  , points0 = points0 
                  , odds0 = 1/odds0 # scorecard wants the inverse
                  , pdo = pdo 
)

sc = scorecard_ply( df_insurance_important, card )

res$score = sc[[1]]

card[[2]]

factor = pdo / log(2)
offset = points0 - factor * log( odds0 )

res$score_ctrl = offset - factor * res$logit

summary(res)


imp = tibble( variable = names( coef(m) )
              , coef = coef(m) ) %>%
  mutate( variable = map_chr( variable, function(x) unlist( str_split(x, '_woe') )[[1]]  ) ) %>%
  left_join( iv ) %>%
  mutate( imp = abs(coef) * info_value ) %>%
  arrange( desc(imp) ) 

knitr::kable( imp, align = 'lccc', digits = 2 )

insurance_woe$Insurance_Description <- Insurance_DataSet$Insurance_Description[match(insurance_woe$Ref,Insurance_DataSet$Ref)]

#Measuring Variable importance by Products Categories - Single, Joint and LCI

#PPI SINGLE

# First let's create a copy of cleaned dataset
insurance_cleaned_test_single <- df_insurance_cleaned_single

# Change the levels to "good " and "bad"
insurance_cleaned_test_single$PPI_SINGLE <- ifelse(insurance_cleaned_test_single$PPI_SINGLE==1,"bad","good")

# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
insurance_cleaned_test_single$PPI_SINGLE <- ifelse(insurance_cleaned_test_single$PPI_SINGLE=="good",1,0)



#IV & WOE by new method

iv_single = iv(insurance_cleaned_test_single[,-1], y = 'PPI_SINGLE') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv_single %>%
  knitr::kable()

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(iv_single)){
  
  if (iv_single$info_value[i]<0.02){
    iv_single$feedback[i] = "Useless"
    
  } else if(iv_single$info_value[i]>=0.02& iv_single$info_value[i]<0.1){
    iv_single$feedback[i] = "Weak"
    
  } else if(iv_single$info_value[i]>=0.1 & iv_single$info_value[i]<0.3){
    iv_single$feedback[i] = "Medium"
    
  }else if(iv_single$info_value[i]>=0.3 & iv_single$info_value[i]<0.5){
    iv_single$feedback[i] = "Strong"
    
  }else if(iv_single$info_value[i] > 0.5){
    iv_single$feedback[i] = "Suspicious"
  }
}


#Important Variables

ggplot(data=iv_single %>% dplyr::filter(.,feedback=="Strong" | feedback=="Medium"), aes(x=reorder(variable,info_value),y=info_value,fill=feedback)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Woe- Important variables based on Information value analysis") +xlab("Variables")

bins_single = woebin(insurance_cleaned_test_single[,-1], y = 'PPI_SINGLE')

bins_single$Income_Range%>%
  knitr::kable()

woebin_plot(bins_single$Income_Range)




#PPI JOINT

# First let's create a copy of cleaned dataset
insurance_cleaned_test_joint <- df_insurance_cleaned_joint

# Change the levels to "good " and "bad"
insurance_cleaned_test_joint$PPI_JOINT <- ifelse(insurance_cleaned_test_joint$PPI_JOINT==1,"bad","good")

# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
insurance_cleaned_test_joint$PPI_JOINT <- ifelse(insurance_cleaned_test_joint$PPI_JOINT=="good",1,0)



#IV & WOE by new method

iv_joint = iv(insurance_cleaned_test_joint[,-1], y = 'PPI_JOINT') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv_joint %>%
  knitr::kable()

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(iv_joint)){
  
  if (iv_joint$info_value[i]<0.02){
    iv_joint$feedback[i] = "Useless"
    
  } else if(iv_joint$info_value[i]>=0.02 & iv_joint$info_value[i]<0.1){
    iv_joint$feedback[i] = "Weak"
    
  } else if(iv_joint$info_value[i]>=0.1 & iv_joint$info_value[i]<0.3){
    iv_joint$feedback[i] = "Medium"
    
  }else if(iv_joint$info_value[i]>=0.3 & iv_joint$info_value[i]<0.5){
    iv_joint$feedback[i] = "Strong"
    
  }else if(iv_joint$info_value[i]>0.5){
    iv_joint$feedback[i] = "Suspicious"
  }
}


#Important Variables

ggplot(data=iv_joint %>% dplyr::filter(.,feedback=="Strong" | feedback=="Medium"), aes(x=reorder(variable,info_value),y=info_value,fill=feedback)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Woe- Important variables based on Information value analysis") +xlab("Variables")

bins_joint = woebin(insurance_cleaned_test_joint[,-1], y = 'PPI_JOINT')

bins_joint$Income_Range%>%
  knitr::kable()

woebin_plot(bins_joint$Total_outstanding_balance__mortg)


#PPI LCI

# First let's create a copy of cleaned dataset
insurance_cleaned_test_lci <- df_insurance_cleaned_lci

# Change the levels to "good " and "bad"
insurance_cleaned_test_lci$PPI_LCI <- ifelse(insurance_cleaned_test_lci$PPI_LCI==1,"bad","good")

# Rechange the levels to "1" for good customer and "0" for bad customers (This change is only for this package)
insurance_cleaned_test_lci$PPI_LCI <- ifelse(insurance_cleaned_test_lci$PPI_LCI=="good",1,0)



#IV & WOE by new method

iv_lci = iv(insurance_cleaned_test_lci[,-1], y = 'PPI_LCI') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv_lci %>%
  knitr::kable()

# Following the thumb rule, a variable is:
# Useless if IV is < 0.02
# Weak if IV is [0.02, 0.1)
# Medium if IV is [0.1, 0.3)
# Strong if IV is[0.3, 0.5) and suspicious thereafter
for(i in 1:nrow(iv_lci)){
  
  if (iv_lci$info_value[i]<0.02){
    iv_lci$feedback[i] = "Useless"
    
  } else if(iv_lci$info_value[i]>=0.02 & iv_lci$info_value[i]<0.1){
    iv_lci$feedback[i] = "Weak"
    
  } else if(iv_lci$info_value[i]>=0.1 & iv_lci$info_value[i]<0.3){
    iv_lci$feedback[i] = "Medium"
    
  }else if(iv_lci$info_value[i]>=0.3 & iv_lci$info_value[i]<0.5){
    iv_lci$feedback[i] = "Strong"
    
  }else if(iv_lci$info_value[i]>0.5){
    iv_lci$feedback[i] = "Suspicious"
  }
}


#Important Variables

ggplot(data=iv_lci %>% dplyr::filter(.,feedback=="Strong" | feedback=="Medium"), aes(x=reorder(variable,info_value),y=info_value,fill=feedback)) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() +
  ggtitle("Woe- Important variables based on Information value analysis") +xlab("Variables")

bins_lci = woebin(insurance_cleaned_test_lci[,-1], y = 'PPI_LCI')

bins_lci$Employment_Status %>%
  knitr::kable()

woebin_plot(bins_lci$Employment_Status)
woebin_plot(bins_lci$Age)


#Building Association Rules using Apriori Algorithm
# Change the description to lowercase
Insurance_DataSet$Insurance_Description <- tolower(Insurance_DataSet$Insurance_Description)


# Specify columns to work with
required_cols <- c("Mosaic_Class","Insurance_Description")

# Filter data and select only required columns. Filtering on PPI=1 as only
# they have product info.
required_df <- Insurance_DataSet %>% dplyr::filter(PPI == 1) %>% dplyr::select(required_cols)

# Wrange data to be usable as transaction data
products <- aggregate(Insurance_Description ~ Mosaic_Class, required_df, 
                      c)
transactions <-0
transactions <- as(products$Insurance_Description, "transactions")

arules::itemFrequencyPlot(transactions,topN=20,main='Relative Item Frequency Plot',type="relative",ylab="Item Frequency (Relative)")

# Run apriori algorithm to get rules
rules <- apriori(transactions, parameter = list(supp = 0.001, conf = 0.50, minlen = 2))

# Sort the rules according to decreasing confidence
rules <- sort(rules, by = "confidence", decreasing = TRUE)

# Remove the redundant rules
rules <- rules[!is.redundant(rules)]
rules_df <- as(rules, "data.frame")



write.csv(rules_df,"C:/Users/Asus/Downloads/Rules_Insurance_Cross Sell.csv")


































