PATH <- './'
selfies = read.table(paste(PATH, 'users_frame.csv', sep=''), sep= ';',header = TRUE)

run_regr <- function() {
  selfies_indices = sample(n, n*0.8, replace = FALSE)
  selfies_train = selfies[selfies_indices, ]
  selfies_test =  selfies[-selfies_indices, ]
  
  final_model_train = lm(PERMA~college+post_graduate + user_posted_photos+ income+ Dimples + Hat +Plastic_surgery +Blonde, data=selfies_train) #final model on trainset
  final_model_test =  lm(PERMA~college+post_graduate + user_posted_photos+ income+ Dimples + Hat +Plastic_surgery +Blonde, data=selfies_test) #final model on testset
  
  SSE_train = sum(residuals(final_model_train)^2)
  SSYY_train = sum((selfies_train$PERMA - mean(selfies_train$PERMA))^2)
  R_train = (SSYY_train-SSE_train)/SSYY_train
  
  prediction = predict(final_model_train, selfies_test)
  SSE_test = sum((prediction-selfies_test$PERMA)^2)
  SSYY_test = sum((selfies_test$PERMA - mean(selfies_test$PERMA))^2)
  R_test = (SSYY_test-SSE_test)/SSYY_test
  
  return(c(R_train, R_test))
}

n = nrow(selfies)
Rs_train = c()
Rs_test = c() 

set.seed(5)

for (i in 1:50) {
  res <- try(run_regr(), silent=TRUE) 
  
  Rs_train[i] <- res[1]
  Rs_test[i] <- res[2]
}

print(Rs_train)
print(Rs_test)
Rs <- c(Rs_train, Rs_test)

write.csv(Rs_train, 'r_squares_train.csv')
write.csv(Rs_test, 'r_squares_test.csv')