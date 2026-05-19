library(caret)
library(randomForest)
library(nlme)


set.seed(146363)
ctrl <- trainControl(
  method = "repeatedcv",    
  number = 5,              
  repeats = 3,              
  verboseIter = TRUE
)

suivi <- suivi %>% mutate(log_PR = log(PR))
suivi <- suivi %>%
  mutate(
    log_PR = log(PR),
    H2 = H^2,
    L02 = L0^2,
    D02 = D0^2,
    B02 = B0^2,
    L12 = L1^2,
    N02 = N0^2,
    D12 = D1^2,
    N12 = N1^2,
    B12 = B1^2
  )


rf_caret <- train(
  log_PR ~  H + L0 + D0 + B0 + L1 + N0 + D1 + N1 + B1 + growth_period,
  data = suivi,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,         
  importance = TRUE,
  ntree = 500
)
rf_quadr_caret <- train(
  log_PR ~  H + H2 + L0 + L02 + D0 + D02 + B0 + B02 + 
    L1 + L12 + N0 + N02 + D1 + D12 + N1 + N12 + B1 + B12 + growth_period,
  data = suivi,
  method = "rf",
  trControl = ctrl,
  tuneLength = 5,         
  importance = TRUE,
  ntree = 500
)

print(rf_caret)
plot(rf_caret)  

rf_best <- rf_caret$finalModel
rf_best

importance_rf <- importance(rf_best)
importance_rf
importance_df <- data.frame(
  Variable = rownames(importance_rf),
  IncNodePurity = importance_rf[, "%IncMSE"]  
) %>% arrange(desc(IncNodePurity))
importance_df


varImpPlot(rf_best, main = "Variablenwichtigkeit")


ggplot(importance_df[1:10,], aes(x = reorder(Variable, IncNodePurity), 
                                 y = IncNodePurity)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Variable", y = "% Increase MSE", 
       title = "Random Forest Variable Importance")


#Model comparison with linear models

mod_log_cv <- train(
  log(PR) ~ L1 + N0 + D0 + D1 + N1 + B0,
  data = suivi,
  method = "lm",
  trControl = ctrl
)


mod_quadrlog_cv <- train(
  log(PR) ~ I(L1^2) + I(N0^2) + N0 + I(D0^2) + D0 + 
    I(N1^2) + N1 + B0 + B1 + I(B1^2),
  data = suivi,
  method = "lm",
  trControl = ctrl
)


mod_log_cv$results$RMSE
mod_quadrlog_cv$results$RMSE
rf_caret$results$RMSE
rf_quadr_caret$results$RMSE
