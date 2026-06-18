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
varImp(rf_caret)
varImpPlot(rf_caret)

rf_best <- rf_caret$finalModel
rf_best

varImpPlot(rf_best, main = "Variable Importance")

importance_rf <- importance(rf_best)
print(importance_rf)


importance_df <- data.frame(
  Variable = rownames(importance_rf),
  IncMSE = importance_rf[, "%IncMSE"] 
) %>% arrange(desc(IncMSE))


fig_RFVarImp <- ggplot(importance_df[1:10,], aes(x = reorder(Variable, IncMSE), y = IncMSE)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    x = "Variable", 
    y = "% Increase MSE", 
    title = "Random Forest Variable Importance"
  ) +
  theme_minimal()
fig_RFVarImp

ggsave("data/figures/RFVarImp.png", 
       plot = fig_RFVarImp,
       width = 4.5, 
       height = 4.5,  
       dpi = 300)

most_important_var <- importance_df$Variable[1]
print(paste("Most important variable:", most_important_var))

library(pdp) 

pdp_plot <- partial(
  rf_caret, 
  pred.var = "D0",    
  train = suivi,                  
  type = "regression",               
  grid.resolution = 100              
)
pdp_plot


pdp_orig <- pdp_plot
pdp_orig$yhat_orig <- exp(pdp_orig$yhat)  # <-- Exponential!

# 3. Plot auf Original-Skala
fig_pdpD0 <- ggplot(pdp_orig, aes(x = D0, y = yhat_orig)) +
  geom_line(size = 1.5, color = "steelblue") +
  geom_rug(data = suivi, aes(x = D0), 
           alpha = 0.1, sides = "b", 
           inherit.aes = FALSE) +
  labs(
    x = "Diameter of principal branch (cm)",
    y = "Predicted root biomass (kg)",  # <-- Original-Skala!
    title = "Partial Dependence Plot for D0"
  ) +
  theme_minimal()
fig_pdpD0

ggsave("data/figures/RFpdpD0.png", 
       plot = fig_pdpD0,
       width = 4.5, 
       height = 4.5,  
       dpi = 300)




#Model comparison with linear models

mod_log_cv <- train(
  log(PR) ~ H + L0 + D0 + B0 + growth_period,
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
