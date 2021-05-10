
### Training Models ################

preset_params = expand.grid(nrounds = df_6_result[[2]]$bestTune[1] %>% as.numeric,
                            max_depth = df_6_result[[2]]$bestTune[2] %>% as.numeric,
                            eta = df_6_result[[2]]$bestTune[3] %>% as.numeric,
                            gamma = df_6_result[[2]]$bestTune[4] %>% as.numeric,
                            colsample_bytree = df_6_result[[2]]$bestTune[5] %>% as.numeric,
                            min_child_weight = df_6_result[[2]]$bestTune[6] %>% as.numeric,
                            subsample = df_6_result[[2]]$bestTune[7] %>% as.numeric)

# F-L+
lab_option1_model = train(target ~., 
                          data = df_6 %>% select(option1_vars, target),
                          method = 'xgbTree',
                          metric = 'ROC',
                          na.action = na.pass,
                          trControl = trainControl(method = 'none', summaryFunction = twoClassSummary, classProbs = T, savePredictions = T),
                          tuneGrid = preset_params)

# F-L-
nolab_option1_model = train(target ~., 
                            data = df_6 %>% select(option1_vars_nolab, target),
                            method = 'xgbTree',
                            metric = 'ROC',
                            na.action = na.pass,
                            trControl = trainControl(method = 'none', summaryFunction = twoClassSummary, classProbs = T, savePredictions = T),
                            tuneGrid = preset_params)

# F+L-
nolab_full_model = train(target ~., 
                         data = df_6 %>% select(-c(HGB, HCT, LYMPHO, PLT, WBC)),
                         method = 'xgbTree',
                         metric = 'ROC',
                         na.action = na.pass,
                         trControl = trainControl(method = 'none', summaryFunction = twoClassSummary, classProbs = T, savePredictions = T),
                         tuneGrid = preset_params)


### Testing on Holdout Set ################

holdout_full = holdout %>% 
  mutate(target = ifelse(CSS >=6, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No'))) %>%
  select(-c(CURSIT, OUTCOME, id, PERIOD, CSS))


###### Holdout Performance ########################

holdout_comparison = data.frame()
option_list = list(df_6_result[[2]], lab_option1_model,nolab_full_model, nolab_option1_model)
model_names = c('Lab_Full', 'Lab_Simple','No_Lab_Full', 'No_Lab_Simple')
cutoff = c(0.0255, 0.0243, 0.055, 0.0575) # Pre-found thresholds for 90% specificity 
cv_list = list(single_cv_conf_mat(df_6_result[[2]], output = 'auc'),
               rfe_performance(xgbProfile, option1),
               rfe_performance(xgbProfile_nolab, 32),
               rfe_performance(xgbProfile_nolab, option1_nolab))


for( i in 1:length(option_list)){
  
  model = option_list[[i]]
  holdout_pred = predict(object = model,
                         newdata = holdout_full %>% select(model$coefnames), na.action = na.pass, type = 'prob')[,1]
  
  prob = holdout_pred
  obs = holdout_full %>% pull(target) 
  roc = roc(obs,prob,quiet = T, levels = c('No', 'Yes'))
  
  set.seed(2021)
  CIs = ci.coords(roc,
                  x = cutoff[i],
                  input = 'threshold',
                  ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv'))
  AUC = ci.auc(roc)
  AUC = bind_rows(tibble(auc.1 = AUC[1], auc.2 = AUC[2], auc.3 = AUC[3]))
  holdout_diff_pvalue = roc.test(roc,cv_list[[i]], method = 'bootstrap')$p.value
  model_name = model_names[i]
  nvars = length(model$coefnames)
  
  temp = cbind(model_name,nvars,AUC,CIs,holdout_diff_pvalue)
  
  holdout_comparison = rbind(holdout_comparison,temp)
}


names(holdout_comparison)[3:20] = names(as.data.table(df_CIs))[3:20]

RFE_option_performance

holdout_comparison_CIs_cleaned = 
  holdout_comparison %>% 
  mutate_if(is.numeric,round,2) %>%
  mutate(auc = add_ci(auc.1,auc.2,auc.3),
         specificity = add_ci(specificity.V1,specificity.V2,specificity.V3),
         sensitivity = add_ci(sensitivity.V1,sensitivity.V2,sensitivity.V3),
         accuracy = add_ci(accuracy.V1,accuracy.V2,accuracy.V3),
         ppv = add_ci(ppv.V1,ppv.V2,ppv.V3),
         npv = add_ci(npv.V1,npv.V2,npv.V3)) %>%
  select(everything(),-ends_with(c('1','2','3')),holdout_diff_pvalue)

holdout_comparison_CIs_cleaned


