### Basic functions for prediction modeling and performance evaluation

#### 10 fold cross-validation modeling 

train_models = function(df_train, sampling = 'none', nfold = 10){
  set.seed(2021)
  if(sampling == 'none'){
    ### Default setting of 5 times repeated 10-fold CV
    
    trcontrol = trainControl(
      method = 'cv', number = nfold,
      savePredictions = T, verboseIter = T, classProbs = T,
      summaryFunction = twoClassSummary)
    
  } else if (sampling == 'SMOTE'){
    ### Using SMOTE, not utilized in the study
    
    trcontrol = trainControl(
      method = 'cv', number = nfold,
      savePredictions = T, verboseIter = T, classProbs = T,
      summaryFunction = twoClassSummary,
      sampling = 'smote')
    
  }
  
  ### Logistic Regression ###########################
  
  cv_glm = train(
    target ~ ., df_train,  
    method = 'glm',  family = 'binomial',  metric = 'ROC',  na.action = na.omit,
    trControl = trcontrol
  )
  
  
  ### XGBoost ###########################
  set.seed(2021)
  cv_xgb = train(
    target ~ ., df_train,
    method = 'xgbTree',
    metric = 'ROC',
    na.action = na.pass,
    trControl = trcontrol,
    tuneGrid = expand.grid(max_depth = c(2, 4, 6), subsample = c(0.6, 0.8), 
                           colsample_bytree = c(0.6, 0.8), eta = 0.1, nrounds = c(100,200),
                           gamma = 0, min_child_weight = 1)
  )
  
  return(list(cv_glm, cv_xgb))
  
}

#### Performance Evaluation Function ###################

single_cv_conf_mat = function(cv_model, cutoff = 0.5, output = 'stats'){
  
  if('parameter' %in% names(cv_model$pred)){
    cv_result = cv_model$pred
    prob = cv_result %>% pull(Yes)
    obs = cv_result %>% pull(obs)
    
  } else{
    cv_result = suppressMessages(cv_model$bestTune %>% left_join(cv_model$pred))
    prob = cv_result %>% pull(Yes)
    obs = cv_result %>% pull(obs)
    
  }
  
  prob = cv_result %>% pull(Yes)
  obs = cv_result %>% pull(obs)
  result = confusionMatrix(ifelse(prob > cutoff, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No')), obs, positive = 'Yes')
  
  table = result$table
  auc = roc(obs,prob,quiet = T, levels = c('No', 'Yes'))$auc
  sens = result$byClass[[1]]
  spec = result$byClass[[2]]
  ppv = result$byClass[[3]]
  npv = result$byClass[[4]]
  acc = result$overall[[1]]
  
  
  if(output == 'stats'){
    return(tibble(model = cv_model$method, cutoff = cutoff, auc = auc, sens = sens, spec = spec, acc = acc, ppv = ppv, npv = npv))
  } else if(output == 'auc'){
    return(roc(obs,prob,quiet = T, levels = c('No', 'Yes')))
  } else if(output == 'confmat'){
    return(table)
  }
  
}

aggr_result = function(model_list, cutoff){
  
  df_result = tibble()
  
  for( i in 1:length(model_list)){
    
    result = single_cv_conf_mat(model_list[[i]], cutoff = cutoff[i])
    df_result = rbind(df_result, result)
    
  }
  
  return(df_result)
  
}


performance_to_CI = function(result, performance){
  
  roc_glm = single_cv_conf_mat(result[[1]], output = 'auc')
  roc_xgb = single_cv_conf_mat(result[[2]], output = 'auc')
  
  set.seed(2021)
  CIs = 
    bind_rows(ci.coords(roc_glm,
                        x = performance$cutoff[1],
                        input = 'threshold',
                        ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv')),
              
              ci.coords(roc_xgb,
                        x = performance$cutoff[2],
                        input = 'threshold',
                        ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv'))
    )
  
  auc_ci_glm = ci.auc(roc_glm, method = 'bootstrap')
  auc_ci_xgb = ci.auc(roc_xgb, method = 'bootstrap')
  AUC = bind_rows(tibble(auc.1 = auc_ci_glm[1], auc.2 = auc_ci_glm[2], auc.3 = auc_ci_glm[3]),
                  tibble(auc.1 = auc_ci_xgb[1], auc.2 = auc_ci_xgb[2], auc.3 = auc_ci_xgb[3]))
  
  pvalue = roc.test(roc_glm,roc_xgb, method = 'bootstrap')$p.value
  
  df_aggr = cbind(performance %>% select(model,cutoff), AUC, CIs)
  df_aggr$auc_diff_pvalue = pvalue
  
  return(df_aggr)
  
}


rfe_performance = function(rfe, numvars){
  
  cv_result = rfe$pred %>% filter(Variables == numvars)
  prob = cv_result %>% pull(Yes)
  obs = cv_result %>% pull(obs)
  auc = roc(obs,prob,quiet = T, levels = c('No', 'Yes'))
  
  return(auc)
  
}
