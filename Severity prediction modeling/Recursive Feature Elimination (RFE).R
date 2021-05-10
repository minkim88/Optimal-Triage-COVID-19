
library(SHAPforxgboost)

## Feature importance calculation for RFE
shap_values = shap.values(xgb_model = df_6_result[[2]]$finalModel, X_train = df_6 %>% dplyr::select(-target) %>% as.matrix)
shap_values$mean_shap_score


## RFE (Recursive Feature Elimination) functions

xgbFit = function(x, y, first, last){
  
  set.seed(2021)
  
  train(x = x, y = y,
        method = 'xgbTree',
        metric = 'ROC',
        #        na.action = na.pass,
        trControl = trainControl(method = 'none', summaryFunction = twoClassSummary, classProbs = T, savePredictions = T),
        
        tuneGrid = expand.grid(nrounds = df_6_result[[2]]$bestTune[1] %>% as.numeric,
                               max_depth = df_6_result[[2]]$bestTune[2] %>% as.numeric,
                               eta = df_6_result[[2]]$bestTune[3] %>% as.numeric,
                               gamma = df_6_result[[2]]$bestTune[4] %>% as.numeric,
                               colsample_bytree = df_6_result[[2]]$bestTune[5] %>% as.numeric,
                               min_child_weight = df_6_result[[2]]$bestTune[6] %>% as.numeric,
                               subsample = df_6_result[[2]]$bestTune[7] %>% as.numeric)
  )
  
}

xgbFuncs = list(summary = twoClassSummary,
                
                fit = xgbFit,
                
                pred = function(object, x) {
                  cbind(data.frame(pred = predict(object, x)),
                        as.data.frame(predict(object, x, type = 'prob')))
                  },
                
                rank = function(object, x, y){
                  vimp = data.frame(Overall = shap_values$mean_shap_score,
                                    var = shap_values$mean_shap_score %>% names)
                  vimp
                },
                selectSize = pickSizeBest,
                selectVar = pickVars)

# Index matching from cross validated result
df_6_result_to_index = df_6_result[[2]]$bestTune %>% left_join(df_6_result[[2]]$pred) %>% select(Resample, rowIndex)
cust_index = list()
for( i in 1:(df_6_result_to_index$Resample %>% table() %>% length())){
  cust_index[[i]] = setdiff(1:nrow(df_6),split(df_6_result_to_index$rowIndex,df_6_result_to_index$Resample)[[i]])
}

ctrl = rfeControl(functions = xgbFuncs,
                  method = 'cv',
                  index = cust_index,
                  indexOut = split(df_6_result_to_index$rowIndex,df_6_result_to_index$Resample),
                  verbose = T,
                  rerank = F,
                  saveDetails = T)

set.seed(2021)
xgbProfile = rfe(x = df_6 %>% select(-target,-PREG,-PREGGW),
                 y = df_6$target,
                 sizes = c(1:35),
                 rfeControl = ctrl,
                 metric = 'ROC')



## Minimizing Number of Variables using p-values

# Including laboratory variables
set.seed(2021)
lab_numvars_pvalue_df = tibble()
for(nvars in 1:35){
  pvalue = roc.test(single_cv_conf_mat(df_6_result[[2]], output = 'auc'),rfe_performance(xgbProfile, nvars), method = 'bootstrap')$p.value
  lab_numvars_pvalue_df = bind_rows(lab_numvars_pvalue_df, tibble(nvars = nvars, pvalue = pvalue))
  print(nvars)
}

lab_numvars_pvalue_df = lab_var_order %>% left_join(lab_numvars_pvalue_df %>% mutate(pvalue = round(pvalue,3)), by = c('Order' = 'nvars'))
write.csv(lab_numvars_pvalue_df, 'lab_rfe_pvalues.csv',row.names = F)

# No laboratory variables
set.seed(2021)
nolab_numvars_pvalue_df = tibble()
for(nvars in 1:31){
  pvalue = roc.test(rfe_performance(xgbProfile_nolab, 32),rfe_performance(xgbProfile_nolab, nvars), method = 'bootstrap')$p.value
  nolab_numvars_pvalue_df = bind_rows(nolab_numvars_pvalue_df, tibble(nvars = nvars, pvalue = pvalue))
  print(nvars)
}

nolab_numvars_pvalue_df = nolab_var_order %>% left_join(nolab_numvars_pvalue_df %>% mutate(pvalue = round(pvalue,3)), by = c('Order' = 'nvars'))
nolab_numvars_pvalue_df = nolab_numvars_pvalue_df[1:31,]


# RFE p-value for each iteration
RFE_Performance_Detail_list = 
  lab_numvars_pvalue_df %>% cbind(xgbProfile$results %>% select(ROC)) %>%
  bind_rows(df_6_result[[2]]$results %>% filter(ROC == max(ROC)) %>% select(ROC) %>% mutate(Order = 37)) %>%
  select(Order, ROC, pvalue) %>%
  rename(NumVars = Order, Lab_AUROC = ROC, Lab_Pvalue = pvalue) %>% 
  left_join(
    
    nolab_numvars_pvalue_df %>% cbind(xgbProfile_nolab$results %>% select(ROC) %>% dplyr::slice(1:31)) %>%
      bind_rows(tibble(Order = 32, ROC = xgbProfile_nolab$results$ROC[32])) %>%
      select(Order, ROC, pvalue) %>%
      rename(NumVars = Order, Nolab_AUROC = ROC, Nolab_Pvalue = pvalue),
    by = 'NumVars'
  )

# Variable importance rankings (SHAP)
RFE_Varible_Ranking = 
  lab_var_order %>% bind_rows(tibble(Order = c(37,37), Variable = c('PREG', 'PREGGW'))) %>%
  left_join(df_names) %>% rename(Lab_Variable = Full_Name) %>% select(-Variable) %>%
  left_join(nolab_var_order %>% left_join(df_names)) %>% 
  rename(Nolab_Variable = Full_Name) %>%
  select(-Variable)


##### RFE Result Plots ############################################

## Model including laboratory variables [F+L+] => [F-L+] 

option1 = 17
option1_auc = xgbProfile$results %>% filter(Variables == option1) %>% pull(ROC)

xgbProfile$results %>% filter(Variables > 1) %>% select(Variables, ROC) %>% 
  bind_rows(df_6_result[[2]]$results %>% filter(ROC == max(ROC)) %>% mutate(Variables = 37) %>% select(Variables, ROC)) %>% 
  ggplot(aes(Variables, ROC)) + 
  geom_point() + geom_line(size = 1) +
  labs(title = 'AUC by Number of Variables (2 - 37)', x = 'Number of variables', y = 'Average AUC') +
  geom_vline(xintercept = option1, color = 'red', linetype = 'dotted', size = 1) +
  scale_x_continuous(breaks= c(2,seq(5,35,5), option1, 37)) +
  scale_y_continuous(breaks= c(seq(0.94,0.95,0.01),option1_auc), labels = scales::number_format(accuracy = 0.001))+
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave('XGB RFE Plot (End Point 6) (2 - 37).pdf', width = 10, height = 8)

selectedVars = xgbProfile$variables
option1_vars = xgbProfile$control$functions$selectVar(selectedVars, option1)

lab_var_order = xgbProfile$control$functions$selectVar(selectedVars, 35)
lab_var_order = data.frame(Order = 1:35, Variable = lab_var_order)


## Model without laboratory variables [F+L-] => [F-L-] 

df_6_nolab = df_6 %>% select(-c(HGB, HCT, LYMPHO, PLT, WBC))
set.seed(2021)
xgbProfile_nolab = rfe(x = df_6_nolab %>% select(-target),
                       y = df_6_nolab$target,
                       sizes = c(1:32),
                       rfeControl = ctrl,
                       metric = 'ROC')

option1_nolab = 11
option1_auc_nolab = xgbProfile_nolab$results %>% filter(Variables == option1_nolab) %>% pull(ROC)

xgbProfile_nolab$results %>% filter(Variables > 1) %>% ggplot(aes(Variables, ROC)) + 
  geom_point() + geom_line(size = 1) +
  labs(title = '[No Lab] AUC by Number of Variables (2 - 32)', x = 'Number of variables', y = 'Average AUC') +
  geom_vline(xintercept = option1_nolab, color = 'red', linetype = 'dotted', size = 1) +
  scale_x_continuous(breaks= c(2,seq(5,35,5), option1_nolab)) +
  scale_y_continuous(breaks= c(seq(0.8,0.95,0.01),option1_auc_nolab), labels = scales::number_format(accuracy = 0.001))+
  theme_bw() +
  theme(text = element_text(size = 20))
ggsave('[No Lab] XGB RFE Plot (End Point 6) (2 - 37).pdf', width = 10, height = 8)

selectedVars_nolab = xgbProfile_nolab$variables
option1_vars_nolab = xgbProfile_nolab$control$functions$selectVar(selectedVars_nolab, option1_nolab)

nolab_var_order = xgbProfile_nolab$control$functions$selectVar(selectedVars_nolab, 32)
nolab_var_order = data.frame(Order = 1:32, Variable = nolab_var_order)



### RFE Performance comparison #########################

## F+L+ Model
set.seed(2021)
CIs = ci.coords(single_cv_conf_mat(df_6_result[[2]], output = 'auc'),
                x = 0.0255,
                input = 'threshold',
                ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv'))
AUC = ci.auc(single_cv_conf_mat(df_6_result[[2]], output = 'auc'))
AUC = bind_rows(tibble(auc.1 = AUC[1], auc.2 = AUC[2], auc.3 = AUC[3]))
pvalue = roc.test(single_cv_conf_mat(df_6_result[[2]], output = 'auc'), rfe_performance(xgbProfile_nolab, 32), method = 'bootstrap')$p.value
model = 'lab_full'
nvars = 37
lab_full_performance = cbind(model,nvars,AUC,CIs,pvalue)

## F-L+ Model
set.seed(2021)
CIs = ci.coords(rfe_performance(xgbProfile, option1),
                x = 0.0243,
                input = 'threshold',
                ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv'))
AUC = ci.auc(rfe_performance(xgbProfile, option1))
AUC = bind_rows(tibble(auc.1 = AUC[1], auc.2 = AUC[2], auc.3 = AUC[3]))
pvalue = roc.test(rfe_performance(xgbProfile, option1),rfe_performance(xgbProfile_nolab, option1_nolab), method = 'bootstrap')$p.value
model = 'lab_reduced'
nvars = option1
lab_reduced_performance = cbind(model,nvars,AUC,CIs,pvalue)

## F+L- Model
set.seed(2021)
CIs = ci.coords(rfe_performance(xgbProfile_nolab, 32),
                x = 0.055,
                input = 'threshold',
                ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv'))
AUC_nolab = ci.auc(rfe_performance(xgbProfile_nolab, 32))
AUC = bind_rows(tibble(auc.1 = AUC_nolab[1], auc.2 = AUC_nolab[2], auc.3 = AUC_nolab[3]))
model = 'nolab_full'
nvars = 32

nolab_full_performance = cbind(model,nvars,AUC,CIs)

## F-L- Model
set.seed(2021)
CIs = ci.coords(rfe_performance(xgbProfile_nolab, option1_nolab),
                x = 0.0575,
                input = 'threshold',
                ret = c('accuracy','sensitivity', 'specificity', 'ppv', 'npv'))
AUC_nolab = ci.auc(rfe_performance(xgbProfile_nolab, option1_nolab))
AUC = bind_rows(tibble(auc.1 = AUC_nolab[1], auc.2 = AUC_nolab[2], auc.3 = AUC_nolab[3]))
model = 'nolab_reduced'
nvars = option1_nolab
nolab_reduced_performance = cbind(model,nvars,AUC,CIs)


### Model Performance Aggregation  #########################

RFE_option_performance = bind_rows(lab_full_performance, lab_reduced_performance, nolab_full_performance, nolab_reduced_performance)
names(RFE_option_performance)[3:20] = names(as.data.table(df_CIs))[3:20]

RFE_option_performance

RFE_performance_option_CIs_cleaned = 
  RFE_option_performance %>% 
  mutate_if(is.numeric,round,4) %>%
  mutate(auc = add_ci(auc.1,auc.2,auc.3),
         specificity = add_ci(specificity.V1,specificity.V2,specificity.V3),
         sensitivity = add_ci(sensitivity.V1,sensitivity.V2,sensitivity.V3),
         accuracy = add_ci(accuracy.V1,accuracy.V2,accuracy.V3),
         ppv = add_ci(ppv.V1,ppv.V2,ppv.V3),
         npv = add_ci(npv.V1,npv.V2,npv.V3)) %>%
  select(everything(),pvalue,-ends_with(c('1','2','3')))

RFE_performance_option_CIs_cleaned






###### Creating Plots with Variable Selection ##

### ROC Curve
roc_ci_lab_full = roc_ci_for_graph(rfe_performance(xgbProfile,37),'Lab_Full')
roc_ci_lab_simple = roc_ci_for_graph(rfe_performance(xgbProfile,option1),'Lab_Simple')
roc_ci_nolab_full = roc_ci_for_graph(rfe_performance(xgbProfile_nolab,32),'No_Lab_Full')
roc_ci_nolab_simple = roc_ci_for_graph(rfe_performance(xgbProfile_nolab,option1_nolab),'No_Lab_Simple')

plt = 
  ggroc(list(Lab_Full = rfe_performance(xgbProfile,37), Lab_Simple = rfe_performance(xgbProfile,option1), 
             No_Lab_Full = rfe_performance(xgbProfile_nolab,32), No_Lab_Simple = rfe_performance(xgbProfile_nolab,option1_nolab)), size = 1.3, legacy.axes = T) +
  geom_ribbon(data = bind_rows(roc_ci_lab_full,roc_ci_lab_simple,roc_ci_nolab_full,roc_ci_nolab_simple), 
              aes(x = 1 - specificity, ymin = lower, ymax = upper, group = Model, fill = Model),alpha = 0.2, inherit.aes = F)

valid_roc_all = 
  plt+
  geom_vline(xintercept = c(0,1)) +
  geom_hline(yintercept = c(0,1))+
  geom_abline(slope = 1, size = 0.7, linetype = 'dotted') + 
  ggtitle('XGB ROC by Variables') +
  labs(colour = 'Model')
valid_roc_all
# ggsave('XGB Validation ROC ALL.pdf', width = 10, height = 8)

ggroc(list(Lab_Full = rfe_performance(xgbProfile,37), Lab_Simple = rfe_performance(xgbProfile,option1), 
           No_Lab_Full = rfe_performance(xgbProfile_nolab,32), No_Lab_Simple = rfe_performance(xgbProfile_nolab,option1_nolab)), size = 1.3, legacy.axes = T) +
  geom_vline(xintercept = c(0,1)) +
  geom_hline(yintercept = c(0,1))+
  geom_abline(slope = 1, size = 0.7, linetype = 'dotted') + 
  ggtitle('XGB ROC by Variables') +
  labs(colour = 'Model')
# ggsave('XGB Validation ROC ALL (without CI).pdf', width = 10, height = 8)

### PR Curve

model_names = c('Lab', 'Lab_Option1','Lab_Option2','No_Lab', 'No_Lab_Option1')
for ( i in 1:length(option_list)){
  assign(paste0('df_PR_',i),
         suppressMessages(suppressWarnings(
           coords(
             
             roc(option_list[[i]]$bestTune %>% left_join(option_list[[i]]$pred) %>% pull(obs),
                 option_list[[i]]$bestTune %>% left_join(option_list[[i]]$pred) %>% pull(Yes), levels = c('No', 'Yes')),
             
             'all', ret = c('recall', 'precision'), transpose = F
             
           )
         )) %>%
           mutate(name = model_names[i]))
  
}

df_PR = bind_rows(df_PR_1, df_PR_2,df_PR_3, df_PR_4, df_PR_5)

plt = 
  ggplot(df_PR) +
  geom_path(aes(x= recall, y = precision, color = name), size = 1.2) +
  coord_equal()

PR_Curve = 
  plt+
  ggtitle('XGB PR Curve by Variables') +
  labs(colour = 'Model', x = 'Recall (Sensitivity)', y = 'Precision (PPV)')

PR_Curve
# ggsave('XGB Validation PR ALL.pdf', width = 10, height = 8)


