#### Compare model performances for different endpoints
df_5_result = train_models(df_train = df_5)
df_6_result = train_models(df_train = df_6)
df_7_result = train_models(df_train = df_7)
df_8_result = train_models(df_train = df_8)

### Setting Probability Cutoff (0.9 Specificity)
df_5_performance = aggr_result(df_5_result,c(0.074,0.063))
df_6_performance = aggr_result(df_6_result,c(0.052,0.0255))
df_7_performance = aggr_result(df_7_result,c(0.04,0.048))
df_8_performance = aggr_result(df_8_result,c(0.035,0.043))

### 95% Confidence Intervals for performance
df_5_CIs = performance_to_CI(df_5_result,df_5_performance)
df_6_CIs = performance_to_CI(df_6_result,df_6_performance)
df_7_CIs = performance_to_CI(df_7_result,df_7_performance)
df_8_CIs = performance_to_CI(df_8_result,df_8_performance)

### Saving Results into CSV
write.csv(df_5_CIs %>% mutate_if(is.numeric,round,3), 'Endpoint_5_result.csv', row.names = F)
write.csv(df_6_CIs %>% mutate_if(is.numeric,round,3), 'Endpoint_6_result.csv', row.names = F)
write.csv(df_7_CIs %>% mutate_if(is.numeric,round,3), 'Endpoint_7_result.csv', row.names = F)
write.csv(df_8_CIs %>% mutate_if(is.numeric,round,3), 'Endpoint_8_result.csv', row.names = F)

df_5_CIs$Endpoint = '5' ; df_6_CIs$Endpoint = '6' ; df_7_CIs$Endpoint = '7' ; df_8_CIs$Endpoint = '8'

df_CIs = bind_rows(df_5_CIs, df_6_CIs, df_7_CIs, df_8_CIs)
write.csv(df_CIs %>% mutate_if(is.numeric,round,4), 'CV_Performance_AGGR.csv', row.names = F)

add_ci = function(low, med, high){paste0(med, ' [', low, '-', high,"]")}

df_CIs_cleaned = 
  df_CIs %>% as.data.table %>% 
  mutate_if(is.numeric,round,4) %>%
  mutate(auc = add_ci(auc.1,auc.2,auc.3),
         specificity = add_ci(specificity.V1,specificity.V2,specificity.V3),
         sensitivity = add_ci(sensitivity.V1,sensitivity.V2,sensitivity.V3),
         accuracy = add_ci(accuracy.V1,accuracy.V2,accuracy.V3),
         ppv = add_ci(ppv.V1,ppv.V2,ppv.V3),
         npv = add_ci(npv.V1,npv.V2,npv.V3),
         Endpoint = c('5','5','6','6','7','7','8','8')) %>%
  select(-ends_with(c('1','2','3')))

write.csv(df_CIs_cleaned,'df_CIs_cleaned.csv', row.names = F)
