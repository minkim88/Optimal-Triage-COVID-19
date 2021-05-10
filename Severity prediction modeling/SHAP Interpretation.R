library(ggplot2)
library(SHAPforxgboost)
library(gridExtra)
library(grid)

df_6_result = readRDS('C:/Users/admin/Desktop/Final/df_6_result')

shap_values = shap.values(xgb_model = df_6_result[[2]]$finalModel, X_train = df_6 %>% dplyr::select(-target) %>% as.matrix)
shap_long = shap.prep(xgb_model = df_6_result[[2]]$finalModel, X_train = df_6 %>% dplyr::select(-target) %>% as.matrix)

# Join full names dataset for fully named plots

summary_plot_df = 
  shap_long %>% filter(mean_value > 0) %>% mutate(variable = factor(variable)) %>% 
  left_join(df_names, by = c('variable' = 'Variable')) %>%
  mutate(variable = Full_Name) %>% 
  mutate(variable = factor(variable,levels = variable %>% unique)) %>% select(-Full_Name)

# Shap summary plot (Beeswarm plot)
data_long = summary_plot_df

ggplot(data = data_long) +
  coord_flip(ylim = c(-4,4)) +
  geom_hline(yintercept = 0) +
  ggforce::geom_sina(aes(x = variable, y = value, color = stdfvalue), method = 'counts', maxwidth = 0.7, alpha = 0.8) +
  scale_color_gradient(low = '#FFCC33', high = '#6600CC',
                       breaks = c(0, 1), labels = c(' Low', 'High '),
                       guide = guide_colorbar(barwidth = 12, barheight = 0.3)) +
  theme_bw() +
  theme(axis.line.y = element_blank(), axis.ticks.y = element_blank(),
        legend.position = 'bottom', legend.title = element_text(size = 10),
        legend.text = element_text(size = 8), axis.title.x = element_text(size = 10))+
  scale_x_discrete(limits = rev(levels(data_long$variable)),
                   labels = rev(levels(data_long$variable))) +
  labs(y = 'SHAP value', x = "", color = "Variable value    ") +
  guides(color = guide_colorbar(barwidth = 10, barheight = 0.5)) +
  theme(legend.title = element_text(size = 20),
        legend.text = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.y = element_text(size = 20, color = 'black'),
        axis.text.x.bottom = element_text(size = 20))

ggsave('SHAP Summary Plot.pdf', width = 10, height = 8)


# SHAP Dependence Plot (Top 9)
fig_list = lapply(as.character(summary_plot_df$variable %>% unique)[1:9],
                  shap.plot.dependence, data_long = summary_plot_df, add_hist = T, dilute = F, smooth = F)
grid.arrange(grobs = fig_list, ncol = 3)


# Modified dependence plot function

## TBU ##



### Individual Case Explanation #################

# Patient-specific interpretation plots

holdout_full = holdout %>% 
  mutate(target = ifelse(CSS >=6, 'Yes', 'No') %>% factor(levels = c('Yes','No'), labels = c('Yes', 'No'))) %>%
  dplyr::select(-c(CURSIT, OUTCOME, id, PERIOD, CSS))

model = df_6_result[[2]]$finalModel
test_data = holdout_full %>% dplyr::select(-target)

plot_shap_indiv = function(model, test_data, case_number, top_n){
  
  df = test_data
  test_data %<>% dplyr::select(-target)
  pred_result = data.frame(pred = predict(model,
                                          newdata = test_data %>% as.matrix %>% xgb.DMatrix))
  shap_result = predict(model,
                        newdata =  test_data %>% as.matrix %>% xgb.DMatrix,
                        predcontrib = T) %>% as_tibble %>% dplyr::select(-BIAS)
  
  colnames(shap_result) = paste0('shap_', colnames(shap_result))
  result = cbind(test_data, shap_result, pred_result)
  
  input_temp = result %>% dplyr::select(-contains('shap_')) %>% dplyr::slice(case_number)
  pred = input_temp$pred
  input_temp = input_temp %>% dplyr::select(-pred)
  
  shap_temp = suppressMessages(inner_join(result, input_temp)) %>%
    dplyr::select(contains('shap_')) %>%
    gather(shap_features, shap_values, contains('shap_')) %>%
    mutate(shap_sign = ifelse(shap_values > 0, '+', '-') %>% factor(levels = c("+", "-"))) %>%
    arrange(desc(abs(shap_values))) %>%
    dplyr::slice(1:top_n)
  
  ylim_temp = result %>% 
    dplyr::select(contains('shap_')) %>% 
    gather(shap_features, shap_values, contains('shap_')) %>% 
    pull(shap_values) %>%
    max %>% abs
  
  plt = ggplot(shap_temp %>% mutate(shap_features = gsub('shap_','',shap_features), Contribution = shap_sign) %>% 
                 left_join(df_names, by = c('shap_features' = 'Variable')) %>% dplyr::select(-shap_features) %>% rename(shap_features = Full_Name),
               aes(x = reorder(shap_features, abs(shap_values)), y = shap_values, fill = Contribution)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_y_continuous(limits = c(-2, 2)) + # can be changed to -ylim_temp & +ylim_temp
    scale_fill_manual(values = c('grey20', 'grey50'))+
    labs(x = 'Variables', y = 'SHAP values')+
    theme_bw()+
    theme(plot.margin = unit(c(4.5,1,1,1), 'lines'),text = element_text(size = 25),
          axis.text.x = element_text(color = 'black'), axis.text.y = element_text(color = 'black'), legend.position = 'none')
  
  print(plt)
  
  grid.text(paste0(#'Case: ', case_number, '\n',
                   'Actual ', ifelse(df$target[case_number] == 'Yes','severe patient', 'non-severe patient'), '\n',
                   'Probability: ', round(pred,3), '\n'
                   ),
            x= 0.45, y= 0.92, gp = gpar(fontsize = 20,  fontface = 'bold'),
            just = 'left')
  
}


plot_shap_indiv(df_6_result[[2]]$finalModel, holdout_full, case_number = 14, top_n = 10) # Case 14
plot_shap_indiv(df_6_result[[2]]$finalModel, holdout_full, case_number = 22, top_n = 10) # Case 22
