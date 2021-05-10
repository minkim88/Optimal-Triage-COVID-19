
# Hypothetical Influx Simulation


# Create simulation by R0 values

R10 = create_sir(0.5, 60000)
R9 = create_sir(0.45, 60000)
R8 = create_sir(0.4, 60000)
R7 = create_sir(0.35, 60000)
R6 = create_sir(0.3, 60000)
R5 = create_sir(0.25, 60000)
R4 = create_sir(0.2, 60000)
R3 = create_sir(0.15, 60000)
R2 = create_sir(0.1, 60000)
R1.5 = create_sir(0.075, 60000)

# Repeated simulation
# Time & memory consuming.
# Recommended to used a high-end computer and refresh the session after each simulation
# You can refresh the session in rstudio by using .rs.restartR()

result_R10 = find_threshold(R10, rep = 20)
result_R9 = find_threshold(R9, rep = 20)
result_R8 = find_threshold(R8, rep = 20)
result_R7 = find_threshold(R7, rep = 20)
result_R6 = find_threshold(R6, rep = 20)
result_R5 = find_threshold(R5, rep = 20)
result_R4 = find_threshold(R4, rep = 20)
result_R3 = find_threshold(R3, rep = 20)
result_R2 = find_threshold(R2, rep = 20)
result_R1.5 = find_threshold(R1.5, rep = 20)

R_aggr = bind_rows(result_R1.5 %>% mutate(R0 = 1.5), result_R2 %>% mutate(R0 = 2),
                   result_R4 %>% mutate(R0 = 4), result_R10 %>% mutate(R0 = 10))


# Hypothetical Mortality by Threshold

R_aggr %>% mutate(mortality = death/tot, R0 = factor(R0)) %>%
  group_by(threshold,R0) %>% summarise(mortality = mean(mortality)) %>%
  ggplot(aes(x = threshold,y = mortality, group = R0, color = R0)) +
  geom_line(size = 1.5) +
  geom_point(data = R_aggr %>% mutate(mortality = death/tot, R0 = factor(R0)) %>%
               group_by(threshold,R0) %>% summarise(mortality = mean(mortality)) %>%
               group_by(R0) %>% filter(mortality == min(mortality)) %>% dplyr::slice(1),
             aes(x = threshold, y= mortality, group = R0),color = 'black',inherit.aes = F, size = 2) +
  labs(x = 'Threshold', y = 'Mortality rate') +
  theme_bw() +
  theme(text = element_text(size = 20))

# Hypothetical Mortality by Threshold and Type

R_aggr %>%
  dplyr::select(-death) %>%
  mutate(Type1 = resource_indep/tot, Type2 = resource_dep/tot, Type3 = threshold_dep/tot) %>%
  group_by(R0,threshold) %>% summarise(Type1 = mean(Type1) , Type2 = mean(Type2), Type3 = mean(Type3)) %>%
  melt(id.vars = c('R0', 'threshold')) %>%
  rename(Type = variable, Proportion = value) %>% 
  ggplot(aes(threshold, Proportion, group = Type, fill = Type, color = Type)) +
  geom_bar(stat = 'identity') +
  facet_wrap(vars(R0), labeller = label_both) +
  scale_fill_manual(values = c(Type1 = 'grey80', Type2 = 'grey50', Type3 = 'grey30')) +
  scale_color_manual(values = c(Type1 = 'grey80', Type2 = 'grey50', Type3 = 'grey30'))+
  theme_bw()+
  theme(text = element_text(size = 20))+
  theme(strip.background =element_rect(fill="white"))+
  xlab('Threshold')


# Simulation using J-index

j_index = 0.01309538
j_index_R1.5 = simulation_J_index(R1.5,J_index = j_index, rep = 20)
j_index_R2 = simulation_J_index(R2,J_index = j_index, rep = 20)
j_index_R4 = simulation_J_index(R4,J_index = j_index, rep = 20)
j_index_R6 = simulation_J_index(R6,J_index = j_index, rep = 20)
j_index_R8 = simulation_J_index(R8,J_index = j_index, rep = 20)
j_index_R10 = simulation_J_index(R10,J_index = j_index, rep = 20)


J_R_aggr = bind_rows(j_index_R1.5 %>% mutate(R0 = 1.5), j_index_R2 %>% mutate(R0 = 2),j_index_R4 %>% mutate(R0 = 4), 
                     j_index_R6 %>% mutate(R0 = 6),j_index_R8 %>% mutate(R0 = 8), j_index_R10 %>% mutate(R0 = 10))
R_aggr = bind_rows(result_R1.5 %>% mutate(R0 = 1.5), result_R2 %>% mutate(R0 = 2),result_R4 %>% mutate(R0 = 4), 
                   result_R6 %>% mutate(R0 = 6), result_R8 %>% mutate(R0 = 8), result_R10 %>% mutate(R0 = 10))


# Setting graph ticks & breaks 
my_breaks <- function(x, n = 5, drop = 2) {
  breaks <- if((max(x) %>% between(0.18,0.2))){
    c(0.06, 0.12, 0.18)
  } else if (max(x) > 0.1){
    c(0.1,0.3,0.5)
  } else if (max(x)> 0.01){
    c(0.02,0.03,0.04)
  } else{
    c(0.002,0.004,0.006)
  }
  breaks
}

# Hypothetical Influx Simulation Results

R_aggr %>% mutate(mortality = death/tot, R0 = factor(R0)) %>%
  group_by(threshold, R0) %>% summarise(mortality = mean(mortality)) %>%
  group_by(R0) %>% filter(mortality == min(mortality))  %>% dplyr::slice(1) %>% rename(Opt_mortality = mortality) %>% 
  left_join(J_R_aggr %>% mutate(mortality = death/tot, R0 = factor(R0)) %>%
              group_by(threshold, R0) %>% summarise(mortality = mean(mortality)) %>%
              rename(J_Index = threshold, J_mortality = mortality)) %>%
  dplyr::select(R0, threshold, J_Index, everything()) %>%
  mutate(threshold = threshold,
         `J Index Mortality - Optimized Mortality` = (J_mortality-Opt_mortality)/J_mortality,
         Opt_mortality = Opt_mortality) %>%
  dplyr::select(-J_Index, -J_mortality) %>%
  rename(`Optimal threshold` = threshold,
         `Optimized mortality` = Opt_mortality,
         `Decreased mortality` = `J Index Mortality - Optimized Mortality`) %>%
  melt(id.vars = 'R0') %>%
  ggplot(aes(x = R0, y = value, group = variable)) +
  facet_wrap(vars(variable), scales = 'free_y', ncol = 1,
             labeller = as_labeller(facet_names)) +
  scale_y_continuous(breaks= my_breaks) +
  geom_point(size = 2) + geom_line(size = 1) +
  labs(x = 'R0 (basic reproduction rate)', y = 'Value') +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"))+
  theme(text = element_text(size = 40))



# Creating supplementary table 6 with historical and hypothetical influxes

H_aggr %>% mutate(mortality = death/tot, H = factor(H)) %>%
  group_by(threshold, H) %>% summarise(mortality = mean(mortality)) %>%
  group_by(H) %>% filter(mortality == min(mortality)) %>% rename(Opt_mortality = mortality) %>% 
  left_join(J_H_aggr %>% mutate(mortality = death/tot, H = factor(H)) %>%
              group_by(threshold, H) %>% summarise(mortality = mean(mortality)) %>%
              rename(J_Index = threshold, J_mortality = mortality)) %>%
  dplyr::select(H, threshold, J_Index, everything()) %>%
  mutate(threshold = threshold,
         `J Index Mortality - Optimized Mortality` = (J_mortality-Opt_mortality)/J_mortality,
         Opt_mortality = Opt_mortality) %>%
  dplyr::select(-J_Index, -J_mortality) %>%
  rename(Influx = H, `Optimal Threshold` = threshold, `Optimized Mortality` = Opt_mortality) %>%
  arrange(Influx) %>% mutate(Influx = paste0('H',Influx)) %>%
  rbind(R_aggr %>% mutate(mortality = death/tot, R0 = factor(R0)) %>%
          group_by(threshold, R0) %>% summarise(mortality = mean(mortality)) %>%
          group_by(R0) %>% filter(mortality == min(mortality)) %>% dplyr::slice(1) %>% rename(Opt_mortality = mortality) %>% 
          left_join(J_R_aggr %>% mutate(mortality = death/tot, R0 = factor(R0)) %>%
                      group_by(threshold, R0) %>% summarise(mortality = mean(mortality)) %>%
                      rename(J_Index = threshold, J_mortality = mortality)) %>%
          dplyr::select(R0, threshold, J_Index, everything()) %>%
          mutate(`J Index Mortality - Optimized Mortality` = (J_mortality-Opt_mortality)/J_mortality) %>%
          dplyr::select(-J_Index, -J_mortality) %>%
          rename(Influx = R0, `Optimal Threshold` = threshold, `Optimized Mortality` = Opt_mortality) %>%
          arrange(Influx) %>% mutate(Influx = paste0('R',Influx))
  ) %>%
  rename(`Decreased mortality` = `J Index Mortality - Optimized Mortality`)


### The codes for final results will be simplifed in the close future ###
