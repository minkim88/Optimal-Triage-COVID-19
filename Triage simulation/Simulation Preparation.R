
# Simulation Preparation

# Out-Of-Fold Prediction Probabilities from F-L+ Model

pred = fread('Probability_Severity_FINAL_REDUCED.csv')
pred$obs = factor(pred$obs, levels = c('Yes', 'No'))

coords(roc(pred$obs,pred$Yes,quiet = T, levels = c('Yes', 'No')),x = 'best', best.method = 'youden') %>% print

# J-index for F-L+ Model: 0.01309538


# Validating generated probabilities from inverse transform sampling

model_yes = pred %>% filter(obs == 'Yes') %>% pull(Yes)
model_no = pred %>% filter(obs == 'No') %>% pull(Yes)
simulated_yes = simulate_distribution(pred,R10) %>% filter(target == 1) %>% pull(prob)
simulated_no = simulate_distribution(pred,R10) %>% filter(target == 0) %>% pull(prob)

bind_rows(tibble(Type = 'Model: severe', Probability = model_yes %>% boot::logit()), 
          tibble(Type = 'Generated: severe', Probability = simulated_yes%>% boot::logit()),
          tibble(Type = 'Model: non-severe', Probability = model_no %>% boot::logit()), 
          tibble(Type = 'Generated: non-severe', Probability = simulated_no %>% boot::logit())) %>%
  mutate(Type = factor(Type, levels = c('Model: severe', 'Generated: severe', 'Model: non-severe', 'Generated: non-severe'))) %>%
  ggplot(aes(x= Probability, group = Type, fill = Type)) +
  facet_wrap(vars(Type)) +
  geom_density(bw='SJ') +
  scale_fill_manual(values = c(`Model: severe` = 'grey50', `Generated: severe` = 'grey50', 
                               `Model: non-severe` = 'grey80', `Generated: non-severe` = 'grey80')) +
  theme_bw() +
  theme(text = element_text(size = 20), legend.position = "none")+
  theme(strip.background =element_rect(fill="white"))+
  labs(x = 'Logits', y = 'Density')
