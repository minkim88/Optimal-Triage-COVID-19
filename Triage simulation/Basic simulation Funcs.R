## Basic Simulation Functions


# Uniform distribution of intraday arrival time

create_distribution = function(n){
  newx = c()
  x = 1:length(n)
  
  for( i in x){
    temp = seq(x[i], x[i] + 1, length.out = n[i])
    newx = c(newx, temp)
  }
  
  return(newx)
  
}

# Inverse transform sampling

simulate_distribution = function(pred, n, seed = 0){
  
  set.seed(seed)
  proportion = pred$obs %>% table %>% prop.table %>% round(3)
  
  pdf_0 = density(pred %>% filter(obs == 'No') %>% pull(Yes) %>% boot::logit(), bw = 'SJ')
  pdf_1 = density(pred %>% filter(obs == 'Yes') %>% pull(Yes) %>% boot::logit(), bw = 'SJ')
  
  non_severe_sim = approxfun(cumsum(pdf_0$y)/sum(pdf_0$y), pdf_0$x) 
  severe_sim = approxfun(cumsum(pdf_1$y)/sum(pdf_1$y), pdf_1$x)
  
  wave1_non_severe_sim = non_severe_sim(runif(round(sum(n)*proportion[2]))) %>% boot::inv.logit()
  wave1_severe_sim = severe_sim(runif(round(sum(n)*proportion[1]))) %>% boot::inv.logit()
  
  wave_1 = create_distribution(n)
  lagged = lag(sort(wave_1))
  lagged[1] = 0
  wave_1_interval = sort(wave_1) - lagged
  
  prob_target = tibble(prob = c(wave1_non_severe_sim, wave1_severe_sim),
                       target = c(rep(0, times = length(wave1_non_severe_sim)), rep(1, times = length(wave1_severe_sim)))) 
  prob_target = prob_target[sample(1:nrow(prob_target), size = nrow(prob_target), replace = F),]
  patient_df = tibble(time = wave_1_interval %>% round(3), 
                      ID = 1:length(wave_1_interval))
  
  patient_df = cbind(patient_df, prob_target) %>% as_tibble()
  
  return(patient_df)
  
}


# Descrete event simulation

Cure = 0.493
run_simulation = function(patient_df, seed = 0, threshold = 0.05, Cure = 0.493){
  
  set.seed(seed)
  patient =
    trajectory("Patient's Stay") %>%
    simmer::select(function(){
      return(ifelse(get_attribute(ICU, 'prob') < threshold, 'Self', 'Bed'))
    }) %>%
    seize_selected(1) %>%
    timeout(function(){
      admission = get_selected(ICU)
      target = get_attribute(ICU, 'target')
      return(
        if(admission == 'Bed' & target == 1){
          ifelse(rbinom(1,size = 1, prob = 1 - Cure) == 1, 
                 rgamma(1, shape = 1.5488, rate = 0.1331) %>% round(2),                 # dead
                 (rgamma(1, shape = 0.8904, rate = 0.0477) %>% round(2) + 0.0001))      # alive
        } else if(admission == 'Bed' & target == 0){
          rnorm(1, mean = 20, sd = 5) %>% round(3)
        } else {
          Inf
        }
      )
    }) %>%
    release_selected(1) 
  
  ICU = simmer('ICU') %>%
    add_resource("Bed",504, queue_size = 0, queue_size_strict = T) %>%
    add_resource("Self",Inf, queue_size = Inf) %>%
    add_dataframe("Patient", patient, patient_df,mon = 2)
  
  ICU %>% run(until = Inf)
  
  return(ICU)
  
}

# Aggregating DES results

count_death = function(sim, data, Cure = 0.507, Self = 0.01){
  
  arrival = sim %>% get_mon_arrivals() %>% tibble
  resource = sim %>% get_mon_resources() %>% tibble
  attribute = sim %>% get_mon_attributes() %>% tibble
  
  arrival %<>% 
    left_join(attribute %>% filter(key == 'prob') %>% rename(prob = value) %>% dplyr::select(name, prob), by = 'name') %>%
    left_join(attribute %>% filter(key == 'target') %>% rename(Severity = value) %>% dplyr::select(name, Severity), by = 'name') %>%
    left_join(attribute %>% filter(key == 'ID') %>% rename(ID = value) %>% dplyr::select(name, ID), by = 'name')
  
  arrival %<>% mutate(type1 = ifelse(nchar(activity_time) == 7, 1, 0))
  TP = arrival %>% filter(end_time != Inf & finished == 'TRUE' & Severity == 1) %>% nrow
  FP = arrival %>% filter(end_time != Inf & finished == 'TRUE' & Severity == 0) %>% nrow
  TP_No_Bed = arrival %>% filter(end_time != Inf & finished == 'FALSE' & Severity == 1) %>% nrow
  FP_No_Bed = arrival %>% filter(end_time != Inf & finished == 'FALSE' & Severity == 0) %>% nrow
  FN = arrival %>% filter(end_time == Inf & Severity == 1) %>% nrow
  TN = arrival %>% filter(end_time == Inf & Severity == 0) %>% nrow
  
  resource_indep = sum(arrival$type1)
  resource_dep = as.integer(TP_No_Bed*(1 - Self))
  threshold_dep = as.integer(FN*(1 - Self))
  
  Tot_Death = resource_indep + resource_dep + threshold_dep
  #print(paste0('Total Utility: ', Tot_Utility))
  #print(paste0('Total Death: ', Tot_Death))
  return(list(death = Tot_Death,
              resource_indep = resource_indep,
              resource_dep = resource_dep,
              threshold_dep = threshold_dep))
  
}

# Repeat simulation to achieve consistent results

find_threshold = function(patient_flow, rep = rep, seed = 1, name){
  sim_df = tibble()
  
  for ( i in 1:rep){
    
    patient_df = simulate_distribution(pred, patient_flow, seed = seed)
    
    for( threshold in seq(0,1,0.01)){
      
      sim = run_simulation(patient_df = patient_df, seed = seed, threshold = threshold)
      temp = count_death(sim, patient_df)
      
      sim_df = bind_rows(sim_df, 
                         tibble(rep = i, threshold = threshold, death = temp$death, resource_indep = temp$resource_indep,
                                resource_dep = temp$resource_dep, threshold_dep = temp$threshold_dep, tot = sum(patient_flow)))
    }
    seed = seed + 1  
    print(i)
    gc()
  }
  
  return(sim_df)
  
}
