jackknife_prototype_2 <- function(df_w_mis){
  
  imps <- 
    mice(df_w_mis, m = 2, maxit = 1, method = "pmm", print = FALSE) %>%
    complete("long")
  
  reg_data <- imps %>%
    by(as.factor(.$.imp), lm, formula = outcome_variable ~ V1 + V2 + V3) %>%
    pool() %>%
    tidy() %>%
    filter(term == "V1") %>%
    select(riv, df, fmi) %>%
    as.data.frame()
  
  length_sample_vec <- choose(n, k)
  
  if (choose(n, k) > 1e15) {
    length_sample_vec = 1e15
  } else {
    length_sample_vec = choose(n, k)
  }
  
  # Column is index for what to include/exclude 
  
  # If choose(n,k) is a big number (~1e15), code fails. 
  n_sub_samples <- 1e3
  
  sample <- 
    sapply(sample.int(length_sample_vec, n_sub_samples, replace = FALSE)-1,
           choose.int, n=nrow(imps), k=50)
  
  drop_d_subsamples <- map(1:ncol(sample),
                           ~imps[sample[,.x],])
  
  non_mice_res <- map(drop_d_subsamples, non_mice_analyzer)
  
  analysis_vector <- non_mice_res %>%
    do.call(rbind, .) %>%
    as.vector()
  
  re <- 1 / (1 + (reg_data$fmi/2))
  
  sd_for_noise <-  (1 + sqrt(reg_data$riv))/re
  
  noisy_vector <- analysis_vector + 
    rnorm(length(analysis_vector), mean = 0, sd = sd_for_noise)
  
  point_estimate_jackknife <- mean(noisy_vector)
  
  # Var_based CI 
  
  sse <- sum((noisy_vector - point_estimate_jackknife)^2)
  
  var <- ((1)/(nrow(df_w_mis) * (nrow(df_w_mis)-1)))*sse
  
  crit_val <- qt(p=0.025, df=reg_data$df, lower.tail=FALSE)
  
  bound <- crit_val*sqrt(var/nrow(df_w_mis))
  
  UB_var <- point_estimate_jackknife + bound
  LB_var <- point_estimate_jackknife - bound
  
  # Quantile CI
  UB <- quantile(noisy_vector, 0.975)
  LB <- quantile(noisy_vector, 0.025)
  
  return(data.frame("point_estimate" = point_estimate_jackknife, 
                    "UB" = UB, 
                    "LB" = LB, 
                    "UB_var" = UB_var, 
                    "LB_var" = LB_var) %>% tibble::remove_rownames())
  
}

tic()
jackknife_prototype_2(df_w_mis)
toc()