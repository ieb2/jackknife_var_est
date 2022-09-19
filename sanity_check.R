small_sanity_check <- lapply(c(1:100), function(x) {
  df <- list_of_sim_data[[x]]
  
  imps <- bootImpute::bootMice(df, nBoot = 300, nImp = 5, nCores = 6, seed = 123, print = FALSE)
  
  estimated_var <- sapply(imps, function(x)
    lm(formula = outcome_variable ~ V1 + V2 + V3, data = x) %>%
      broom::tidy() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  )
  estimated_var_vec <- as.vector(estimated_var)
  
  var_point_estimate <- mean(estimated_var_vec)
  
  UB <- quantile(estimated_var_vec, 0.975)
  LB <- quantile(estimated_var_vec, 0.025)
  
  
  true_var <- list_of_sim_data_for_comp[[x]] %>%
    lm(formula = outcome_variable ~ V1 + V2 + V3, data = .) %>%
    broom::tidy() %>%
    dplyr::filter(term == "V1") %>%
    dplyr::select(estimate) %>%
    unlist()
  
  temp_df <- data.frame("point_estimate" = var_point_estimate, 
                        "UB" = UB, 
                        "LB" = LB, 
                        "true_variance" = true_var) %>% tibble::remove_rownames()
  
  final_df <- temp_df %>%
    mutate("is_between" = ifelse(true_variance > LB & true_variance < UB, TRUE, FALSE))
  
  return(final_df)
})

small_sanity_check_df <- do.call(rbind, small_sanity_check)

CI_coverage <- sum(small_sanity_check_df$is_between)/nrow(small_sanity_check_df) * 100


# Try larger samples 
df_w_mis <- list_of_sim_data[[1]]

bootstrap_samples <- lapply(1:100, function(x) (df_w_mis[sample(1:nrow(df_w_mis), 50, replace=TRUE),]) )

clean_samples <- lapply(bootstrap_samples, function(x) as.data.frame(do.call(cbind, x)) %>%
                          remove_rownames() %>%
                          as.data.frame())

test_estimator <- function(subsamples){
  contains_na <- map_lgl(subsamples, ~any(is.na(.x)))
  
  for(i in 1:length(subsamples)){
    if(contains_na[[i]] == TRUE){
      subsamples[[i]] <- mice(subsamples[[i]], seed = 123, m = 2,method = "pmm", print=FALSE, maxit = 10)
    } else{
      subsamples[[i]] <- subsamples[[i]]
    }
  }
  
  # Logical test to determine presence of MICE objects 
  is_mice <- vector("logical", length = length(subsamples))
  
  for(i in 1:length(subsamples)){
    is_mice[[i]] <- ifelse(class(subsamples[[i]]) == "mids", TRUE, FALSE)
  }
  
  # Applies appt analysis depending on type 
  analysis_vector <- vector("numeric", length = length(subsamples))
  
  for(i in 1:length(subsamples)){
    sample_size <- nrow(complete(subsamples[[i]],1))
    if(is_mice[[i]] == TRUE){
      analysis_vector[[i]] <- subsamples[[i]] %>%
        mice::complete("all") %>%
        map(lm, formula = outcome_variable ~ V1 + V2 + V3) %>%
        map(., summary) %>%
        lapply(., broom::tidy) %>%
        do.call(rbind, .) %>%
        dplyr::filter(term == "V1") %>%
        dplyr::select(estimate) %>%
        unlist() %>%
        mean()
    } else{
      analysis_vector[[i]] <- subsamples[[i]] %>%
        lm(formula = outcome_variable ~ V1 + V2 + V3, data = .) %>%
        broom::tidy() %>%
        dplyr::filter(term == "V1") %>%
        dplyr::select(estimate) %>%
        unlist()
    }
    
  }
  
  # Jackknife point estimate
  
  jittered_analysis_vector <- jitter(analysis_vector)
  
  point_estimate_jackknife <- mean(jittered_analysis_vector)
  
  # Quantile CI
  UB <- quantile(jittered_analysis_vector, 0.975)
  LB <- quantile(jittered_analysis_vector, 0.025)
  
  return(data.frame("point_estimate" = point_estimate_jackknife, 
                    "UB" = UB, 
                    "LB" = LB) %>% tibble::remove_rownames())
  
}

plan(multisession)

boot_check <- future_lapply(clean_samples, function(x) test_estimator(x), future.seed=TRUE)

