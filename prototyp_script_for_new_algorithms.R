choose.int <- function(x, n, k) {
  if(n <= k) return(rep(TRUE, k))
  u <- choose(n-1, k-1)
  pick <- x < u
  if (pick) y <- choose.int(x, n-1, k-1) else y <- choose.int(x-u, n-1, k)
  return(c(pick, y))
}
# n is the sample size of the full datasets (n = 1000 for us)
# k is the # of observations we would like to keep from n = 1000 
# The value inside sapply is the # of subsamples we would like. 
n <- 1000; k <- poss_k

length_sample_vec <- choose(n, k)

if (choose(n, k) > 1e15) {
  length_sample_vec = 1e15
} else
{
  length_sample_vec = choose(n, k)
}

# Column is index for what to include/exclude 

# If choose(n,k) is a big number (~1e15), code fails. 
sample <- sapply(sample.int(length_sample_vec, 500, replace = FALSE, )-1,
                 choose.int, n=n, k=k)

jackknife_estimator <- function(df_w_mis){
  
  drop_d_subsamples <- map(1:ncol(sample),
                           ~df_w_mis[sample[,.x],])
  
  uncon_pred_mat <- make.predictorMatrix(df_w_mis)
  
  uncon_pred_mat[,4] <- 0
  
  # Creates n subsamples of n = n-1 
  
  contains_na <- map_lgl(drop_d_subsamples, ~any(is.na(.x)))
  
  for (i in 1:length(drop_d_subsamples)) {
    if (contains_na[[i]] == TRUE) {
      drop_d_subsamples[[i]] <-
        mice(
          drop_d_subsamples[[i]],
          seed = 123,
          m = number_of_imps,
          method = "pmm",
          print = FALSE,
          maxit = number_of_its,
          predictorMatrix = uncon_pred_mat
        )
    } else{
      drop_d_subsamples[[i]] <- drop_d_subsamples[[i]]
    }
  }
  
  # Logical test to determine presence of MICE objects 
  is_mice <- vector("logical", length = length(drop_d_subsamples))
  
  for(i in 1:length(drop_d_subsamples)){
    is_mice[[i]] <- ifelse(class(drop_d_subsamples[[i]]) == "mids", TRUE, FALSE)
  }
  
  # Applies appt analysis depending on type 
  analysis_vector <- vector("numeric", length = length(drop_d_subsamples))
  
  for(i in 1:length(drop_d_subsamples)){
    sample_size <- 1000
    if(is_mice[[i]] == TRUE){
      analysis_vector[[i]] <- drop_d_subsamples[[i]] %>%
        mice::complete("long") %>%
        lm(outcome_variable ~ V1 + V2 + V3, data = .) %>%
        summary() %>%
        broom::tidy() %>%
        dplyr::filter(term == "V1") %>%
        dplyr::select(estimate) %>%
        unlist()
    } else{
      analysis_vector[[i]] <- drop_d_subsamples[[i]] %>%
        lm(formula = outcome_variable ~ V1 + V2 + V3, data = .) %>%
        summary() %>%
        broom::tidy() %>%
        dplyr::filter(term == "V1") %>%
        dplyr::select(estimate) %>%
        unlist()
    }
    
  }
  
  # Jackknife point estimate
  # jittered_analysis_vector <- jitter(analysis_vector)
  point_estimate_jackknife <- mean(analysis_vector)
  
  # Quantile CI
  UB <- quantile(analysis_vector, 0.975)
  LB <- quantile(analysis_vector, 0.025)
  
  return(data.frame("point_estimate" = point_estimate_jackknife, 
                    "UB" = UB, 
                    "LB" = LB) %>% tibble::remove_rownames())
}