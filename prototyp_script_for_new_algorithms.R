df_w_mis <- list_of_sim_data[[54]]

choose.int <- function(x, n, k) {
  if(n <= k) return(rep(TRUE, k))
  u <- choose(n-1, k-1)
  pick <- x < u
  if (pick) y <- choose.int(x, n-1, k-1) else y <- choose.int(x-u, n-1, k)
  return(c(pick, y))
}
# n is the sample size of the subsamples 
# k s the number of subsamples desired 
n <- 100; k <- 10

# This code selects 1000 disjoint k subsets of size n from the dataframe 
# Column is index for what to include/exclude 
sample <- sapply(sample.int(choose(n, k), 1000)-1, choose.int, n=n, k=k)

drop_d_subsamples <- map(1:ncol(sample),
                         ~df_w_mis[sample[,.x],])

drop_d_subsamples <- map(drop_d_subsamples, ~as_tibble(.x))

uncon_pred_mat <- make.predictorMatrix(df_w_mis)

uncon_pred_mat[,4] <- 0

# Creates n subsamples of n = n-1 
subsamples <- vector("list", nrow(df_w_mis))
for(i in 1:nrow(df_w_mis)){
  subsamples[[i]] <- df_w_mis[-i,]}

contains_na <- map_lgl(subsamples, ~any(is.na(.x)))

for(i in 1:length(subsamples)){
  if(contains_na[[i]] == TRUE){
    subsamples[[i]] <- mice(subsamples[[i]], seed = 123, m = 2, method = "pmm", 
                            print=FALSE, maxit = 1, predictorMatrix = uncon_pred_mat)
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
      mice::complete("long") %>%
      lm(outcome_variable ~ V1 + V2 + V3, data = .) %>%
      summary() %>%
      broom::tidy() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  } else{
    analysis_vector[[i]] <- subsamples[[i]] %>%
      lm(formula = outcome_variable ~ V1 + V2 + V3, data = .) %>%
      summary() %>%
      broom::tidy() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  }
  
}

# Jackknife point estimate
jittered_analysis_vector <- jitter(analysis_vector, factor = 1000)
point_estimate_jackknife <- mean(jittered_analysis_vector)

density(jittered_analysis_vector) %>%
  plot()

# Quantile CI
UB <- quantile(jittered_analysis_vector, 0.975)
LB <- quantile(jittered_analysis_vector, 0.025)

print(data.frame("point_estimate" = point_estimate_jackknife, 
                 "UB" = UB, 
                 "LB" = LB) %>% tibble::remove_rownames())

