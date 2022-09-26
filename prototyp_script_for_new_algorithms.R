# Try to come up with a way to justify the amount of noise added. 
combn(analysis_vector, 2)

df_w_mis <- list_of_sim_data[[10]]

uncon_pred_mat <- make.predictorMatrix(df_w_mis)

uncon_pred_mat[,4] <- 0

# Creates n subsamples of n = n-1 
subsamples <- vector("list", nrow(df_w_mis))
for(i in 1:nrow(df_w_mis)){
  subsamples[[i]] <- df_w_mis[-i,]}

contains_na <- map_lgl(subsamples, ~any(is.na(.x)))

for(i in 1:length(subsamples)){
  if(contains_na[[i]] == TRUE){
    subsamples[[i]] <- mice(subsamples[[i]], seed = 123, m = 2, method = "norm.boot", 
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
      group_by(.imp) %>%
      do(model = lm(outcome_variable ~ V1 + V2 + V3, data = .)) %>%
      as.list() %>%
      .[[-1]] %>%
      pool() %>%
      summary() %>%
      as.data.frame() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist() %>%
      mean()
  } else{
    analysis_vector[[i]] <- subsamples[[i]] %>%
      lm(formula = outcome_variable ~ V1 + V2 + V3, data = .) %>%
      summary() %>%
      as.data.frame() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  }
  
}

# Jackknife point estimate



jittered_analysis_vector <- jitter(analysis_vector, factor = 10000)
point_estimate_jackknife <- mean(jittered_analysis_vector)

# Quantile CI
UB <- quantile(jittered_analysis_vector, 0.975)
LB <- quantile(jittered_analysis_vector, 0.025)

print(data.frame("point_estimate" = point_estimate_jackknife, 
                 "UB" = UB, 
                 "LB" = LB) %>% tibble::remove_rownames())

################ Playing around with analysis vector ################ 

library(boot)

meanfun <- function(data, i){
  d <- data[i]
  return(median(d))   
}

results <- boot(data=jittered_analysis_vector, statistic = meanfun, R=1e5)

plot(results)

# get 95% confidence interval
boot.ci(results, type="bca")

lm(formula = outcome_variable ~ V1 + V2 + V3, data = df_w_mis)

regfun <- function(data, i){
  d <- data[i, ]
  mod <- lm(formula = outcome_variable ~ V1 + V2 + V3, data = d)
  summary(mod) %>%
    broom::tidy() %>%
    as.data.frame() %>%
    dplyr::filter(term == "V1") %>%
    dplyr::select(estimate) %>%
    unlist()
}

reg_res <- boot(data = df_w_mis, statistic = regfun, R=1e3)
plot(reg_res)
boot.ci(reg_res, type="bca")


