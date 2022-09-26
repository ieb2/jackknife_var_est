df_w_mis <- list_of_sim_data[[20]]

lm(outcome_variable ~ ., data = df_w_mis)

uncon_pred_mat <- make.predictorMatrix(df_w_mis)

uncon_pred_mat[,4] <- 0

# Creates n subsamples of n = n-1 
subsamples <- vector("list", nrow(df_w_mis))
for(i in 1:nrow(df_w_mis)){
  subsamples[[i]] <- df_w_mis[-i,]}

contains_na <- map_lgl(subsamples, ~any(is.na(.x)))

for(i in 1:length(subsamples)){
  if(contains_na[[i]] == TRUE){
    subsamples[[i]] <- mice(subsamples[[i]], seed = 123, m = 2, method = "norm.nob", 
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
jittered_analysis_vector <- jitter(analysis_vector, factor = 10000)
point_estimate_jackknife <- mean(jittered_analysis_vector)

density(jittered_analysis_vector) %>%
  plot()

analysis_vector_split <- 
  split(jittered_analysis_vector, 
        cut(jittered_analysis_vector, 
            quantile(jittered_analysis_vector, prob = c(0, 0.25, 0.50, 0.75, 1), names = FALSE), include = TRUE)) %>%
  map(., ~mean(.x)) %>%
  unlist() %>%
  weighted.mean(., w=c(0.7, 0.1, 0.1, 0.1))

# Quantile CI
UB <- quantile(jittered_analysis_vector, 0.975)
LB <- quantile(jittered_analysis_vector, 0.025)

print(data.frame("point_estimate" = point_estimate_jackknife, 
                 "UB" = UB, 
                 "LB" = LB) %>% tibble::remove_rownames())

################ Playing around with analysis vector ################ 

library(posterior)

library(boot)
library(arm)
sim_from_prior_analysis <- bayesglm(formula = outcome_variable ~ V1 + V2 + V3, data = df_w_mis) %>%
  sim(n.sims = 1e4) 

prior_analysis_V1 <- coef(sim_from_prior_analysis) %>%
  as.data.frame() %>%
  select(V1) %>%
  unlist() %>% 
  as.vector()

sim_from_prior_imp <- bayesglm(formula = outcome_variable ~ V1 + V2, data = df_w_mis) %>%
  sim(n.sims = 1e4) 

prior_imp_V1 <- coef(sim_from_prior_imp) %>%
  as.data.frame() %>%
  select(V1) %>%
  unlist() %>% 
  as.vector()

ks.test(x = analysis_vector, y = prior_analysis_V1)
ks.test(x = analysis_vector, y = prior_imp_V1)

plot(ecdf(analysis_vector), verticals=TRUE, do.points=FALSE, col="blue") 
plot(ecdf(prior_analysis_V1), verticals=TRUE, do.points=FALSE, col="green", add=TRUE) 
  
View(sim_from_prior_analysis)

mean.fun <- function(d, i) {
  m = mean(d[i])
  n = length(d)
  v = (n-1)*var(d[i])/n^2
  c(m, v) }

meanfun <- function(data, i){
  d <- data[i]
  return(mean(d))   
}

results <- boot(data=jittered_analysis_vector, statistic = mean.fun, R=1e4)

plot(results)

# get 95% confidence interval
boot.ci(results, type="stud")

regfun <- function(data, i){
  d <- data[i, ]
  mod <- lm(formula = outcome_variable ~ V1 + V2 + V3, data = d)
    broom::tidy(mod) %>%
    as.data.frame() %>%
    dplyr::filter(term == "V1") %>%
    dplyr::select(estimate) %>%
    unlist()
}

reg_res <- boot(data = df_w_mis, statistic = regfun, R=1e3)
plot(reg_res)
boot.ci(reg_res, type="perc")

# Try to come up with a way to justify the amount of noise added. 
combn(analysis_vector, 2)