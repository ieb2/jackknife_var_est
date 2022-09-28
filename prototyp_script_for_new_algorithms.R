df_w_mis <- list_of_sim_data[[54]]

lapply(list_of_sim_data, function(x) lm(outcome_variable ~ V1 + V2, data = x)) %>%
  map(., tidy) %>%
  do.call(rbind, .) %>%
  dplyr::filter(term == "V1") %>%
  dplyr::select(estimate) %>%
  unlist() %>%
  as.vector() %>%
  mean()

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

################ Playing around with analysis vector ################ 

library(posterior)
library(boot)
library(arm)

# Random sample from posterior dist of analysis model. 
sim_from_prior_analysis <- 
  bayesglm(formula = outcome_variable ~ V1 + V2 + V3, data = df_w_mis) %>%
  sim(n.sims = length(analysis_vector)) %>%
  coef() %>%
  as.data.frame() %>%
  select(V1) %>%
  unlist() %>% 
  as.vector()

# Random sample from posterior dist of imp model. 
sim_from_prior_imp <- 
  bayesglm(formula = outcome_variable ~ V1 + V2, data = df_w_mis) %>%
  sim(n.sims = length(analysis_vector)) %>%
  coef() %>%
  as.data.frame() %>%
  select(V1) %>%
  unlist() %>% 
  as.vector()

# Trying to make sense of Bayesian analysis in this context. 
# In our case, the person doing the analysis either has access to 
# the imputation or analysis model, as well as the jackknife estimates. 

data.frame(sim_from_prior_imp , sim_from_prior_analysis, analysis_vector) %>%
  reshape2::melt() %>%
  ggplot(., aes(x = value, fill = variable)) + 
  geom_density() + 
  #geom_density(aes(mean_dist, fill = "mean_dist"), data = q) +
  theme_classic() 

sample_betas <- sim_from_prior_imp

# Assuming betas come from normal dist with following parameters. 
rnorm(n = length(sample_betas), mean =  mean(sample_betas), sd = sd(sample_betas))
like <- dnorm(x = analysis_vector, mean = mean(sample_betas), sd = sd(sample_betas)) 

par(mfrow = c( 1,2 ) ) 
beta_distr <- rnorm(n = 100000, mean(sample_betas), sd = sd(sample_betas))
p1 <- hist(beta_distr, 60, col = "lightgreen")

possible_betas <- seq(-3, 3, by = 0.01)

likelihood <- dnorm(possible_betas, mean(sample_betas), sd = sd(sample_betas))

p2 <- plot( x = possible_betas,
            y = likelihood,
            type = 'h', 
            xlim=c(1,3))
