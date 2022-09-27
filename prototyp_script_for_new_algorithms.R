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
    subsamples[[i]] <- mice(subsamples[[i]], seed = 123, m = 1, method = "pmm", 
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

sim_from_prior_analysis <- 
  bayesglm(formula = outcome_variable ~ V1 + V2 + V3, data = df_w_mis) %>%
  sim(n.sims = length(analysis_vector)) %>%
  coef() %>%
  as.data.frame() %>%
  select(V1) %>%
  unlist() %>% 
  as.vector()

sim_from_prior_imp <- 
  bayesglm(formula = outcome_variable ~ V1 + V2, data = df_w_mis) %>%
  sim(n.sims = length(analysis_vector)) %>%
  coef() %>%
  as.data.frame() %>%
  select(V1) %>%
  unlist() %>% 
  as.vector()

data.frame(sim_from_prior_imp , sim_from_prior_analysis, analysis_vector) %>%
  reshape2::melt() %>%
  ggplot(., aes(x = value, fill = variable)) + 
  geom_density() + 
  #geom_density(aes(mean_dist, fill = "mean_dist"), data = q) +
  theme_classic() + 
  expand_limits(y=c(0, 1))

dens_prior_analysis <- density(sim_from_prior_analysis)
dens_prior_imputation <- density(sim_from_prior_imp)
dens_analysis_vector <- density(analysis_vector)


lik <- dens_analysis_vector$y
 
prior <- dens_prior_analysis$y

raw_posterior <- lik * prior 

standardized_posterior <- raw_posterior/sum(raw_posterior)

rangeP <- seq(0, 1, length.out = length(lik))

plot(rangeP, lik,
     type = "l", xlab = "P(Black)", ylab = "Density", ylim = c(0,30))
lines(rangeP, prior,
      col = "red")
lines(rangeP, raw_posterior, col = "green")
lines(rangeP, standardized_posterior, col = "blue")
legend("topleft", legend = c("Lik", "Prior", "Unstd Post", "Post"),
       text.col = 1:4, bty = "n") 



pp <- data.frame(density(sim_from_prior_imp)$y, density(sim_from_prior_analysis)$y,
                 density(analysis_vector)$y) %>%
  reshape2::melt() %>%
  ggplot(., aes(x = value, fill = variable)) + 
  geom_density() + 
  geom_density(aes(x = values, fill = "standardized_posterior"),
               data.frame(values = standardized_posterior)) 

  plotly::ggplotly(pp)

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

analysis_vector_split <- 
  split(jittered_analysis_vector, 
        cut(jittered_analysis_vector, 
            quantile(jittered_analysis_vector, prob = c(0, 0.25, 0.50, 0.75, 1), names = FALSE), include = TRUE)) %>%
  map(., ~mean(.x)) %>%
  unlist() %>%
  weighted.mean(., w=c(0.7, 0.1, 0.1, 0.1))