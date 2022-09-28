# Scratch Code--Likely Trash 

imps <- mice(df_w_mis, m = 10, maxit = 50, method = "pmm", print = FALSE) %>%
  complete("all")

subsamples <- vector("list", length = {length(imps)})

for(i in 1:length(imps)) {
  for (z in 1:nrow(imps[[i]])) {
    subsamples[[i]][[z]] <- imps[[i]][-z,]
  }
}

analysis_vector <- c()
for(i in 1:length(imps)) {
  for (z in 1:nrow(imps[[i]])) {
  analysis_vector[[i]]  <- subsamples[[i]][[z]] %>%
      lm(outcome_variable ~ V1 + V2 + V3, data = .) %>%
      summary() %>%
      broom::tidy() %>%
      dplyr::filter(term == "V1") %>%
      dplyr::select(estimate) %>%
      unlist()
  }
}


mean(unlist(analysis_vector))

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

mu <- seq( 0, 10, by = 0.5 )
sigma <- seq( 0.1, 10, by = 0.3 )
pars <- expand.grid( mu = mu, sigma = sigma )

plot( pars, pch = 19, main = 'The Parameter Space' )

pars$mu_prior <- dnorm( pars$mu, mean = 18, sd = 5 )
pars$sigma_prior <- dunif( pars$sigma, min = 0, max = 10 )
pars$prior <- pars$mu_prior * pars$sigma_prior
for( i in 1:nrow( pars ) ) {
  likelihoods <- dnorm( temp, pars$mu[i], pars$sigma[i])
  pars$likelihood[i] <- prod( likelihoods )
}

#calculate the posterior probability
pars$probability <- pars$likelihood * pars$prior
pars$probability <- pars$probability / sum( pars$probability )









res <- seq(0, 1, length.out = length(analysis_vector)) 

plot(rangeP, dens_prior_imputation,
     type = "l", xlab = "P(Black)", ylab = "Density")

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
beta_distr <- analysis_vector
p1 <- hist(beta_distr, 60, col = "lightgreen")

possible_betas <- c(sim_from_prior_imp, analysis_vector)

likelihood <- dnorm(possible_betas, mean(analysis_vector), sd = sd(analysis_vector))

p2 <- plot( x = possible_betas,
            y = likelihood,
            type = 'h', 
            xlim=c(1,3))
