
complete_point_estimate <- 
  lm(outcome_variable ~., data = df_w_mis)$coefficients[2]

n_sub_samples <- 500

n = nrow(df_w_mis); k = 30

length_sample_vec <- choose(n, k)

if (length_sample_vec > 1e15) {
  length_sample_vec = 1e15
} else {
  length_sample_vec = length_sample_vec
}

sample <- 
  sapply(sample.int(length_sample_vec, n_sub_samples, replace = FALSE)-1,
         choose.int, n=n, k=k)

drop_d_subsamples <- map(1:ncol(sample),
                         ~df_w_mis[sample[,.x],])

imputations <- map(drop_d_subsamples, ~.x %>%
                     mutate(., outcome_imputed = fill_NA_N(
                       x = .,
                       model = "pmm", 
                       posit_y = 1, 
                       posit_x = c(2:4), 
                       k = 2
                     )) %>%
                     select(-"outcome_variable") %>%
                     as.matrix())

# subset_imp <- map(1:length(imputations), ~imputations[-.x])

pseudo_estimates <- 
  vapply(imputations, function(x) .lm.fit(x = x[,1:3], y = x[,4])$coefficients[1], numeric(1))

# Trimming the pseudo_estimate_vector 
p95 <- quantile(unlist(pseudo_estimates), 0.95)
p05 <- quantile(unlist(pseudo_estimates), 0.05)
pseudo_estimates_trimmed <- 
  unlist(pseudo_estimates)[which(unlist(pseudo_estimates) <= p95 & unlist(pseudo_estimates) >= p05)]

# Obtaining point estimate from trimmed 
jackknife_point_estimate <- mean.default(pseudo_estimates_trimmed)

corrected_point_estimates <- 
  pseudo_estimates_trimmed - (jackknife_point_estimate - complete_point_estimate)

corrected_jackknife_estimate <- mean(corrected_point_estimates)

sse <- sum((corrected_point_estimates - corrected_jackknife_estimate)^2)

leading_term <- (nrow(df_w_mis)-1)/(nrow(df_w_mis))

var <- sse*leading_term

critical_value <- qnorm(p=.05/2, lower.tail=FALSE)

bound <- critical_value*sqrt(var/nrow(df_w_mis))

LB <- quantile(corrected_point_estimates, 0.025)
UB <- quantile(corrected_point_estimates, 0.975)

print(data.frame("UB" = UB, 
                  "LB" = LB, 
                  "point_estimate" = corrected_jackknife_estimate) %>%
         remove_rownames())
