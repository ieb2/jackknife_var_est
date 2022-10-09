n_sub_samples <- 1e3

n = nrow(df_w_mis); k = 40

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
                       k = 10
                     )) %>%
                     select(-"outcome_variable") %>%
                     as.matrix())

pseudo_estimates <- vapply(imputations, function(x) .lm.fit(x = x[,1:3], 
                                                            y = x[,4])$coefficients[1], numeric(1))

jackknife_point_estimate <- mean.default(pseudo_estimates)

LB <- quantile(pseudo_estimates, 0.025)
UB <- quantile(pseudo_estimates, 0.975)

print(data.frame("UB" = UB, 
                  "LB" = LB, 
                  "point_estimate" = jackknife_point_estimate) %>%
         remove_rownames())
