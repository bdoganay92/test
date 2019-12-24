# -----------------------------------------------------------------------------
# Calculate coverage of estimates of pairwise differences in end-of-study means
# between DTRs
# -----------------------------------------------------------------------------
coverage.diff.eos.means.plusplus.plusminus <- left_join(est.diff.eos.means.plusplus.plusminus,
                                                        est.stderr.diff.eos.means.plusplus.plusminus,
                                                        by = c("datagen.params.N", 
                                                               "datagen.params.rho",
                                                               "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusplus.plusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusplus.minusplus <- left_join(est.diff.eos.means.plusplus.minusplus,
                                                        est.stderr.diff.eos.means.plusplus.minusplus,
                                                        by = c("datagen.params.N", 
                                                               "datagen.params.rho",
                                                               "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusplus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusplus.minusminus <- left_join(est.diff.eos.means.plusplus.minusminus,
                                                         est.stderr.diff.eos.means.plusplus.minusminus,
                                                         by = c("datagen.params.N", 
                                                                "datagen.params.rho",
                                                                "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusplus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusminus.minusplus <- left_join(est.diff.eos.means.plusminus.minusplus,
                                                         est.stderr.diff.eos.means.plusminus.minusplus,
                                                         by = c("datagen.params.N", 
                                                                "datagen.params.rho",
                                                                "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.plusminus.minusminus <- left_join(est.diff.eos.means.plusminus.minusminus,
                                                          est.stderr.diff.eos.means.plusminus.minusminus,
                                                          by = c("datagen.params.N", 
                                                                 "datagen.params.rho",
                                                                 "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.plusminus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.eos.means.minusminus.minusplus <- left_join(est.diff.eos.means.minusminus.minusplus,
                                                          est.stderr.diff.eos.means.minusminus.minusplus,
                                                          by = c("datagen.params.N", 
                                                                 "datagen.params.rho",
                                                                 "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.eos.means.minusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

list.coverage.diff.eos.means <- list(coverage.diff.eos.means.plusplus.plusminus=coverage.diff.eos.means.plusplus.plusminus,
                                     coverage.diff.eos.means.plusplus.minusplus=coverage.diff.eos.means.plusplus.minusplus,
                                     coverage.diff.eos.means.plusplus.minusminus=coverage.diff.eos.means.plusplus.minusminus,
                                     coverage.diff.eos.means.plusminus.minusplus=coverage.diff.eos.means.plusminus.minusplus,
                                     coverage.diff.eos.means.plusminus.minusminus=coverage.diff.eos.means.plusminus.minusminus,
                                     coverage.diff.eos.means.minusminus.minusplus=coverage.diff.eos.means.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Calculate coverage of estimates of pairwise differences in AUC between DTRs
# -----------------------------------------------------------------------------
coverage.diff.AUC.plusplus.plusminus <- left_join(est.diff.AUC.plusplus.plusminus,
                                                  est.stderr.diff.AUC.plusplus.plusminus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusplus.plusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusplus.minusplus <- left_join(est.diff.AUC.plusplus.minusplus,
                                                  est.stderr.diff.AUC.plusplus.minusplus,
                                                  by = c("datagen.params.N", 
                                                         "datagen.params.rho",
                                                         "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusplus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusplus.minusminus <- left_join(est.diff.AUC.plusplus.minusminus,
                                                   est.stderr.diff.AUC.plusplus.minusminus,
                                                   by = c("datagen.params.N", 
                                                          "datagen.params.rho",
                                                          "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusplus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusminus.minusplus <- left_join(est.diff.AUC.plusminus.minusplus,
                                                   est.stderr.diff.AUC.plusminus.minusplus,
                                                   by = c("datagen.params.N", 
                                                          "datagen.params.rho",
                                                          "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.plusminus.minusminus <- left_join(est.diff.AUC.plusminus.minusminus,
                                                    est.stderr.diff.AUC.plusminus.minusminus,
                                                    by = c("datagen.params.N", 
                                                           "datagen.params.rho",
                                                           "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.plusminus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.AUC.minusminus.minusplus <- left_join(est.diff.AUC.minusminus.minusplus,
                                                    est.stderr.diff.AUC.minusminus.minusplus,
                                                    by = c("datagen.params.N", 
                                                           "datagen.params.rho",
                                                           "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.AUC.minusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

list.coverage.diff.AUC <- list(coverage.diff.AUC.plusplus.plusminus=coverage.diff.AUC.plusplus.plusminus,
                               coverage.diff.AUC.plusplus.minusplus=coverage.diff.AUC.plusplus.minusplus,
                               coverage.diff.AUC.plusplus.minusminus=coverage.diff.AUC.plusplus.minusminus,
                               coverage.diff.AUC.plusminus.minusplus=coverage.diff.AUC.plusminus.minusplus,
                               coverage.diff.AUC.plusminus.minusminus=coverage.diff.AUC.plusminus.minusminus,
                               coverage.diff.AUC.minusminus.minusplus=coverage.diff.AUC.minusminus.minusplus)

# -------------------------------------------------------------------------------------
# Calculate coverage of estimates of pairwise differences in change score between DTRs
# -------------------------------------------------------------------------------------
coverage.diff.change.score.plusplus.plusminus <- left_join(est.diff.change.score.plusplus.plusminus,
                                                           est.stderr.diff.change.score.plusplus.plusminus,
                                                           by = c("datagen.params.N", 
                                                                  "datagen.params.rho",
                                                                  "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.change.score.plusplus.plusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.change.score.plusplus.minusplus <- left_join(est.diff.change.score.plusplus.minusplus,
                                                           est.stderr.diff.change.score.plusplus.minusplus,
                                                           by = c("datagen.params.N", 
                                                                  "datagen.params.rho",
                                                                  "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.change.score.plusplus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.change.score.plusplus.minusminus <- left_join(est.diff.change.score.plusplus.minusminus,
                                                            est.stderr.diff.change.score.plusplus.minusminus,
                                                            by = c("datagen.params.N", 
                                                                   "datagen.params.rho",
                                                                   "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.change.score.plusplus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.change.score.plusminus.minusplus <- left_join(est.diff.change.score.plusminus.minusplus,
                                                            est.stderr.diff.change.score.plusminus.minusplus,
                                                            by = c("datagen.params.N", 
                                                                   "datagen.params.rho",
                                                                   "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.change.score.plusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.change.score.plusminus.minusminus <- left_join(est.diff.change.score.plusminus.minusminus,
                                                             est.stderr.diff.change.score.plusminus.minusminus,
                                                             by = c("datagen.params.N", 
                                                                    "datagen.params.rho",
                                                                    "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.change.score.plusminus.minusminus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

coverage.diff.change.score.minusminus.minusplus <- left_join(est.diff.change.score.minusminus.minusplus,
                                                             est.stderr.diff.change.score.minusminus.minusplus,
                                                             by = c("datagen.params.N", 
                                                                    "datagen.params.rho",
                                                                    "datagen.params.sim")) %>%
  rename(est.diff=estimates.x, est.stderr=estimates.y) %>%
  mutate(LB = est.diff - qnorm(1-(input.alpha/2))*est.stderr, 
         UB = est.diff + qnorm(1-(input.alpha/2))*est.stderr,
         truth = diff.change.score.minusminus.minusplus) %>%
  mutate(is.cover = (truth>=LB)&(truth<=UB)) %>%
  group_by(datagen.params.N, datagen.params.rho) %>%
  summarise(coverage = mean(is.cover, na.rm=TRUE))

list.coverage.diff.change.score <- list(coverage.diff.change.score.plusplus.plusminus=coverage.diff.change.score.plusplus.plusminus,
                                        coverage.diff.change.score.plusplus.minusplus=coverage.diff.change.score.plusplus.minusplus,
                                        coverage.diff.change.score.plusplus.minusminus=coverage.diff.change.score.plusplus.minusminus,
                                        coverage.diff.change.score.plusminus.minusplus=coverage.diff.change.score.plusminus.minusplus,
                                        coverage.diff.change.score.plusminus.minusminus=coverage.diff.change.score.plusminus.minusminus,
                                        coverage.diff.change.score.minusminus.minusplus=coverage.diff.change.score.minusminus.minusplus)

# -----------------------------------------------------------------------------
# Aggregate results
# -----------------------------------------------------------------------------
for(i in 1:length(list.coverage.diff.eos.means)){
  list.coverage.diff.eos.means[[i]]$pair <- i
}

for(i in 1:length(list.coverage.diff.AUC)){
  list.coverage.diff.AUC[[i]]$pair <- i
}

for(i in 1:length(list.coverage.diff.change.score)){
  list.coverage.diff.change.score[[i]]$pair <- i
}

coverage.diff.eos.means <- bind_rows(list.coverage.diff.eos.means)
coverage.diff.AUC <- bind_rows(list.coverage.diff.AUC)
coverage.diff.change.score <- bind_rows(list.coverage.diff.change.score)

