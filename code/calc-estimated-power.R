# Note that 
#   - input.alpha
# need to be specified prior to running the code below

use.this.est.diff.eos.means <- est.diff.eos.means
use.this.est.diff.AUC <- est.diff.AUC
use.this.est.stderr.diff.eos.means <- est.stderr.diff.eos.means
use.this.est.stderr.diff.AUC <- est.stderr.diff.AUC

# Preliminary data preparation steps
use.this.est.diff.eos.means <- use.this.est.diff.eos.means %>%
  rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
  select(sim, est.diff)

use.this.est.diff.AUC <- use.this.est.diff.AUC %>%
  rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.diff = estimates) %>%
  select(sim, est.diff)

use.this.est.stderr.diff.eos.means <- use.this.est.stderr.diff.eos.means %>%
  rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
  select(sim, est.stderr.diff)

use.this.est.stderr.diff.AUC <- use.this.est.stderr.diff.AUC %>%
  rename(N = datagen.params.N, rho = datagen.params.rho, sim = datagen.params.sim, est.stderr.diff = estimates) %>%
  select(sim, est.stderr.diff)

# Merge data frames
use.this.eos.means <- inner_join(x = use.this.est.diff.eos.means, y = use.this.est.stderr.diff.eos.means, by = "sim")
use.this.AUC <- inner_join(x = use.this.est.diff.AUC, y = use.this.est.stderr.diff.AUC, by = "sim")

# Calculate power
use.this.eos.means <- use.this.eos.means %>% 
  mutate(z = est.diff/est.stderr.diff) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))

use.this.AUC <- use.this.AUC %>% 
  mutate(z = est.diff/est.stderr.diff) %>%
  mutate(is.reject = abs(z)>qnorm(1-(input.alpha/2)))

power.diff.eos.means <- mean(use.this.eos.means$is.reject, na.rm=TRUE)
power.diff.AUC <- mean(use.this.AUC$is.reject, na.rm=TRUE)

