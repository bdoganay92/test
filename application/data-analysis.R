library(sas7bdat)
library(dplyr)
library(geeM)

path.code <- Sys.getenv("path.code")
path.raw_data <- Sys.getenv("path.raw_data")
path.input_data <- Sys.getenv("path.input_data")
path.output_data <- Sys.getenv("path.output_data")
source(file.path(path.code,"input-utils.R"))
source(file.path(path.code,"datagen-utils.R"))
source(file.path(path.code,"analysis-utils.R"))
source(file.path(path.code, "geemMod.r"))
environment(geemMod) <- asNamespace('geeM')

input.alpha <- 0.05
input.power <- 0.80
input.tot.time <- 6
input.rand.time <- 2

###############################################################################
# Create dataset for analyses
###############################################################################

raw.data <- read.sas7bdat(file.path(path.raw_data))
df.observed.Yit <- raw.data %>% arrange(id,week) %>%
  # Keep observations measured at weeks 4,8,12,16,20,24 (months 1,2,3,4,5,6)
  filter((week==4)|(week==8)|(week==12)|(week==16)|(week==20)|(week==24)) %>% 
  mutate(time = week/4) %>%
  # R=1 if participant responded to intervention A1 and
  # R=0 if participant did not respond to intervention A1 
  mutate(R = if_else(r2==1,0,1)) %>%
  mutate(a2 = replace(a2, is.nan(a2), NA_real_)) %>%
  mutate(DaysCoc = replace(DaysCoc, is.nan(DaysCoc), NA_real_)) %>%
  select(id, t=time, observed.A1=a1, observed.A2=a2, R, Yit=DaysCoc)

# We will only perform a complete case analysis. 
# We determine which observations to drop.
# Drop all observations from a participant
# if there are missing observations at any one of the response
# variables measured on or after randomization at any month
ids.to.drop <- df.observed.Yit %>% 
  filter(is.na(Yit)) %>%
  select(id) %>% unique(.) %>% as.matrix(.)

ids.to.keep <- setdiff(unique(df.observed.Yit$id),ids.to.drop)
df.observed.Yit <- df.observed.Yit %>% filter(id %in% ids.to.keep)

###############################################################################
# Perform data analyses
###############################################################################
list.longdf.observed <- list(datagen.params=NULL, df.observed.Yit=df.observed.Yit)
list.longdf.wr <- WeightAndReplicate(list.df=list.longdf.observed, tot.time=input.tot.time)
list.df.est.beta <- AnalyzeData(list.df=list.longdf.wr, tot.time=input.tot.time, rand.time=input.rand.time, working.corr="ar1")

est.beta <- list.df.est.beta$estimates$est.beta

# Calculate estimated mean trajectories
list.C <- CreateC(input.tot.time = input.tot.time, input.rand.time = input.rand.time)
C.plusplus <- list.C$C.plusplus
C.plusminus <- list.C$C.plusminus
C.minusplus <- list.C$C.minusplus
C.minusminus <- list.C$C.minusminus

u.plusplus <- exp(C.plusplus %*% est.beta)
u.plusminus <- exp(C.plusminus %*% est.beta)
u.minusplus <- exp(C.minusplus %*% est.beta)
u.minusminus <- exp(C.minusminus %*% est.beta)

# Difference in end-of-study means
L.eos.means <- t(eCol(input.tot.time,input.tot.time))
D.eos.means <- cbind(L.eos.means,-L.eos.means)

# Difference in change score
L.change.score <- -t(eCol(input.rand.time, input.tot.time)) + t(eCol(input.tot.time, input.tot.time))
D.change.score <- cbind(L.change.score, -L.change.score)

# Difference in AUC
for(i in 1:input.tot.time){
  if(input.tot.time==2){
    L.AUC <- (1/2)*t(eCol(1,input.tot.time)) + (1/2)*t(eCol(input.tot.time,input.tot.time))
  }else if(input.tot.time>2 & i==1){
    L.AUC <- (1/2)*t(eCol(1,input.tot.time))
  }else if(input.tot.time>2 & i==input.tot.time){
    L.AUC <- L.AUC+(1/2)*t(eCol(input.tot.time,input.tot.time))
  }else{
    L.AUC <- L.AUC+t(eCol(i,input.tot.time))
  }
}
D.AUC <- cbind(L.AUC,-L.AUC)

# -----------------------------------------------------------------------------
# Calculate quantities of interest
# -----------------------------------------------------------------------------
eos.means.plusplus <- L.eos.means %*% exp(C.plusplus %*% est.beta)
eos.means.plusminus <- L.eos.means %*% exp(C.plusminus %*% est.beta)
eos.means.minusplus <- L.eos.means %*% exp(C.minusplus %*% est.beta)
eos.means.minusminus <- L.eos.means %*% exp(C.minusminus %*% est.beta)

AUC.plusplus <- L.AUC %*% exp(C.plusplus %*% est.beta)
AUC.plusminus <- L.AUC %*% exp(C.plusminus %*% est.beta)
AUC.minusplus <- L.AUC %*% exp(C.minusplus %*% est.beta)
AUC.minusminus <- L.AUC %*% exp(C.minusminus %*% est.beta)

change.score.plusplus <- L.change.score %*% exp(C.plusplus %*% est.beta)
change.score.plusminus <- L.change.score %*% exp(C.plusminus %*% est.beta)
change.score.minusplus <- L.change.score %*% exp(C.minusplus %*% est.beta)
change.score.minusminus <- L.change.score %*% exp(C.minusminus %*% est.beta)

# -----------------------------------------------------------------------------
# Calculate contrasts
# -----------------------------------------------------------------------------
stacked.C.plusplus.plusminus <- rbind(C.plusplus, C.plusminus)
stacked.C.plusplus.minusplus <- rbind(C.plusplus, C.minusplus)
stacked.C.plusplus.minusminus <- rbind(C.plusplus, C.minusminus)
stacked.C.plusminus.minusplus <- rbind(C.plusminus, C.minusplus)
stacked.C.plusminus.minusminus <- rbind(C.plusminus, C.minusminus)
stacked.C.minusminus.minusplus <- rbind(C.minusminus, C.minusplus)

# -----------------------------------------------------------------------------
# Differences in end-of-study means
# -----------------------------------------------------------------------------
diff.eos.means.plusplus.plusminus <- D.eos.means %*% exp(stacked.C.plusplus.plusminus %*% est.beta)
diff.eos.means.plusplus.minusplus <- D.eos.means %*% exp(stacked.C.plusplus.minusplus %*% est.beta)
diff.eos.means.plusplus.minusminus <- D.eos.means %*% exp(stacked.C.plusplus.minusminus %*% est.beta)
diff.eos.means.plusminus.minusplus <- D.eos.means %*% exp(stacked.C.plusminus.minusplus %*% est.beta)
diff.eos.means.plusminus.minusminus <- D.eos.means %*% exp(stacked.C.plusminus.minusminus %*% est.beta)
diff.eos.means.minusminus.minusplus <- D.eos.means %*% exp(stacked.C.minusminus.minusplus %*% est.beta)

# -----------------------------------------------------------------------------
# Differences in AUC
# -----------------------------------------------------------------------------
diff.AUC.plusplus.plusminus <- D.AUC %*% exp(stacked.C.plusplus.plusminus %*% est.beta)
diff.AUC.plusplus.minusplus <- D.AUC %*% exp(stacked.C.plusplus.minusplus %*% est.beta)
diff.AUC.plusplus.minusminus <- D.AUC %*% exp(stacked.C.plusplus.minusminus %*% est.beta)
diff.AUC.plusminus.minusplus <- D.AUC %*% exp(stacked.C.plusminus.minusplus %*% est.beta)
diff.AUC.plusminus.minusminus <- D.AUC %*% exp(stacked.C.plusminus.minusminus %*% est.beta)
diff.AUC.minusminus.minusplus <- D.AUC %*% exp(stacked.C.minusminus.minusplus %*% est.beta)

# -----------------------------------------------------------------------------
# Differences in change score
# -----------------------------------------------------------------------------
diff.change.score.plusplus.plusminus <- D.change.score %*% exp(stacked.C.plusplus.plusminus %*% est.beta)
diff.change.score.plusplus.minusplus <- D.change.score %*% exp(stacked.C.plusplus.minusplus %*% est.beta)
diff.change.score.plusplus.minusminus <- D.change.score %*% exp(stacked.C.plusplus.minusminus %*% est.beta)
diff.change.score.plusminus.minusplus <- D.change.score %*% exp(stacked.C.plusminus.minusplus %*% est.beta)
diff.change.score.plusminus.minusminus <- D.change.score %*% exp(stacked.C.plusminus.minusminus %*% est.beta)
diff.change.score.minusminus.minusplus <- D.change.score %*% exp(stacked.C.minusminus.minusplus %*% est.beta)

# -----------------------------------------------------------------------------
# Get SE's of differences
# -----------------------------------------------------------------------------
est.stderr.diff.eos.means <- EstimateStdErrDiffs(list.df.est.beta, D=D.eos.means, list.C=list.C)
est.stderr.diff.AUC <- EstimateStdErrDiffs(list.df.est.beta, D=D.AUC, list.C=list.C)
est.stderr.diff.change.score <- EstimateStdErrDiffs(list.df.est.beta, D=D.change.score, list.C=list.C)

est.stderr.diff.eos.means <- lapply(est.stderr.diff.eos.means, function(x){return(x$estimates)})
est.stderr.diff.AUC <- lapply(est.stderr.diff.AUC, function(x){return(x$estimates)})
est.stderr.diff.change.score <- lapply(est.stderr.diff.change.score, function(x){return(x$estimates)})

###############################################################################
# Present results in Latex tables
###############################################################################
library(xtable)

# -----------------------------------------------------------------------------
# betas
# -----------------------------------------------------------------------------
df.results.betas <- data.frame(coefficient = list.df.est.beta$estimates$coefnames,
                               est.beta = list.df.est.beta$estimates$est.beta,
                               se.beta = sqrt(diag(list.df.est.beta$estimates$est.cov.beta)))
df.results.betas <- df.results.betas %>% 
  mutate(Z = est.beta/se.beta) %>%
  mutate(p = pnorm(abs(Z), lower.tail = FALSE))
print(xtable(df.results.betas, digits=2), include.rownames=FALSE)

# -----------------------------------------------------------------------------
# mean trajectories
# -----------------------------------------------------------------------------
df.results.diffs <- data.frame(contrast = c("Difference in end-of-study means: (+1, +1) vs. (-1, +1)",
                                               "Difference in AUC: (+1, +1) vs. (-1, +1)",
                                               "Difference in delayed effect: (+1, +1) vs. (-1, +1)"),
                               est.diffs = c(diff.eos.means.plusplus.minusplus,
                                             diff.AUC.plusplus.minusplus,
                                             diff.change.score.plusplus.minusplus),
                               se.diffs = c(as.numeric(est.stderr.diff.eos.means$plusminus.minusplus),
                                            as.numeric(est.stderr.diff.AUC$plusminus.minusplus),
                                            as.numeric(est.stderr.diff.change.score$plusminus.minusplus)))
df.results.diffs <- df.results.diffs %>% 
  mutate(Z = est.diffs/se.diffs) %>%
  mutate(p = pnorm(abs(Z), lower.tail = FALSE))
print(xtable(df.results.diffs, digits=2), include.rownames=FALSE)

###############################################################################
# Plot mean trajectories
###############################################################################
library(ggplot2)
plotdat <- data.frame(DTR = c(rep("Choice-Throughout: (+1,+1)",6), 
                              rep("Initial-Choice: (+1,-1)",6), 
                              rep("Delayed-Choice: (-1,+1)",6), 
                              rep("No-Choice: (-1,-1)",6)),
                      month = rep(1:6, times=4),
                      mu = rbind(u.plusplus, u.plusminus, u.minusplus, u.minusminus))

plotdat$DTR <- as.factor(plotdat$DTR)
gg <- ggplot(plotdat, aes(x=month, y=mu, color=DTR, shape=DTR, linetype=DTR))
gg <- gg + scale_x_continuous(limits = c(1,6), breaks=1:6)
gg <- gg + scale_y_continuous(limits = c(0,4))
gg <- gg + geom_line(size=1.2, show.legend = TRUE)
gg <- gg + geom_point(size=3, alpha=0.7)
gg <- gg + labs(title = "") + xlab("Month") + ylab("Estimated mean number of days in the past month with any cocaine use")
gg <- gg + theme(legend.position="bottom")
ggsave(filename = file.path(path.output_data, "estimatedtrajectories.jpeg"))



