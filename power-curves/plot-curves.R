library(ggplot2)
path.output_data <- Sys.getenv("path.output_data")

###############################################################################
# N is fixed while standardized effect size is varied
###############################################################################
load(file.path(path.output_data, "curve-ind-6M-FIXEDN.RData"))

list.all.FIXEDN$df.collect.eos.means$datagen.params.rho <- as.factor(list.all.FIXEDN$df.collect.eos.means$datagen.params.rho)
gg.eos.means <- ggplot(list.all.FIXEDN$df.collect.eos.means, aes(x=delta, y=power, shape=datagen.params.rho, color=datagen.params.rho))
gg.eos.means <- gg.eos.means + geom_point() + ylim(0,1) + xlim(0,0.45) + labs(title = "End-of-Study Means")
gg.eos.means <- gg.eos.means + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="direct")))
gg.eos.means <- gg.eos.means + scale_shape_discrete(name="rho") + scale_color_discrete(name="rho")

list.all.FIXEDN$df.collect.AUC$datagen.params.rho <- as.factor(list.all.FIXEDN$df.collect.AUC$datagen.params.rho)
gg.AUC <- ggplot(list.all.FIXEDN$df.collect.AUC, aes(x=delta, y=power, shape=datagen.params.rho, color=datagen.params.rho))
gg.AUC <- gg.AUC + geom_point() + ylim(0,1) + xlim(0,0.45) + labs(title = "AUC")
gg.AUC <- gg.AUC + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="direct")))
gg.AUC <- gg.AUC + scale_shape_discrete(name="rho") + scale_color_discrete(name="rho")

list.all.FIXEDN$df.collect.change.score$datagen.params.rho <- as.factor(list.all.FIXEDN$df.collect.change.score$datagen.params.rho)
gg.change.score <- ggplot(list.all.FIXEDN$df.collect.change.score, aes(x=delta, y=power, shape=datagen.params.rho, color=datagen.params.rho))
gg.change.score <- gg.change.score + geom_point() + ylim(0,1) + xlim(0,0.45) + labs(title = "Change Score")
gg.change.score <- gg.change.score + stat_smooth(method = "loess", se=TRUE, fullrange=TRUE, method.args = list(control = loess.control(surface="direct")))
gg.change.score <- gg.change.score + scale_shape_discrete(name="rho") + scale_color_discrete(name="rho")

