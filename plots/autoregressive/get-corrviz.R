tau <- round(curr.list$tau.max, 2)
tau2 <- round(curr.list$tau.max^2, 2)
tau3 <- round(curr.list$tau.max^3, 2)
tau4 <- round(curr.list$tau.max^4, 2)
tau5 <- round(curr.list$tau.max^5, 2)

use.colors <- c("forestgreen", "darkgoldenrod1", "lightskyblue1")
use.title <- paste("tau = ", tau, ", tau^2 = ", tau2, ", tau^3 = ", tau3, ", tau^4 = ", tau4, ", tau^5 = ", tau5, sep="")

adjust_plot <- theme(panel.border = element_blank(), 
                     panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     text = element_text(size=14),
                     axis.text.x = element_text(face = "bold", size = 18),
                     axis.text.y = element_text(face = "bold", size = 18),
                     axis.ticks = element_blank())

jpeg(file.path(path.plots, this.folder, this.subfolder, paste("correlation_","scenario_", i, "_", curr.nam, "_plus.r", ".jpg",sep="")), 
     width = 650, height = 650)

colnames(curr.list[["cormat.plus.r"]]) <- c("Y1(.)","Y2(+1,1,0)","Y3(+1,1,0)","Y4(+1,1,0)","Y5(+1,1,0)","Y6(+1,1,0)")
row.names(curr.list[["cormat.plus.r"]]) <- colnames(curr.list[["cormat.plus.r"]])
p <- ggcorrplot(curr.list[["cormat.plus.r"]], 
                lab = TRUE, lab_size = 8,
                title = use.title,
                colors = use.colors, 
                ggtheme = ggplot2::theme_gray(),
                outline.color = "white",
                show.legend = FALSE) + adjust_plot

print(p)
dev.off()


jpeg(file.path(path.plots, this.folder, this.subfolder, paste("correlation_","scenario_", i, "_", curr.nam, "_plus.nr.plus", ".jpg",sep="")), 
     width = 650, height = 650)

colnames(curr.list[["cormat.plus.nr.plus"]]) <- c("Y1(.)","Y2(+1,0,+1)","Y3(+1,0,+1)","Y4(+1,0,+1)","Y5(+1,0,+1)","Y6(+1,0,+1)")
row.names(curr.list[["cormat.plus.nr.plus"]]) <- colnames(curr.list[["cormat.plus.nr.plus"]])
p <- ggcorrplot(curr.list[["cormat.plus.nr.plus"]], 
                lab = TRUE, lab_size = 8,
                title = use.title,
                colors = use.colors, 
                ggtheme = ggplot2::theme_gray(),
                outline.color = "white",
                show.legend = FALSE) + adjust_plot

print(p)
dev.off()


jpeg(file.path(path.plots, this.folder, this.subfolder, paste("correlation_","scenario_", i, "_", curr.nam, "_plus.nr.minus", ".jpg",sep="")), 
     width = 650, height = 650)

colnames(curr.list[["cormat.plus.nr.minus"]]) <- c("Y1(.)","Y2(+1,0,-1)","Y3(+1,0,-1)","Y4(+1,0,-1)","Y5(+1,0,-1)","Y6(+1,0,-1)")
row.names(curr.list[["cormat.plus.nr.minus"]]) <- colnames(curr.list[["cormat.plus.nr.plus"]])
p <- ggcorrplot(curr.list[["cormat.plus.nr.minus"]], 
                lab = TRUE, lab_size = 8,
                title = use.title,
                colors = use.colors, 
                ggtheme = ggplot2::theme_gray(),
                outline.color = "white",
                show.legend = FALSE) + adjust_plot

print(p)
dev.off()


jpeg(file.path(path.plots, this.folder, this.subfolder, paste("correlation_","scenario_", i, "_", curr.nam, "_minus.r", ".jpg",sep="")), 
     width = 650, height = 650)

colnames(curr.list[["cormat.minus.r"]]) <- c("Y1(.)","Y2(-1,1,0)","Y3(-1,1,0)","Y4(-1,1,0)","Y5(-1,1,0)","Y6(-1,1,0)")
row.names(curr.list[["cormat.minus.r"]]) <- colnames(curr.list[["cormat.minus.r"]])
p <- ggcorrplot(curr.list[["cormat.minus.r"]], 
                lab = TRUE, lab_size = 8,
                title = use.title,
                colors = use.colors, 
                ggtheme = ggplot2::theme_gray(),
                outline.color = "white",
                show.legend = FALSE) + adjust_plot

print(p)
dev.off()


jpeg(file.path(path.plots, this.folder, this.subfolder, paste("correlation_","scenario_", i, "_", curr.nam, "_minus.nr.plus", ".jpg",sep="")), 
     width = 650, height = 650)

colnames(curr.list[["cormat.minus.nr.plus"]]) <- c("Y1(.)","Y2(-1,0,+1)","Y3(-1,0,+1)","Y4(-1,0,+1)","Y5(-1,0,+1)","Y6(-1,0,+1)")
row.names(curr.list[["cormat.minus.nr.plus"]]) <- colnames(curr.list[["cormat.minus.nr.plus"]])
p <- ggcorrplot(curr.list[["cormat.minus.nr.plus"]], 
                lab = TRUE, lab_size = 8,
                title = use.title,
                colors = use.colors, 
                ggtheme = ggplot2::theme_gray(),
                outline.color = "white",
                show.legend = FALSE) + adjust_plot

print(p)
dev.off()


jpeg(file.path(path.plots, this.folder, this.subfolder, paste("correlation_","scenario_", i, "_", curr.nam, "_minus.nr.minus", ".jpg",sep="")), 
     width = 650, height = 650)

colnames(curr.list[["cormat.minus.nr.minus"]]) <- c("Y1(.)","Y2(-1,0,-1)","Y3(-1,0,-1)","Y4(-1,0,-1)","Y5(-1,0,-1)","Y6(-1,0,-1)")
row.names(curr.list[["cormat.minus.nr.minus"]]) <- colnames(curr.list[["cormat.minus.nr.minus"]])
p <- ggcorrplot(curr.list[["cormat.minus.nr.minus"]], 
                lab = TRUE, lab_size = 8,
                title = use.title,
                colors = use.colors, 
                ggtheme = ggplot2::theme_gray(),
                outline.color = "white",
                show.legend = FALSE) + adjust_plot

print(p)
dev.off()

