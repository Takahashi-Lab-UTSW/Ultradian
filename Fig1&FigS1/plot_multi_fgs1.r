path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig1/"
setwd(paste(path))

library(cowplot)
p1<-plot_grid(p_tempS+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
              p_OligA+ theme(plot.margin = margin(8, 4, 0, 12, "pt")), 
              labels = c("c","d"), label_size = 10,vjust=1, ncol=1)

p2<-plot_grid(p_tempF+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
              p1,
              labels = c("b",""), label_size = 10,vjust=1, ncol=2)

pcow1<-plot_grid(p_PDKO+ theme(plot.margin = margin(8, 4, 0, 12, "pt")), 
                 p2, 
                 labels = c("a", ""), label_size = 10,vjust=1, ncol=1, rel_heights = c(1,2))
# + theme(plot.margin = margin(2, 2, 2, 2, "pt"))


# pcow2<-plot_grid(p_2DG, p_FCCP, p_LTcrop1, labels=c("d", "e","f"), vjust=1, ncol=1) + theme(plot.margin = margin(0, 0, 0, 0, "pt"))
# 
# pcow3<-plot_grid(p_tempF,p_tempS, labels=c("g", "h"), vjust=1, ncol=1, rel_heights=c(2,1)) + theme(plot.margin = margin(0, 0, 0, 8, "pt"))


# pcow<-plot_grid(pcow1, pcow2, pcow3, ncol=3)
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# ggsave(filename="Fig1.pdf", plot=pcow, useDingbats=F, width=6, height=4)

# save_plot("Fig1s.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)
save_plot("Figs1.pdf", pcow1, base_height = 3, base_width = 3)

