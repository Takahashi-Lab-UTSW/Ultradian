path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig1/"
setwd(paste(path))

library(cowplot)
library(magick)

pwt_clone <- ggdraw() + draw_image(magick::image_read_pdf("p_clone12_wt.pdf", density = 3000))
  # +theme(plot.margin = margin(8, 0, 8, 0, "pt"))
pwt_cdko <- ggdraw() + draw_image(magick::image_read_pdf("p_CDKO_wt.pdf", density = 3000))

# pwc_LTcrop1 <- ggdraw() + draw_image(magick::image_read_pdf("p_LTcrop1_wc.pdf", density = 3000))


pcow1<-plot_grid(p_clone12+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
                 pwt_clone+ theme(plot.margin = margin(0, 0, 0, 12, "pt")),
                 p_CDKO+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
                 pwt_cdko+ theme(plot.margin = margin(0, 0, 0, 12, "pt")),
                 labels = c("a", "b", "c", "d"), label_size = 10, vjust=1, ncol=1)

# pcow2<-plot_grid(p_tempF,p_tempS, labels=c("d", "e"), label_size = 10,vjust=1, ncol=1, rel_heights=c(2,1)) + theme(plot.margin = margin(0, 0, 0, 0, "pt"))

pcow2<-plot_grid(p_CLIH+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
                  p_2DG+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
                 p_FCCP+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
                 p_LTcrop1_2y+ theme(plot.margin = margin(8, 4, 0, 12, "pt")),
                 # pwc_LTcrop1+ theme(plot.margin = margin(0, 0, 0, 12, "pt")),
                 labels=c("e","f","g","h"), label_size = 10,vjust=1, ncol=1)


pcow<-plot_grid(pcow1, pcow2, ncol=2)
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# ggsave(filename="Fig1.pdf", plot=pcow, useDingbats=F, width=6, height=4)

# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)

save_plot("Fig1.pdf", pcow, base_height = 3.5, base_width = 3.5)

