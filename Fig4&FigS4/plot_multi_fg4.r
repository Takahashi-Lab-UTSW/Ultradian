# plot p_DT10m _2y the last before multi plot

path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_C/Fig4/"
setwd(paste(path))

library(cowplot)
library(magick)

p1 <- ggdraw()

p_model <- ggdraw() + draw_image(magick::image_read_pdf("diag_2in1.pdf", density = 3000))+theme(plot.margin = margin(8, 12, 8, 12, "pt"))

pcow1<-plot_grid(p_KCl, p_Ouabain,  
                 labels = c("a", "b"), label_size = 10, 
                 vjust=1, nrow=1) 

pcow2<-plot_grid(p_DT10m4, `p_CGB-TB1`,
                 labels=c("c","d"), label_size = 10,
                 vjust=1, nrow=1) 

pcow3<-plot_grid(p_Nimodipine, p_CBZ,
                 labels=c("e","f"), label_size = 10,
                 vjust=1, nrow=1) 


pcow4<-plot_grid(p_model, p1,
                  labels=c("g",""), label_size = 10, rel_widths = c(3,1),
                  vjust=1, nrow=1)

pcow<-plot_grid(pcow1, pcow2, pcow3, pcow4, ncol = 1, rel_heights = c(1,1,1,1.5))
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)

save_plot("Fig4.pdf", pcow, base_height = 4.5, base_width = 3.5)

