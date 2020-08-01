path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig4/"
setwd(paste(path))

library(cowplot)
library(magick)

# p1 <- ggdraw() 
      # + draw_image(magick::image_read_pdf("Glutaminolyisis_1.5in.pdf", density = 3000))+theme(plot.margin = margin(8, 0, 8, 0, "pt"))

pcow1<-plot_grid(`p_MTB-4`,
                 labels = c("a"), label_size = 10, 
                 vjust=1, nrow=1) 

pcow2<-plot_grid(p_APG, p_MQAE,   
                 labels=c("b", "c"), label_size = 10,
                 vjust=1, nrow=1) 

pcow3<-plot_grid(p_TTA,  p_NNC, 
                  labels=c("d","e"), label_size = 10,
                  vjust=1, nrow=1) 

pcow4<-plot_grid( p_BAPTA, `p_BAPTA-AM`,
                 labels=c("f","g"), label_size = 10,
                 vjust=1, nrow=1) 

pcow<-plot_grid(pcow1, pcow2, pcow3, pcow4, ncol = 1)
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)

save_plot("FigS4.pdf", pcow, base_height = 6, base_width = 4)

