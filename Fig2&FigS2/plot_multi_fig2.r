path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig2/"
setwd(paste(path))

library(cowplot)
library(magick)

p_image <- ggdraw() + draw_image(magick::image_read_pdf("20170405CGB-LQBT10m01_39_1_BeRST_smth_lbl.pdf", density = 3000))+theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# p_model <- ggdraw() + draw_image(magick::image_read_pdf("diag_2in1.pdf", density = 3000))+theme(plot.margin = margin(8, 12, 8, 12, "pt"))


pcow1<-plot_grid(p_cfl, p_ncfl,
                 labels = c("a", "b"), label_size = 10, rel_heights = c(1,1),
                 vjust=1, ncol=1)

pcow2<-plot_grid(p_image, p_NCFL_TMRM, 
                 labels=c("c", "d"), label_size = 10, rel_heights = c(1,2.5),
                 vjust=1, ncol=1)

pcow3<-plot_grid(p_MixU2OS+theme(plot.margin = margin(8, 12, 8, 12, "pt")), p_CBXDV , p_shCx,
                  labels=c("e","f","g"), label_size = 10,  rel_heights = c(1,2,1),
                  vjust=1, ncol=1)

pcow<-plot_grid(pcow1, pcow2, pcow3, nrow = 1)

save_plot("Fig2.pdf", pcow, base_height = 4, base_width = 7)

