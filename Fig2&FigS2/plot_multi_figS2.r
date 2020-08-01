path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_C/Fig2/"
setwd(paste(path))

library(cowplot)
library(magick)

# p_image <- ggdraw() + draw_image(magick::image_read_pdf("20170405CGB-LQBT10m01_39_1_BeRST_smth_lbl.pdf", density = 3000))+theme(plot.margin = margin(8, 8, 8, 8, "pt"))
# 
# p_model <- ggdraw() + draw_image(magick::image_read_pdf("diag_2in.pdf", density = 3000))+theme(plot.margin = margin(8, 12, 8, 12, "pt"))
# 
# 
pcow1<-plot_grid(p_shCx, p_shCxQPCR, 
                 labels = c("f", "g"), label_size = 10, rel_heights = c(1,1),
                 vjust=1, ncol=1)
# 
# pcow2<-plot_grid(p_image, p_NCFL_TMRM, p_MixU2OS+theme(plot.margin = margin(8, 12, 8, 12, "pt")),
#                  labels=c("d", "e","f"), label_size = 10, rel_heights = c(1,2,1),
#                  vjust=1, ncol=1)
# 
# pcow3<-plot_grid( p_CBXDV , p_shCx, p_model,
#                   labels=c("g","h","i"), label_size = 10,  rel_heights = c(1.5,1,1.5),
#                   vjust=1, ncol=1)

pcow<-plot_grid(p_MixU2OS, p_Mix3T3, p_CBX, p_GJMEFL, p_CBXDV, pcow1, ncol = 2, labels=c("a","b","c","d","e",""), label_size = 10, rel_heights = c(1,1,2))

save_plot("FigS2.pdf", pcow, base_height = 4, base_width = 4)

