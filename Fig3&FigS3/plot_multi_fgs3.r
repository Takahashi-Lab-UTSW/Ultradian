path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig3/"
setwd(paste(path))

library(cowplot)
library(magick)

# p1 <- ggdraw() + draw_image(magick::image_read_pdf("Glutaminolyisis_1.5in.pdf", density = 3000))+theme(plot.margin = margin(8, 0, 8, 0, "pt"))
p1<-ggdraw()

pcow1<-plot_grid(p_NKG, p1, labels = c("a", ""), label_size = 10, vjust=1, nrow=1,rel_widths = c(1.5,1))

pcow2<-plot_grid(`p_DH+N+NaAc` , `p_DH-N+NaAc`, `p_DH+N+DMS`, `p_DH-N+DMS`, `p_DH+N+NAM`, `p_DH-N+NAM`,  
                 labels=c("b","c","d","e","f","g"), label_size = 10,vjust=1, nrow=3)

pcow3<-plot_grid(`p_ATPase-KG`, p1, labels = c("h", ""), label_size = 10, vjust=1, nrow=1,rel_widths = c(1.5,1))


pcow<-plot_grid(pcow1, pcow2, pcow3, nrow = 3, rel_heights = c(1,3,1))
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)

save_plot("Figs3.pdf", pcow, base_height = 5, base_width = 4)

