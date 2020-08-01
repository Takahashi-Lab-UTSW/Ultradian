path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig3/"
setwd(paste(path))

library(cowplot)
library(magick)

p1 <- ggdraw() + draw_image(magick::image_read_pdf("Glutaminolyisis_1.5in.pdf", density = 3000))+theme(plot.margin = margin(8, 0, 8, 0, "pt"))


pcow1<-plot_grid(p1, p_COM, p_MIN, labels = c("a", "b","c"), label_size = 10, vjust=1, nrow=1,rel_widths = c(1.5,2,2))+ theme(plot.margin = margin(8, 8, 2, 8, "pt"))

pcow2<-plot_grid(p_NIME, p_GPNA, p_BPTES, p_AOEG,   labels=c("d", "e","f", "g"), label_size = 10,vjust=1, nrow=1) + theme(plot.margin = margin(2, 8, 2, 8, "pt"))

pcow3<-plot_grid( p_DmKG, `p_ATP-KG`, `p_ATPsyn-KG-C3p_Ki`, `p_TMRM-KG`,`p_MS-KG_dot`, `p_MS-Gln_dot`, labels=c("h","i","j","k","l","m"), label_size = 10,vjust=1, nrow=1,rel_widths = c(1, 0.5,0.75,0.75,0.5,0.5)) + 
  theme(plot.margin = margin(2, 8, 2, 8, "pt"))

# pcow4<-plot_grid(p_NKG,  labels=c("j", "k","l","m"), label_size = 10,vjust=1, nrow=1,rel_widths = c(2.5, 1.5,2,1.5)) + theme(plot.margin = margin(2, 8, 8, 8, "pt"))



pcow<-plot_grid(pcow1, pcow2, pcow3, nrow = 3)
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)

save_plot("Fig3.pdf", pcow, base_height = 4, base_width = 7)

