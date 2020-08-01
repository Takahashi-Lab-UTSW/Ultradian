path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/TCA/"
setwd(paste(path))

library(cowplot)
library(magick)

# p1 <- ggdraw() + draw_image(magick::image_read_pdf("Glutaminolyisis_1in.pdf", density = 3000))+theme(plot.margin = margin(8, 0, 8, 0, "pt"))


# pcow1<-plot_grid(p1, p_COM, p_MIN, labels = c("a", "b","c"), label_size = 10, vjust=1, nrow=1,rel_widths = c(1.5,2,2))+ theme(plot.margin = margin(8, 8, 2, 8, "pt"))
# 
# pcow2<-plot_grid(p_NIME, p_GPNA, `p_MS-Gln_dot`,  labels=c("d", "e","f"), label_size = 10,vjust=1, nrow=1,rel_widths = c(2,2,1)) + theme(plot.margin = margin(2, 8, 2, 8, "pt"))
# 
# pcow3<-plot_grid(p_BPTES, p_AOEG, p_DmKG,  labels=c("g", "h","i"), label_size = 10,vjust=1, nrow=1) + theme(plot.margin = margin(2, 8, 2, 8, "pt"))
# 
# pcow4<-plot_grid(p_NKG, `p_ATP-KG`,`p_TMRM-KG`,`p_MS-KG_dot`, labels=c("j", "k","l","m"), label_size = 10,vjust=1, nrow=1,rel_widths = c(2.5, 1.5,2,1.5)) + theme(plot.margin = margin(2, 8, 8, 8, "pt"))



pcow<-plot_grid(`p_DH+N+NaAc` , `p_DH-N+NaAc`, `p_DH+N+DMS`, `p_DH-N+DMS`, `p_DH+N+NAM`, `p_DH-N+NAM`,  labels=c("a", "b","c","d","e","f"), label_size = 10,vjust=1, nrow=3)
  # + theme(plot.margin = margin(8, 8, 8, 8, "pt"))

# save_plot("Fig1.pdf", pcow, base_height = NULL, base_aspect_ratio = 1.618, base_width = 6)

save_plot("Fig3s_TCA.pdf", pcow, base_height = 4, base_width = 4)

