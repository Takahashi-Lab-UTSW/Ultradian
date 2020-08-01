path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig2/ncfl/"
setwd(paste(path))

library(cowplot)
library(magick)

# p1 <- ggdraw() 
#       # + draw_image(magick::image_read_pdf("Glutaminolyisis_1.5in.pdf", density = 3000))+theme(plot.margin = margin(8, 0, 8, 0, "pt"))
# 
# pcow1<-plot_grid(p_DT10m, p_KCl, p_Ouabain, 
#                  labels = c("a", "b","c"), label_size = 10, 
#                  vjust=1, nrow=1) 
# 
# pcow2<-plot_grid(p_CBZ, p_Nimodipine, p_CaCC,   
#                  labels=c("d", "e","f"), label_size = 10,
#                  vjust=1, nrow=1) 
# 
# pcow3<-plot_grid( `p_CGB-T`, p_BAPTA, p1,
#                   labels=c("g","h",""), label_size = 10,
#                   vjust=1, nrow=1) 

pcowc<-plot_grid(phc, plc, nrow = 2, rel_heights = c(2,2), align = 'v')+ theme(plot.margin = margin(8,12,8,12,"pt"))
assign("p_cfl", pcowc)

save_plot("p_cfl.pdf", pcowc, base_height = 2, base_width = 3)

pcown<-plot_grid(phn, pln, nrow = 2, rel_heights = c(2,2), align = 'v') + theme(plot.margin = margin(8,12,8,12,"pt"))
assign("p_ncfl", pcown)

save_plot("p_ncfl.pdf", pcown, base_height = 2, base_width = 3)
