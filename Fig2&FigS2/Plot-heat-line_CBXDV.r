path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_C/Fig2/CBXDV/"

setwd(paste(path))
library(reshape2)
library(ggplot2)
library(cowplot)
library(magick)
library(grid)

plotname<-"p_CBXDV"  #########################################################
dt <- 10   # Input, interval=15min ##########################
offs <- 0  # Input offset in Hours ###########################

df<-read.csv("20181106_2D3S_DT10m_Tracked-T.csv")  ####################################

df<-df[ , colSums(is.na(df)) == 0]  # remove columns containing NA

Hours <- (1:nrow(df)-1)/(60/dt)+offs				# offset hours after Shock

datn<-sweep(df, 2, colMeans(df[1:144,]), FUN="/") #normalize to mean of 1st day
datn<-cbind(Hours,datn)
datn<-datn[datn[,"Hours"]<=84,]  # subset 
datnl<-melt(datn, id.vars="Hours", variable.name="Cell", value.name="TMRM") # make long form data

# plot heatmap 
ph<- ggplot(datnl, aes(Hours, Cell)) +
  geom_tile(aes(fill = TMRM)) +
  scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("UFs") +    #########################################################
  xlab("Hours") +
  theme(
    legend.title = element_text(size = 4),
    legend.text = element_text(size = 4),
    legend.position = "none",
    # legend.box.margin=margin(-10,-10,-10,-20),
    plot.title = element_text(size=4),
    axis.title=element_text(size=6),
    axis.title.x=element_blank(),
    # axis.title.y=element_text(margin = margin(0,-3,0,-3)),
    axis.line = element_blank(),
    axis.text = element_blank(),
    # axis.text.x = element_text(size=4),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.1),
    axis.ticks.length = unit(1, "pt")
  ) +
  scale_x_continuous(expand = c(0,0),breaks = seq(0,96,12))+
  guides(fill= guide_colorbar(barheight = unit(0.5,"npc"), barwidth = unit(0.02,"npc")))+
  labs(fill = "TMRM") + 
  theme(plot.margin = margin(8,12,8,12,"pt"))

# output pdf
ggsave(filename=paste(plotname,"_heat.pdf",sep=""), plot=ph, useDingbats=F, width=1.5, height=1)

# plot line
pl<- ggplot(datnl, aes(x=Hours, y=TMRM, group = Cell, colour = factor(Cell))) +
  geom_line(linetype=1,size=0.2)+
  # scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("TMRM") +    #########################################################
xlab("Hours") +
  theme(legend.position = "none",
        axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 6), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1.5, "pt")
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        # plot.margin = margin(8, 6, 6, 8, "pt")
  )+
  scale_x_continuous(expand = c(0,0),breaks=seq(0,96,12))
  # scale_y_continuous(limits=c(0.7,1.35))
pl<-pl + annotate("rect", xmin=42, xmax=84, ymin=0.5 , ymax=0.6, alpha=0.25, size=0, color="black", fill="black") +
  theme(plot.margin = margin(8,12,8,12,"pt"))

# output pdf
ggsave(filename=paste(plotname,"_line.pdf",sep=""), plot=pl, useDingbats=F, width=1.5, height=1)


# plot heat & line
library(cowplot)
library(magick)

pcow<-plot_grid(ph, pl, nrow = 2, rel_heights = c(2,2), align = 'v')
assign(plotname,pcow)

save_plot(filename=paste(plotname,".pdf",sep=""), pcow, base_height = 2, base_width = 1.5)


