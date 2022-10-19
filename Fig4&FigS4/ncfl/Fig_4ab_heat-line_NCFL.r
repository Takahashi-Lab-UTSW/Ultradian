path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_D/Fig4/ncfl/"

setwd(paste(path))
library(reshape2)
library(ggplot2)
library(cowplot)
library(magick)
library(grid)

# function to detrend a df by subtracting loess line then add back means of loess fit; span adjustable
detrend <- function(x){
  ## response
  vars <- colnames(x)
  ## covariate
  id <- 1:nrow(x)
  # define a loess filter function (fitting loess regression line) for each column of a df
  loess.filter <- function (y, span) loess(formula = as.formula(paste(y, "id", sep = "~")),
                                           data = data.frame(x),
                                           degree = 1,
                                           span = span)$fitted 
  ## apply loess filter column-by-column
  new.x <- as.data.frame(lapply(vars, loess.filter, span = 0.75), col.names = colnames(x))
  bsl<-matrix(rep(colMeans(new.x),each=nrow(new.x)),nrow=nrow(new.x))
  return(x-new.x+bsl)
}


df<-read.csv("LV_20130801-22_ncfl-cfl1.csv")  ####################################

Hours<-df[,1]
dat<-df[,-1]
dat.cfl<-df[,grepl("^cfl",colnames(df))] # select data for confluent cells
dat.ncfl<-df[,grepl("^ncfl",colnames(df))]  # select data for non-confluent cells

# detrend
dat<-detrend(dat) # all
dat.cfl<-detrend(dat.cfl) # confluent cells
dat.ncfl<-detrend(dat.ncfl) # non-confluent cells

datn<-sweep(dat, 2, colMeans(dat), FUN="/") #normalize to mean 
datn<-cbind(Hours,datn)
datnl<-melt(datn, id.vars="Hours", variable.name="Cell", value.name="Lum") # make long form data

datn.cfl<-sweep(dat.cfl, 2, colMeans(dat.cfl), FUN="/") #normalize to mean 
datn.cfl<-cbind(Hours,datn.cfl)
datnl.cfl<-melt(datn.cfl, id.vars="Hours", variable.name="Cell", value.name="Lum")

datn.ncfl<-sweep(dat.ncfl, 2, colMeans(dat.ncfl), FUN="/") #normalize to mean 
datn.ncfl<-cbind(Hours,datn.ncfl)
datnl.ncfl<-melt(datn.ncfl, id.vars="Hours", variable.name="Cell", value.name="Lum")

# plot Confluent cells ========================
phc<- ggplot(datnl.cfl, aes(Hours, Cell)) +
  geom_tile(aes(fill = Lum)) +
  scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Cells") +   
  # xlab("Hours") +
  ggtitle('Confluent')+
  theme(
    plot.title = element_text(hjust = 0.5, size=7, margin=margin(0,0,2,0)),
    # legend.title = element_text(size = 6),
    # legend.text = element_text(size = 4),
    legend.position = "none",
    # legend.box.margin=margin(-10,-10,-10,-20),
    # plot.title = element_text(size=4),
    plot.margin = margin(1, 1, 1, 1, "pt"),
    axis.title=element_text(size=6),
    axis.title.x=element_blank(),
    # axis.title.y=element_text(margin = margin(0,-3,0,-3)),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.1),
    axis.ticks.length = unit(1, "pt")
  ) +
  scale_x_continuous(expand = c(0,0),breaks = seq(0,96,12))+
  guides(fill= guide_colorbar(barheight = unit(0.5,"npc"), barwidth = unit(0.02,"npc")))+
  labs(fill = "Lum")
ggsave(filename="p_heat-CFL.pdf", plot=phc, useDingbats=F, width=1.5, height=.6)

plc<- ggplot(datnl.cfl, aes(x=Hours, y=Lum, group = Cell, colour = factor(Cell))) +
  geom_line(linetype=1,size=0.2)+
  # scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Luminescence") +    
  xlab("Hours") +
  theme(legend.position = "none",
        axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1., "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(1, 1, 1, 1, "pt"))+
  scale_x_continuous(expand = c(0,0), breaks=seq(0,96,12))+
  scale_y_continuous(limits=c(0.7,1.35))
ggsave(filename="p_line_CFL.pdf", plot=plc, useDingbats=F, width=1.5, height=.75)

# plot heat & line
pcow<-plot_grid(phc, plc, nrow = 2, rel_heights = c(1,1.2), align = 'v')
save_plot(filename='p_CFL.pdf', pcow, base_width = 1.5, base_height = 1.2)


# plot non-confluent cells ======================
phn<- ggplot(datnl.ncfl, aes(Hours, Cell)) +
  geom_tile(aes(fill = Lum)) +
  scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Cells") +   
  # xlab("Hours") +
  ggtitle('Non-Confluent')+
  theme(
    plot.title = element_text(hjust = 0.5, size=7, margin=margin(0,0,2,0)),
    # legend.title = element_text(size = 6),
    # legend.text = element_text(size = 4),
    legend.position = "none",
    # legend.box.margin=margin(-10,-10,-10,-20),
    plot.margin = margin(1, 1, 1, 1, "pt"),
    # plot.title = element_text(size=4),
    axis.title=element_text(size=6),
    axis.title.x=element_blank(),
    # axis.title.y=element_text(margin = margin(0,-3,0,-3)),
    axis.line = element_blank(),
    axis.text = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks = element_line(colour = "black", size = 0.1),
    axis.ticks.length = unit(1, "pt")
  ) +
  scale_x_continuous(expand = c(0,0), breaks = seq(0,96,12))+
  guides(fill= guide_colorbar(barheight = unit(0.5,"npc"), barwidth = unit(0.02,"npc")))+
  labs(fill = "Lum")
ggsave(filename="p_heat-NCFL.pdf", plot=phn, useDingbats=F, width=1.5, height=.6)

pln<- ggplot(datnl.ncfl, aes(x=Hours, y=Lum, group = Cell, colour = factor(Cell))) +
  geom_line(linetype=1,size=0.2)+
  # scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Luminescence") +  
  xlab("Hours") +
  theme(legend.position = "none",
        axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 5), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(1, 1, 1, 1, "pt"))+
  scale_x_continuous(expand = c(0,0), breaks=seq(0,96,12)) +
  scale_y_continuous(limits=c(0.7,1.35))
ggsave(filename="p_line-NCFL.pdf", plot=pln, useDingbats=F, width=1.5, height=.75)

# plot heat & line
pcow<-plot_grid(phn, pln, nrow = 2, rel_heights = c(1,1.2), align = 'v')
save_plot(filename='p_NCFL.pdf', pcow, base_width = 1.5, base_height = 1.2)


# # calculate synchrony
# library(synchrony)
# syn.cfl<-community.sync(dat.cfl)
# syn.ncfl<-community.sync(dat.ncfl)



