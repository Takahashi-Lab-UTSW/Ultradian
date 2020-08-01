path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig2/ncfl/"

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

# plotname<-"p_HEAT-CFL"  #########################################################

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

# plot heatmap for Confluent cells
phc<- ggplot(datnl.cfl, aes(Hours, Cell)) +
  geom_tile(aes(fill = Lum)) +
  scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Confluent UFs") +    #########################################################
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
  labs(fill = "Lum")
ggsave(filename="p_heat-CFL.pdf", plot=phc, useDingbats=F, width=2, height=1)



# plot heatmap for non-confluent cells
phn<- ggplot(datnl.ncfl, aes(Hours, Cell)) +
  geom_tile(aes(fill = Lum)) +
  scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Non-confluent UFs") +    #########################################################
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
  scale_x_continuous(expand = c(0,0), breaks = seq(0,96,12))+
  guides(fill= guide_colorbar(barheight = unit(0.5,"npc"), barwidth = unit(0.02,"npc")))+
  labs(fill = "Lum")
ggsave(filename="p_heat-NCFL.pdf", plot=phn, useDingbats=F, width=2, height=1)

# p<-rbind(ggplotGrob(pn+theme(legend.position="none")), ggplotGrob(pc+theme(legend.position="none")), size = "last")
# 
# legend <- get_legend(pc)
# pcow<-plot_grid(p, NULL, nrow = 1, rel_widths = c(0.9,0.1))
# 
# pcow<-pcow + draw_grob(legend, 0.9, 0.2, 0.1,0.1)
# 
# save_plot("Fig_Heat-cfl.pdf", pcow, base_height = 2, base_width = 4)

# assign(plotname,p)
# ggsave(filename=paste(plotname,"pdf",sep="."), plot=get(plotname), useDingbats=F, width=2, height=1)



# # calculate synchrony
# library(synchrony)
# syn.cfl<-community.sync(dat.cfl)
# syn.ncfl<-community.sync(dat.ncfl)



