path<-"C:/Users/SHU/Documents/Doc/UltradianCycle/UC-Plots/All_Figs/Figs_B/Fig2/ncfl/"

setwd(paste(path))
library(reshape2)
library(ggplot2)
library(cowplot)
library(magick)

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

# plotname<-"p_line-NCFL"  #########################################################

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
plc<- ggplot(datnl.cfl, aes(x=Hours, y=Lum, group = Cell, colour = factor(Cell))) +
  geom_line(linetype=1,size=0.2)+
  # scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Luminescence") +    #########################################################
  xlab("Hours") +
  theme(legend.position = "none",
        axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 6), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1.5, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(8, 6, 6, 8, "pt"))+
  scale_x_continuous(breaks=seq(0,96,12))+
  scale_y_continuous(limits=c(0.7,1.35))

ggsave(filename="p_line_CFL.pdf", plot=plc, useDingbats=F, width=2, height=1)


# plot heatmap for non-confluent cells
pln<- ggplot(datnl.ncfl, aes(x=Hours, y=Lum, group = Cell, colour = factor(Cell))) +
  geom_line(linetype=1,size=0.2)+
  # scale_fill_gradient(low = "blue", high = "red") +
  # theme_minimal()+
  theme_classic() +
  ylab("Luminescence") +    #########################################################
  xlab("Hours") +
  theme(legend.position = "none",
        axis.title = element_text(face = "plain", color = "black", size = 6), 
        # axis.title.x = element_text(face = "plain", color = "black", size = 8), 
        axis.text = element_text(face = "plain", color = "black", size = 6), 
        # axis.text.x = element_text(face = "plain", color = "black", size = 8), 
        # axis.line = element_blank(),
        axis.line = element_line(colour = "black", size = 0.1),
        axis.ticks = element_line(colour = "black", size = 0.1),
        axis.ticks.length = unit(1.5, "pt"),
        # panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        plot.margin = margin(8, 6, 6, 8, "pt"))+
  scale_x_continuous(breaks=seq(0,96,12)) +
  scale_y_continuous(limits=c(0.7,1.35))

ggsave(filename="p_line-NCFL.pdf", plot=pln, useDingbats=F, width=2, height=1)



# # calculate synchrony
# library(synchrony)
# syn.cfl<-community.sync(dat.cfl)
# syn.ncfl<-community.sync(dat.ncfl)


