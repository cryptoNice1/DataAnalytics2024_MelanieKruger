rm(list=ls())
library(readxl)
library(plm)

dir.str <- "C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\DA_Project\\New Datasets"
setwd(dir.str)
main <- read.csv("main_v2.csv")
main$Medium.Household.Gas.Prices..gJ. <- as.numeric(main$Medium.Household.Gas.Prices..gJ.)
main$Country <- factor(main$Country)
main$Year <- factor(main$Year)
main$X <- NULL
str(main)

#changing some of the names
colnames(main)
colnames(main)[23] <- "fossil_dep"
colnames(main)[24] <- "oil_dep"
colnames(main)[25] <- "gas_dep"
colnames(main)[41] <- "elec_price"
colnames(main)[42] <- "gas_price"


#doing some correlation stuf
library(ggplot2)
library(ggcorrplot)

numeric_only <- data.table::copy(main)
numeric_only$Country <- as.numeric(factor(main$Country,levels=unique(main$Country)))
numeric_only$Year <- as.numeric(main$Year)

for (i in colnames(numeric_only)){
  print(paste(i,sd(na.omit(numeric_only[[i]]))))
}

#getting rid of some other columns
numeric_only$Biogas.Inv <- NULL
numeric_only$Coal.and.peat.Cap <- NULL
numeric_only$Coal.and.peat.Inv <- NULL
numeric_only$Concentrated.solar.power.Inv <- NULL
numeric_only$Fossil.fuels.n.e.s..Inv <- NULL
numeric_only$Geothermal.energy.Inv <- NULL
numeric_only$Liquid.biofuels.Inv <- NULL
numeric_only$Marine.energy.Cap <- NULL
numeric_only$Marine.energy.Inv <- NULL
numeric_only$Natural.gas.Cap <- NULL
numeric_only$Natural.gas.Inv <- NULL
numeric_only$Non.renewable.municipal.waste.Inv <- NULL
numeric_only$Off.grid.Solar.photovoltaic.Inv<- NULL
numeric_only$Oil.Inv <- NULL
numeric_only$Pumped.storage.Inv <- NULL
numeric_only$Renewable.municipal.waste.Inv <- NULL
numeric_only$Solar.thermal.energy.Inv <- NULL
numeric_only$Oil.Cap <- NULL
numeric_only <- na.omit(numeric_only)


plot.size <- 1000
png(filename=paste(dir.str,"\\CorrelationMatrix.png",sep=""),
    width = plot.size, height = plot.size)
ggcorrplot(cor(numeric_only[,-c(1,2)]),
           type = "upper",
           show.diag = F,
           lab=T,
           lab_size=2.5)
dev.off()

str(numeric_only[,-c(1,2)])
