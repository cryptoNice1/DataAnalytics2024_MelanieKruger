library("readxl")
EPI_data <- read_excel("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\2010EPI_data.xls",
                       sheet = "EPI2010_all countries")
EPI <- as.numeric(EPI_data$EPI)

#getting EPI data for countries that are not landlocked
EPI_land <- EPI[!EPI_data$"Landlock"]
E_land <- EPI_land[!is.na(EPI_land)]

#getting descriptive statistics
summary(E_land)
fivenum(EPI,na.rm=TRUE)

#generating a histogram to describe EPI
par(mfrow = c(1, 4))
hist(E_land,seq(30,95,1.0),prob=T)
lines(density(E_land,na.rm=T,bw=1))
rug(E_land)

#filtering based on surface water presence
EPI_water <- EPI[EPI_data$"No_surface_water"==0]
EPI_water <- EPI_water[!is.na(EPI_water)]
hist(EPI_water,seq(30,95,1.0),prob=T)
lines(density(EPI_water,na.rm=T,bw=1))
rug(EPI_water)

#filtering based on having a desert
EPI_desert <- EPI[EPI_data$"Desert"==1]
EPI_desert <- EPI_desert[!is.na(EPI_desert)]
hist(EPI_desert,seq(30,95,1.0),prob=T)
lines(density(EPI_desert,na.rm=T,bw=1))
rug(EPI_desert)

#filtering based on high population density
EPI_highDensity <- EPI[EPI_data$"High_Population_Density"==1]
EPI_highDensity <- EPI_highDensity[!is.na(EPI_highDensity)]
hist(EPI_highDensity,seq(30,95,1.0),prob=T)
lines(density(EPI_highDensity,na.rm=T,bw=1))
rug(EPI_highDensity)

#filtering EPI based on EPI region to get continents
EPI_asia <- EPI[grepl("Asia",EPI_data$"EPI_regions") & !grepl("Europe",EPI_data$"EPI_regions")]
EPI_europe <- EPI[grepl("Europe",EPI_data$"EPI_regions")]

#filtering EPI based on geo region
EPI_western_europe <- EPI[grepl("Eastern Europe",EPI_data$"GEO_subregion")]
