library("readxl")
file <- read_excel("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\2010EPI_data.xls",
                   sheet = "EPI2010_all countries",
                   col_names=T)
EPI <- as.numeric(EPI_data$EPI)
summary(EPI)
boxplot(EPI)
fivenum(EPI,na.rm=T)
hist(EPI)