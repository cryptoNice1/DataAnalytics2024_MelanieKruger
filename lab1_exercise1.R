
# GETTING DATA IN 
#reading in .xls file
library("readxl")
file <- read_excel("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\GPW3_GRUMP_SummaryInformation_2010.xls")

#reading in .csv file
gpw <- read.csv("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\GPW3_GRUMP_SummaryInformation_2010.csv",header=T)

summary(gpw)
str(gpw)

#plotting some variables
hist(as.numeric(unlist(gpw["Area"])),
     main="Area of Countries",
     breaks=20)

hist(as.numeric(unlist(gpw["PopulationPerUnit"])),
     main="Population per Unit Area",
     breaks=20)

hist(as.numeric(unlist(gpw["Resolution"])),
     main="wtf does resolution mean???",
     breaks=20)

# EXERCISE 1 - EXAMPLE
EPI_data <- read_excel("C:\\Users\\Melanie\\OneDrive - Rensselaer Polytechnic Institute\\Desktop\\School\\24_Spring\\Data Analytics\\2010EPI_data.xls",
                  sheet = "EPI2010_all countries",
                  header=T)
View(EPI_data)
str(EPI_data)
fix(EPI_data)
EPI <- as.numeric(EPI_data$EPI)

#getting rid of nan values
tf <- is.na(EPI)
tf
E <- EPI[!tf]

summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
library(ggplot2)
png("EPI_hist.png")
hist(EPI, main="EPI Histogram")
hist(EPI,seq(30,95,1.0),prob=TRUE)
lines(density(EPI,na.rm=T,bw=1))
rug(EPI)
dev.off()

plot(ecdf(EPI),do.points=F,verticals=T)
qqnorm(EPI); qqline(EPI)

x <- seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab="Q-Q plot for t-distribution")
qqline(EPI)

# EXERCISE 1 - AIR_E variable
air_e <- as.numeric(EPI_data$AIR_E)
summary(air_e)
hist(air_e)

qqnorm(air_e,
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Values",
       ylab = "Sample Values")
qqline(air_e)

theoretical_values <- rnorm(length(air_e))
pval <- ks.test(scale(air_e), theoretical_values)$p.value
pval

plot(ecdf(air_e),do.points = F, verticals=T)


#EXERCISE 1 - AIR_H variable
air_h <- as.numeric(EPI_data$AIR_H)
summary(air_h)
hist(air_h)

qqnorm(air_h,
       main = "Normal Q-Q Plot",
       xlab = "Theoretical Values",
       ylab = "Sample Values")
qqline(air_h)

theoretical_values <- rnorm(length(air_h))
pval <- ks.test(scale(air_h), theoretical_values)$p.value
pval

#try a ecdf
plot(ecdf(air_h),do.points = F, verticals=T)

#try a beta distribution
par(mfrow = c(1, 2))
random_data <- rbeta(length(air_h),2,1)
hist(random_data)
hist(air_h)

par(mfrow = c(1,1))
qqplot(random_data,air_h)
p_val <- ks.test(random_data,air_h,exact=ex)$p.value

## INTERCOMPARING VARIABLES
#getting other variables
subdf <- EPI_data[,c("EPI","ENVHEALTH","ECOSYSTEM","DALY","AIR_H","WATER_H","AIR_E","WATER_E","BIODIVERSITY")]
df <- data.frame(lapply(subdf,as.numeric))
boxplot(df)

names <- colnames(subdf)
n <- length(names)

#comparing combinations
tf <- na.omit(df[,c("EPI","DALY")])
qqplot(tf$EPI,tf$DALY)
names <- colnames(subdf)
n <- length(names)

m <- 6
m_1 <- m-1

par(mfrow = c(m-2,m-2),
    cin = c)

for (i in 1:m_1){
  i_1 <- i+1
  for (j in i_1:m) {
    v1 <- as.numeric(subdf[[names[i]]])
    v2 <- as.numeric(subdf[[names[j]]])
    print(cat(names[i]," ",names[j]," ",ks.test(v1,v2,exact=ex)$p.value))
    qqplot(v1,v2,
           xlab = names[i],
           ylab = names[j])
  }
}

#taking a look at the more interesting combinations from above
par(mfrow = c(1,1))
qqplot(subdf$"WATER_H",subdf$"ECOSYSTEM")

