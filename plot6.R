#################################### Plot 6 ###################################
library(ggplot2)
library(gridExtra) 

# downloading the data
if (!file.exists("summarySCC_PM25.rds")&!file.exists("summarySCC_PM25.rds")){
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      "edata_p2.zip")
        unzip("edata_p2.zip")
}

# pre-processing the data
pmd <- readRDS("summarySCC_PM25.rds") # PM2.5 Emissions Data
scc <- readRDS("Source_Classification_Code.rds") # Source Classification Code Table

names(pmd) <- tolower(names(pmd))
pmd <- pmd[c("fips", "scc", "emissions", "type", "year")] #reduce the data
names(scc) <- tolower(gsub("[_.]", "", names(scc)))

# Under "motor vehicle sources" let's understand all "Mobile sources" 
# according to NEI Technical Support Document 
mcode <- as.character(scc[grep("Mobile", scc$eisector),"scc"])

# To answer the question 
# "Which city has seen greater changes over time in motor vehicle emissions?" 
# let's calculate the absolute and relative (%) values of emission changes over time
calc <- function(data) {
        absolute <- 0
        percent <- 0
        for (i in 1:length(data$year)-1) {
                a <- data$emissions[i+1]-data$emissions[i]
                absolute <- c(absolute, a)
                p <- (a/data$emissions[i])*100
                percent <- c(percent, p)
        }    
        return(list(absolute=absolute,percent=percent))
}

# for Baltimore City
baltimorepm <- pmd[pmd$fips == "24510",]
baltmobile <- baltimorepm[baltimorepm$scc %in% mcode,]
sumbaltmobile <- aggregate(emissions~year, data=baltmobile, sum)
sumbaltmobile <- cbind(sumbaltmobile, calc(sumbaltmobile), city = "Baltimore City")
# for Los Angeles County
losangelespm <- pmd[pmd$fips == "06037",]
lamobile <- losangelespm[losangelespm$scc %in% mcode,]
sumlamobile <- aggregate(emissions~year, data=lamobile, sum)
sumlamobile <- cbind(sumlamobile, calc(sumlamobile), city = "Los Angeles County")
# dataset to compare
compare <- rbind(sumbaltmobile, sumlamobile)

# constructing the plot 6 - plot 2 graphs using library(gridExtra)
png("plot6.png", width = 960, bg="transparent")

plot6a <- ggplot(compare, aes(x = factor(year), group=city, color=city)) +
        geom_point(aes(y = absolute)) +
        geom_line(aes(y = absolute))+
        labs(x = "year", y = "change of total emissions (tons)") + 
        labs(title = "Changes over time\n in motor vehicle emissions\n (absolute values)")+
        theme(plot.background = element_blank(), legend.background = element_blank())
plot6b <- ggplot(compare, aes(x = factor(year), group=city, color=city)) +
        geom_point(aes(y = percent))+
        geom_line(aes(y = percent))+
        labs(x = "year", y = "change of total emissions (percents)") + 
        labs(title = "Changes over time\n in motor vehicle emissions\n (relative values)")+
        theme(plot.background = element_blank(), legend.background = element_blank())
grid.arrange(plot6a,plot6b, ncol = 2)

dev.off()
