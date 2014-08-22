#################################### Plot 3 ###################################

library(ggplot2)
library(reshape2)

# downloading the data
if (!file.exists("summarySCC_PM25.rds")&!file.exists("summarySCC_PM25.rds")){
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      "edata_p2.zip")
        unzip("edata_p2.zip")
}

# pre-processing the data
pmd <- readRDS("summarySCC_PM25.rds") # PM2.5 Emissions Data
names(pmd) <- tolower(names(pmd))
pmd <- pmd[c("fips", "emissions", "type", "year")] #reduce the data

# creating a dataset by using library(reshape2) (melt and cast)
baltimorepm <- pmd[pmd$fips == "24510",]
meltdataset <- melt(baltimorepm, id = c("type", "year"), measure.vars = "emissions")
baltimoresumpm <- dcast(meltdataset, type+year ~ variable, sum)
baltimoresumpm <- transform(baltimoresumpm, year = factor(year))

# constructing the plot 3
png("plot3.png", bg="transparent")

qplot(year, emissions, data=baltimoresumpm, color = type, geom = c("point"), 
      main = expression(paste("Total emissions of PM"[2.5], " in Baltimore (by type)")),      
      size = emissions, ylab = "emissions (tons)") + 
        geom_line(size = I(1), aes(group=type)) +
        theme(plot.background = element_blank(), legend.background = element_blank())

dev.off()
