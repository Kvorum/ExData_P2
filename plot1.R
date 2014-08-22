#################################### Plot 1 ###################################

# downloading the data
if (!file.exists("summarySCC_PM25.rds")&!file.exists("summarySCC_PM25.rds")){
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      "edata_p2.zip")
        unzip("edata_p2.zip")
}

# pre-processing the data
pmd <- readRDS("summarySCC_PM25.rds") # PM2.5 Emissions Data
names(pmd) <- tolower(names(pmd))
pmd <- pmd[c("emissions", "year")] #reduce the data

# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
# Using the base plotting system, make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008.

sumpm <- aggregate(emissions~year, data=pmd, sum)

png("plot1.png", bg="transparent")

par(mar=c(5, 7, 2, 1))
with(sumpm, {
        plot(year, emissions, xlab = "", ylab = "", type = "b", main = "YES!",
             xaxt="n", yaxt="n", col = "blue")
        axis(side=1, at=year, labels = year, cex.axis = 0.9)
        axis(side=2, at=emissions, labels = as.integer(emissions),
             las = 1, cex.axis = 0.9)
        mtext(expression(paste("total emissions of PM"[2.5], " (tons)")), 
              side = 2, line = 5)
        mtext("year", side = 1, line = 2)
})

dev.off()