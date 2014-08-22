#################################### Plot 2 ###################################

# downloading the data
if (!file.exists("summarySCC_PM25.rds")&!file.exists("summarySCC_PM25.rds")){
        download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      "edata_p2.zip")
        unzip("edata_p2.zip")
}

# pre-processing the data
pmd <- readRDS("summarySCC_PM25.rds") # PM2.5 Emissions Data
names(pmd) <- tolower(names(pmd))
pmd <- pmd[c("fips", "emissions", "year")] #reduce the data

# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.
baltimoresumpm <- aggregate(emissions~year, data=pmd[pmd$fips == "24510",], sum)

png("plot2.png", bg="transparent")

par(mar=c(5, 7, 2, 1))
with(baltimoresumpm, {
        plot(year, emissions, xlab = "", ylab = "", type = "b", main = "YES!",
             xaxt="n", yaxt="n", col = "blue")
        axis(side=1, at=year, labels = year, cex.axis = 0.9)
        axis(side=2, at=emissions, labels = as.integer(emissions), las = 1,
             cex.axis = 0.9)
        mtext(expression(paste("total emissions of PM"[2.5], " (tons) in Baltimore")),
              side = 2, line = 4)
        mtext("year", side = 1, line = 2)
})

dev.off()