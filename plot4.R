#################################### Plot 4 ###################################

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
pmd <- pmd[c("scc", "emissions", "year")] #reduce the data
names(scc) <- tolower(gsub("[_.]", "", names(scc)))

# selecting coal combustion-related sources codes for sectors
# Fuel Comb - Comm/Institutional - Coal  
# Fuel Comb - Electric Generation - Coal 
# Fuel Comb - Industrial Boilers, ICEs - Coal
# in accordance with NEI Technical Support Document (Table 3)
coalcomb <- scc[grep("Coal", scc$eisector),]
coalcombcode <- as.character(coalcomb[,"scc"])
coalpm <- pmd[pmd$scc %in% coalcombcode,]
coalsumpm <- aggregate(emissions~year, data=coalpm, sum)

# constructing the plot 4
png("plot4.png", bg="transparent")

with(coalsumpm, {
        plot(year, emissions, 
             ylab = expression(paste("total emissions of PM"[2.5], " (tons)")),  
             main = "Change of emissions\n from coal combustion-related sources", 
             type = "b", col = "blue", xaxt="n")
        axis(side=1, at=year, labels = year)
})
      
dev.off()