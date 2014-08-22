#################################### Plot 5 ###################################

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
pmd <- pmd[c("fips", "scc", "emissions", "year")] #reduce the data
names(scc) <- tolower(gsub("[_.]", "", names(scc)))

baltimorepm <- pmd[pmd$fips == "24510",]

# Under "motor vehicle sources" let's understand all "Mobile sources" 
# according to NEI Technical Support Document 
mcode <- as.character(scc[grep("Mobile", scc$eisector),"scc"])

# To answer the question "How have changed emissions of sources" 
# let's choose only sources that are measuring more than one year 
baltmcode <- character(0)
for (i in 1:length(mcode)) {
        a <- unique(baltimorepm[baltimorepm$scc == mcode[i],"year"])
        if (length(a) > 1) baltmcode <- c(baltmcode, mcode[i])
}

baltmobile <- baltimorepm[baltimorepm$scc %in% baltmcode,]
sumbaltmobile <- aggregate(emissions~year, data=baltmobile, sum)

# constructing the plot 5
png("plot5.png", bg="transparent")

with(sumbaltmobile, {
        plot(year, emissions, 
             ylab = expression(paste("total emissions of PM"[2.5], " (tons)")),  
             main = "Change of emissions\n from motor vehicle sources\n in Baltimore", 
             type = "b", col = "blue", xaxt="n")
        axis(side=1, at=year, labels = year)
})

dev.off()