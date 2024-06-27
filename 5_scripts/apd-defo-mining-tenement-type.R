# Load Libraries -------------------------
library(reshape2)
library(tidyverse)

# Define Working Directories -------------
DirMAIN <- "/Users/dondealban/Library/CloudStorage/OneDrive-ClearWindPteLtd/GitHub Repositories/apd-eda-case-studies/"
DirSRCE = paste(DirMAIN, "0_source/", sep = "")
DirEXTR = paste(DirMAIN, "1_extract/", sep = "")
DirTRAN = paste(DirMAIN, "2_transform/", sep = "")
DirLOAD = paste(DirMAIN, "3_transform/", sep = "")
DirOUTP = paste(DirMAIN, "4_load/", sep = "")

# Read Source Data Files -----------------
setwd(DirSRCE)
csvDEFO <- read.csv(file="raw-data-defo-cadt-mining-tenement type.csv", header=TRUE, sep=",")

# Transform Data File -------------------

# Convert csv input file into dataframe
dfDEFO <- csvDEFO
# Rename column names
colnames(dfDEFO) <- c("Tenement.Type","CADT.Number","Defo.1st.Half.HRP","Defo.2nd.Half.HRP")
# Add 1st and 2nd half of deforestation historical reference period
dfDEFO$Defo.Full.HRP <- dfDEFO$Defo.1st.Half.HRP + dfDEFO$Defo.2nd.Half.HRP
# Transform dataframe from wide-format to long-format
tranDEFO <- melt(dfDEFO, id.vars=c("Tenement.Type","CADT.Number","Defo.1st.Half.HRP","Defo.2nd.Half.HRP","Defo.Full.HRP"))

# Remove rows in dataframes that satisfy conditions
tranDEFO <- tranDEFO %>% filter(!(CADT.Number==0))
# Replace variable contents
tranDEFO[tranDEFO=="APSA"] <- "AMPSA"
tranDEFO[tranDEFO=="Coal"] <- "COAL"
tranDEFO[tranDEFO=="-"] <- "UNKNOWN"

# Save dataframe as csv file in transform folder
write.csv(tranDEFO, paste(DirTRAN, "tranDEFO.csv", sep=""), row.names=FALSE)



# Generate Plots -------------------------

# Deforestation within each mining tenement type inside each ancestral domain
plotDEFO <- ggplot() + geom_bar(data=dfDEFO, aes(x=factor(CADT.Number), y=Defo.Full.HRP, fill="#c6c3bf"), stat="identity", position=position_dodge())
plotDEFO <- plotDEFO + facet_wrap(~Tenement.Type, scales="free_y")
plotDEFO <- plotDEFO + labs(x="CADT Number", y="Total Area of Deforestation (ha; HRP 2013-2022)")
plotDEFO <- plotDEFO + guides(fill="none")
plotDEFO <- plotDEFO + theme(axis.text.x=element_text(size=5))
plotDEFO

# Deforestation along existing roads across buffer distances


# Save Output Plots ----------------------
ggsave(plotDEFO, file="Defo-CADT-MiningTenement.pdf", width=30, height=15, units="cm", dpi=300)
