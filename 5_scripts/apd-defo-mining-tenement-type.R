# Load Libraries -------------------------
library(reshape2)
library(tidyverse)

# Set Working Directories ----------------
DirMAIN  <- "/Users/dondealban/Library/CloudStorage/OneDrive-ClearWindPteLtd/GitHub Repositories/apd-eda-case-studies/"

# Read Data Files ------------------------
setwd(DirMAIN)
csvDEFO <- read.csv(file="Deforestation_MINING&ROAD_Don.csv", header=TRUE, sep=",")
colnames(csvDEFO) <- c("Tenement.Type","CADT.Number","Defo.1st.Half.HRP","Defo.2nd.Half.HRP","Defo.Full.HRP")

# Wrangle Input Data Files --------------
# Convert dataframe from wide-format to long-format
dfDEFO <- melt(csvDEFO, id.vars=c("Tenement.Type","CADT.Number","Defo.1st.Half.HRP","Defo.2nd.Half.HRP","Defo.Full.HRP"))
# Remove rows in dataframes that satisfy conditions
dfDEFO <- dfDEFO %>% filter(!(CADT.Number==0))
# Replace variable contents
dfDEFO[dfDEFO=="AMPSA"] <- "APSA"
dfDEFO[dfDEFO=="-"] <- "Unknown"

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
