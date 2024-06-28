# LOAD LIBRARIES -------------------------
library(reshape2)
library(tidyverse)

# DEFINE WORKING DIRECTORIES -------------
DirMAIN <- "/Users/dondealban/Library/CloudStorage/OneDrive-ClearWindPteLtd/GitHub Repositories/apd-eda-case-studies/"
DirSRCE = paste(DirMAIN, "0_source/", sep = "")
DirEXTR = paste(DirMAIN, "1_extract/", sep = "")
DirTRAN = paste(DirMAIN, "2_transform/", sep = "")
DirLOAD = paste(DirMAIN, "3_load/", sep = "")
DirOUTP = paste(DirMAIN, "4_output/", sep = "")

# READ SOURCE DATA FILES -----------------
setwd(DirSRCE)
csvDEFO <- read.csv(file="raw-data-defo-cadt-mining-tenement type.csv", header=TRUE, sep=",")

# TRANSFORM DATA FILE --------------------

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
write.csv(tranDEFO, paste(DirTRAN, "transform-defo-cadt-mining-tenement type.csv", sep=""), row.names=FALSE)

# GENERATE PLOTS -------------------------

# Deforestation within each mining tenement type inside each ancestral domain
plotDEFO <- ggplot() + geom_bar(data=tranDEFO, aes(x=factor(CADT.Number), y=Defo.Full.HRP, fill="#000000"), stat="identity", position=position_dodge())
plotDEFO <- plotDEFO + scales_fill_manual()

plotDEFO <- plotDEFO + facet_wrap(~Tenement.Type, scales="free_y")
plotDEFO <- plotDEFO + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plotDEFO <- plotDEFO + guides(fill="none")
plotDEFO <- plotDEFO + theme(axis.text.x=element_text(size=5))
plotDEFO

# Save output plots in output folder
setwd(DirOUTP)
ggsave(plotDEFO, file="Defo-CADT-Mining-Tenement-Type.pdf", width=30, height=15, units="cm", dpi=300)
