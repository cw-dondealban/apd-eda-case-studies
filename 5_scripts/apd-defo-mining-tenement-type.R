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

# Plot 1: deforestation within each mining tenement type inside each ancestral domain
plot1 <- ggplot() + geom_bar(data=tranDEFO, aes(x=factor(CADT.Number), y=Defo.Full.HRP, fill="#ff0000"), stat="identity", position="dodge")
plot1 <- plot1 + scale_fill_manual(values=c("#ff0000"))
plot1 <- plot1 + facet_wrap(~Tenement.Type, scales="free_y")
plot1 <- plot1 + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot1 <- plot1 + guides(fill="none")
plot1 <- plot1 + theme(axis.text.x=element_text(size=5))
plot1

# Plot 2: deforestation within each mining tenement type inside each ancestral domain
plot2 <- ggplot() + geom_bar(data=tranDEFO, aes(x=factor(CADT.Number), y=Defo.Full.HRP, fill=Tenement.Type), stat="identity", position="stack")
plot2 <- plot2 + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot2 <- plot2 + scale_fill_discrete(name="Tenement Type")
plot2

# Save output plots in output folder
setwd(DirOUTP)
ggsave(plot1, file="Defo-CADT-Mining-Tenement-Type-1.pdf", width=30, height=15, units="cm", dpi=300)
ggsave(plot2, file="Defo-CADT-Mining-Tenement-Type-2.pdf", width=30, height=15, units="cm", dpi=300)
