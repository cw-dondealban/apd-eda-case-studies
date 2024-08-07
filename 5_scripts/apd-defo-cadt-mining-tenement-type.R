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

# Transform #1
tranDEFO1 <- dfDEFO
# Transform dataframe from wide-format to long-format
tranDEFO1 <- melt(tranDEFO1, id.vars=c("TENEMENT_T","CADT_No"))
# Rename column names
colnames(tranDEFO1) <- c("MiningType","CADT","HalfHRP","DefoAreaHa")
# Remove rows in dataframes that satisfy conditions
tranDEFO1 <- tranDEFO1 %>% filter(!(CADT==0))
# Replace variable contents
tranDEFO1[tranDEFO1=="APSA"] <- "AMPSA"
tranDEFO1[tranDEFO1=="Coal"] <- "COAL"
tranDEFO1[tranDEFO1=="-"] <- "UNKNOWN"
# Replace variable contents for the factor column (HalfHRP)
levels(tranDEFO1$HalfHRP)[levels(tranDEFO1$HalfHRP) == "Deforested_in_1st_half_HRP"] <- "2013-2018"
levels(tranDEFO1$HalfHRP)[levels(tranDEFO1$HalfHRP) == "Deforested_in_2nd_half_HRP"] <- "2018-2022"

# Save dataframe as csv file in transform folder
write.csv(tranDEFO1, paste(DirTRAN, "transform-defo-cadt-per-hrp.csv", sep=""), row.names=FALSE)

# GENERATE PLOTS -------------------------

# Plot 1: deforestation per CADT faceted by mining tenement type
plot1 <- ggplot() + geom_bar(data=tranDEFO1, aes(x=factor(CADT), y=DefoAreaHa, fill=HalfHRP), stat="identity", position="stack")
plot1 <- plot1 + facet_wrap(~MiningType, scales="free_y")
plot1 <- plot1 + scale_fill_manual(values=c("#ffc0cb","#ff0000"), name="Years")
plot1 <- plot1 + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot1 <- plot1 + theme(axis.text.x=element_text(size=5))
plot1

# Plot 2: total deforestation per CADT categorised by mining tenement type
plot2 <- ggplot() + geom_bar(data=tranDEFO1, aes(x=factor(CADT), y=DefoAreaHa, fill=MiningType), stat="identity", position="stack")
plot2 <- plot2 + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot2 <- plot2 + scale_fill_discrete(name="Tenement Type")
plot2

# Plot 3: deforestation per CADT categorised by HRP
plot3 <- ggplot() + geom_bar(data=tranDEFO1, aes(x=factor(CADT), y=DefoAreaHa, fill=HalfHRP), stat="identity", position="stack")
plot3 <- plot3 + scale_fill_manual(values=c("#ffc0cb","#ff0000"), name="Years")
plot3 <- plot3 + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot3

# Save output plots in output folder
setwd(DirOUTP)
ggsave(plot1, file="Defo-CADT-Mining-Tenement-Type-1.pdf", width=30, height=15, units="cm", dpi=300)
ggsave(plot2, file="Defo-CADT-Mining-Tenement-Type-2.pdf", width=30, height=15, units="cm", dpi=300)
ggsave(plot3, file="Defo-CADT-Mining-Tenement-Type-3.pdf", width=30, height=15, units="cm", dpi=300)
