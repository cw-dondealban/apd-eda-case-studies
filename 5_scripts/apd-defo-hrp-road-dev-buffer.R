# LOAD LIBRARIES -------------------------
library(reshape2)
library(tidyverse)

# CLEAR ALL OBJECTS FROM GLOBAL ENVIRONMENT
rm(list = ls())

# DEFINE WORKING DIRECTORIES -------------
DirMAIN <- "/Users/dondealban/Library/CloudStorage/OneDrive-ClearWindPteLtd/GitHub Repositories/apd-eda-case-studies/"
DirSRCE = paste(DirMAIN, "0_source/", sep = "")
DirEXTR = paste(DirMAIN, "1_extract/", sep = "")
DirTRAN = paste(DirMAIN, "2_transform/", sep = "")
DirLOAD = paste(DirMAIN, "3_load/", sep = "")
DirOUTP = paste(DirMAIN, "4_output/", sep = "")

# READ SOURCE DATA FILES -----------------
setwd(DirSRCE)
csvDEFO <- read.csv(file="raw-data-defo-hrp-cadt-road-dev-buffer.csv", header=TRUE, sep=",")

# TRANSFORM DATA FILE --------------------

# Convert csv input file into dataframe
dfDEFO <- csvDEFO

# Transform #1
tranDEFO1 <- dfDEFO

# Transform dataframe from wide-format to long-format
tranDEFO1 <- melt(tranDEFO1, id.vars=c("CADT_No"))
# Rename column names
colnames(tranDEFO1) <- c("CADT","HalfHRP","DefoAreaHa")
# Replace variable contents for the factor column (HalfHRP)
levels(tranDEFO1$HalfHRP)[levels(tranDEFO1$HalfHRP) == "Deforested_in_1st_half_HRP"] <- "2013-2018"
levels(tranDEFO1$HalfHRP)[levels(tranDEFO1$HalfHRP) == "Deforested_in_2nd_half_HRP"] <- "2018-2022"

# Save dataframe as csv file in transform folder
write.csv(tranDEFO1, paste(DirTRAN, "transform-defo-hrp-cadt-road-dev-buffer.csv", sep=""), row.names=FALSE)

# GENERATE PLOTS -------------------------

# Plot 3: deforestation per CADT categorised by HRP
plot3 <- ggplot() + geom_bar(data=tranDEFO1, aes(x=factor(CADT), y=DefoAreaHa, fill=HalfHRP), stat="identity", position="stack")
plot3 <- plot3 + scale_fill_manual(values=c("#ffc0cb","#ff0000"), name="Years")
plot3 <- plot3 + labs(x="CADT Number", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot3

# Save output plots in output folder
setwd(DirOUTP)
ggsave(plot3, file="DefoHRP-CADT-Road-Dev-Buffer-1-30x15.pdf", width=30, height=15, units="cm", dpi=300)
