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
csvDEFO <- read.csv(file="raw-data-defo-cadt-road-dev-buffer.csv", header=TRUE, sep=",")

# TRANSFORM DATA FILE --------------------

# Convert csv input file into dataframe
dfDEFO <- csvDEFO
# Transform dataframe from wide-format to long-format
tranDEFO <- melt(dfDEFO, id.vars=c("Agent","Buffer"))
# Rename column names
colnames(tranDEFO) <- c("Agent","BufferDist","HalfHRP","DefoAreaHa")
# Replace variable contents for the factor column (HalfHRP)
levels(tranDEFO$HalfHRP)[levels(tranDEFO$HalfHRP) == "Deforested_in_1st_half_HRP"] <- "2013-2018"
levels(tranDEFO$HalfHRP)[levels(tranDEFO$HalfHRP) == "Deforested_in_2nd_half_HRP"] <- "2018-2022"

# Save dataframe as csv file in transform folder
write.csv(tranDEFO, paste(DirTRAN, "transform-defo-cadt-road-dev-buffer.csv", sep=""), row.names=FALSE)

# GENERATE PLOTS -------------------------

# Plot 1: deforestation within each road development buffer distance range inside ancestral domains
plot1 <- ggplot() + geom_bar(data=tranDEFO, aes(x=factor(BufferDist), y=DefoAreaHa, fill=HalfHRP), stat="identity", position="stack")
plot1 <- plot1 + scale_fill_manual(values=c("#ffc0cb","#ff0000"), name="Years")
plot1 <- plot1 + labs(x="Buffer Distance from Roads (meters)", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot1


