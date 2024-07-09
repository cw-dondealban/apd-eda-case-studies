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
csvDEFO <- read.csv(file="raw-data-lcc-defo-cadt-road-dev-buffer.csv", header=TRUE, sep=",")

# TRANSFORM DATA FILE --------------------

# Convert csv input file into dataframe
dfDEFO <- csvDEFO
# Transform dataframe from wide-format to long-format
tranDEFO <- melt(dfDEFO, id.vars=c("FOR_NFOR","CADT_No","Buffer"))
# Rename column names
colnames(tranDEFO) <- c("HalfHRP","CADT","BufferDist","LandCover","DefoAreaHa")

# Replace variable contents
tranDEFO[tranDEFO=="Deforested in 1st half HRP"] <- "2013-2018"
tranDEFO[tranDEFO=="Deforested in 2nd half HRP"] <- "2018-2022"

# Replace variable contents for the factor column (LandCover)
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Built_up_area"]              <- "Built-up area"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Corn_field"]                 <- "Corn field"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Dipterocarp_forest__closed"] <- "Dipterocarp forest, closed"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Dipterocarp_forest__open"]   <- "Dipterocarp forest, open"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Mangrove_forest"]            <- "Mangrove forest"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Open_barren_ground"]         <- "Open/barren ground"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Other_annual_crop"]          <- "Other annual crop"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Perennial_fruit_crop"]       <- "Perennial fruit crop"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Plantation_forest__closed"]  <- "Plantation forest, closed"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Plantation_forest__open"]    <- "Plantation forest, open"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Rice_paddy"]                 <- "Rice paddy"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Shrub_brush"]                <- "Shrub/brush"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Swamp_forest__closed"]       <- "Swamp forest, closed"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Swamp_forest__open"]         <- "Swamp forest, open"
levels(tranDEFO$LandCover)[levels(tranDEFO$LandCover) == "Water_body"]                 <- "Water body"


# Save dataframe as csv file in transform folder
write.csv(tranDEFO, paste(DirTRAN, "transform-defo-cadt-road-dev-buffer.csv", sep=""), row.names=FALSE)

# GENERATE PLOTS -------------------------

# Plot 1: deforestation within each road development buffer distance range inside ancestral domains
plot1 <- ggplot() + geom_bar(data=tranDEFO, aes(x=factor(BufferDist), y=DefoAreaHa, fill=HalfHRP), stat="identity", position="stack")
plot1 <- plot1 + scale_fill_manual(values=c("#ffc0cb","#ff0000"), name="Years")
plot1 <- plot1 + labs(x="Buffer Distance from Roads (meters)", y="Total Area of Deforestation (ha) during HRP 2013-2022")
plot1

# Save output plots in output folder
setwd(DirOUTP)
ggsave(plot1, file="Defo-CADT-Road-Dev-Buffer-1-30x15.pdf", width=30, height=15, units="cm", dpi=300)
ggsave(plot1, file="Defo-CADT-Road-Dev-Buffer-1-20x15.pdf", width=20, height=15, units="cm", dpi=300)

