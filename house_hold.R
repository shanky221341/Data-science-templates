howWide = 200
options(width = howWide)
options(warn=-1)

list.of.packages <- c("xlsx","reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.ma.imperial.ac.uk/")
#install.packages("reshape2")
require(xlsx)
require(reshape2)
data <- read.xlsx("data/a35final2013_tcm77-383524.xls", 1)
names(data)[2] <- "x"
data_mod <- subset(data, grepl("^[0-9]", x))
data_mod <- subset(data_mod, select = c(-1, -4,-18))
data_mod <- data_mod[complete.cases(data_mod), ]


regions <-c("E12000001","E12000002","E12000003","E12000004","E12000005","E12000006","E12000007","E12000008","E12000009","E92000001","E92000004","E92000003","E92000002")
names(data_mod)[3:15] <- regions
names(data_mod)[1] <- "category"
names(data_mod)[2] <- "item"
row.names(data_mod) <- 1:nrow(data_mod)


data_mod <- data_mod[, 2:15]
data_mod <- melt(data_mod, id.vars = 1)

from_year <- rep(2011, nrow(data_mod))
from_month <- rep(0, nrow(data_mod))
to_year <- rep(2013, nrow(data_mod))
to_month <- rep(0, nrow(data_mod))
ons_code <- data_mod[, 2]
item <- data_mod[, 1]
expenditure <- as.numeric(as.character(data_mod[, 3]))
data_mod <- data.frame(ons_code, cbind(from_year, from_month, to_year, to_month), item, expenditure)

# write the file to the console in csv format
write.csv(data_mod,"",row.names=FALSE)