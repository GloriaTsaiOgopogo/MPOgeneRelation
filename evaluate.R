## By Qingqing Cai 17110700093  

library(plyr)

setwd("your dir")

results <- read.table("resuls.txt", header = F, sep ="\t")

names(results) <- c("MPO", "gene", "distance", "confident")

real_data <- read.table("MGI_MPO_gene_index.txt", header = F, sep ="\t")

head(real_data)

names(real_data) <- c("MPO", "gene")

real_data$tag <- paste(real_data$MPO, real_data$gene, sep ="-")

results <- results[order(results$distance, decreasing = F),]

results <- results[order(results$gene, decreasing = F),]

results$tag <- paste(results$MPO, results$gene, sep = "-")

top_n <- 1  ### 取預測距離最小的top_n個數據

#rm(predicted)

predicted <- ddply(results, 'gene', function(x){if(nrow(x) > top_n) {x <- x[1:top_n,]} else {x <- x}})

dim(predicted)

acu <- merge(real_data, predicted, by = "tag")

acu_score <- paste(round(dim(acu)[1]/length(unique(results$gene)), 5)*100, "%", sep = "")

acu_score
