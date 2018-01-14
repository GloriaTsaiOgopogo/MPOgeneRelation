## For data arranging and data combination of string, go, MPO dericed gene-gene distance data #### 

setwd("your directory")

##################### gene dist based on MPO #################################################

MPO_gene <- read.table("MGI_MPO_gene_index.txt", header = F, sep ="\t")

uni_mpo <- unique(MPO_gene[, 1])

uni_gene <- unique(MPO_gene[,2])

MPO_gene[,1] <- as.factor(MPO_gene[,1])

MPO_gene[,2] <- as.factor(MPO_gene[,2])

MPO_matrix <- table(MPO_gene)

## row.names(MPO_matrix)  ## MPO

## colnames(MPO_matrix)  ## gene

length(uni_gene)

length(uni_mpo)

dim(table(MPO_gene))
## [1] 8289 9167

write.table(t(table(MPO_gene)), "MPO_dist_table.txt", row.names = T, col.names = F, sep = "\t")

## distance prase using java code 

gene_dist_mpo <- read.table("GENE_dist_MPO.txt", header = F, sep ="\t")

names(gene_dist_mpo) <- c("geneid1", "geneid2","dist")

gene_dist_mpo[gene_dist_mpo$geneid1 < gene_dist_mpo$geneid2, ]  ## <0 rows> (or 0-length row.names)

new <- cbind(gene_dist_mpo[,2], gene_dist_mpo[,1],gene_dist_mpo[,3])

new <-as.data.frame(new)

head(new)

write.table(new, "MPO_dist_final.txt", row.names = F, col.names = F, sep = "\t")

new <- read.table("MPO_dist_final.txt", header = F, sep ="\t")

##################### gene dist based on GO ##################################################


gene_GO <- read.table("mouse_gene_GO_index.txt", header = F, sep ="\t")

# gene_GO <- gene_GO[order(gene_GO$gene_id, decreasing = F),]

# head(gene_GO, 100)

names(gene_GO) <- c("gene_id", "GO")

freq <- as.data.frame(table(gene_GO$gene_id), stringsAsFactors = F)

names(freq) <- c("gene_id", "Freq")

#dim(freq[freq$Freq >= 15, ])  ## [1] 3243    2

core_gene_list <- as.data.frame(freq[freq$Freq >= 30, ], stringsAsFactors = F)

core_gene_list$gene_id <- as.character(core_gene_list$gene_id)

gene_GO$gene_id <- as.character(gene_GO$gene_id)

core_gene <- merge(gene_GO, core_gene_list, by = "gene_id")

core_gene <- unique(core_gene[,-3])

print(paste(length(unique(core_gene$gene_id)), "\t", length(unique(core_gene$GO)), sep = ""))
## 1185	8729"

str(core_gene)

core_gene$gene_id <- as.factor(core_gene$gene_id)

#write.table(table(core_gene)[1:50, 1:50], row.names = T, "go_matrix.txt", sep = "\t")

print(paste(length(unique(core_gene$gene_id)), "\t", length(unique(core_gene$GO)), sep = ""))

dim(table(core_gene))

ori_matrix <- t(table(core_gene))

write.table(t(table(core_gene)), row.names = F, col.names = T, "go_matrix.txt", sep = "\t")

ori_matrix <- read.table("go_matrix.txt", sep = "\t")

## 8730 GO terms, 1185 genes 
### run java code

gene_dist <- read.table("GENE_dist_GO.txt", header = F, sep ="\t")

head(sort(gene_dist[,3] , decreasing = T))
## distance range [0,5]

names(gene_dist) <- c("gene1", "gene2","distance")

str(gene_dist)

## scale distance:
gene_dist$distance <- round(gene_dist$distance/5, 5)

hist(gene_dist$distance)

write.table(gene_dist, "go_dist_final.txt", row.names = F, col.names = F, sep = "\t")


###############################################################################################

### merge string dist and GO dist and MPO dist: 

distance <- read.table("dist_final.txt", header = F, sep = "\t")

names(distance) <- c("gene1", "gene2", "distance")

distance <- distance[order(distance$distance, decreasing = T),]

distance <- distance[order(distance$gene2, decreasing = F),]

distance <- distance[order(distance$gene1, decreasing = F),]

# write.table(distance, "dist_final.txt", row.names = F, col.names = F, sep = "\t")


### reorder: put smaller gene id on the first column  by java

### remove duplicate gene-gene pairs(keep smaller distance pairs)


#genes_dist <- read.table("dist_reformat1.txt", header = F, sep = "\t")

indicator <- duplicated(distance[, 1:2]) ## get duplicate pairs index

table(indicator)

#   FALSE     TRUE 
#25249807  1534589 

genes_dist_select <- distance[!duplicated(distance[,1:2], fromLast=TRUE), ] 

write.table(genes_dist_select, "dist_final.txt", row.names = F, col.names = F, sep = "\t")

