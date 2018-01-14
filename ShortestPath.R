## By Qingqing Cai 17110700093  

library(igraph)

setwd("your directory")

##go_gene <- read.table("mouse_gene_GO_index.txt", header = F, sep ="\t")

string <- read.table("string_dist_final.txt", header = F, sep = "\t")

#str(string)

names(string) <- c("gene1", "gene2", "dist")

string$gene1 <- as.character(string$gene1)

string$gene2 <- as.character(string$gene2)

str(string)

tmp1 <- unique(string$gene1)

#tmp2 <- unique(string$gene2)

#sort(tmp1) == sort(tmp2)

edges <- string[, -3]

#head(edges)

network <- graph.data.frame(edges, vertices = tmp1, directed = F)

#summary(is.loop(network))  ## a connected network

#head(E(network))

#head(string)

E(network)$weights <- as.numeric(string$dist)

#range(E(network)$weights)

summary(network)

#adj <- get.adjacency(network, attr='weights')

# E(network)$weights[1]

#shortest.paths(network,"11906","5210", weights = E(network)$weights)

#get.shortest.paths(network,"11906","5210", weights = E(network)$weights)[[1]]

sink("dist.txt")

#dist_data <- shortest.paths(network, weights = E(network)$weights)
#for i in length(tmp1)

for(i in 1:5)
{
	for(j in 1:i){
		shortest_dist <- shortest.paths(network, tmp1[i], tmp1[j], weights = E(network)$weights)
		record <- paste(tmp1[i], tmp1[j], shortest_dist, sep = "\t")
		cat(record)
		cat("\n")
	}		
}

sink()
