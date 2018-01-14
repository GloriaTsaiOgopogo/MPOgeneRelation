## By Qingqing Cai 17110700093  

setwd("your directory")

# memory.size(F)
# ls()
# rm(list=ls())

######################################### data check ################################

MPO <- read.table("gene_MPO_sort_by_MPO.txt", header = F, sep ="\t")

gene_freq <- table(MPO[,1])

mpo_freq <- table(MPO[,2])

uni_gene <- unique(MPO[,1])

uni_MPO <- unique(MPO[,2])

length(uni_gene)  ## [1] 9167

length(uni_MPO)  ##  [1] 5500

mean(gene_freq)  ## [1] 6.202029

median(gene_freq)  ## 3

mean(mpo_freq)  ##  [1] 10.33709

median(mpo_freq)  ## [1] 3

d <- read.table("dist_final.txt", header = F, sep ="\t")

hist(d[,3])

mean(d[,3])  ## 0.4904047

median(d[,3])  ## 0.48392

gene1 <- d[,1]

gene2 <- d[,2]

uni_gene1 <- unique(gene1)  

uni_gene2 <- unique(gene2)

length(uni_gene1)  ## [1] 16360

length(uni_gene2)  ## [1] 16295

###########################################################################################

#### 

## Reading data:

## data 1: gene - MPO relation
setwd("your directory")

MPO <- read.table("gene_MPO_sort_by_MPO.txt", header = F, sep ="\t")

names(MPO) <- c("geneid", "MPO")

head(MPO)

freq <- as.data.frame(table(MPO$MPO), stringsAsFactors = F)

names(freq) <- c("MPO", "Freq")
  
MPO_conf <- as.data.frame(freq[freq$Freq >= 5, ], stringsAsFactors = F)

MPO_conf <- merge(MPO, MPO_conf, by = "MPO")

MPO_conf <- unique(MPO_conf[,-3])

## data 2: gene - gene distance

d <- read.table("dist_final.txt", header = F, sep ="\t")

names(d) <- c("geneid1", "geneid2", "dist")

#uni_gene <- unique(MPO[,1])
uni_gene <- unique(MPO_conf[,2])

uni_MPO <- unique(MPO_conf[,1])

length (uni_gene)  ##  [1] 8495

length (uni_MPO)  ##  [1] 2213


######################### Double loop test ##############################################

#request_MPO <- MPO$MPO[1]  ## MP:0002904

#tmp <- MPO[MPO$MPO == request_MPO, ]$geneid  ## [1] 20476  3611 17749  7825  4888 18942  3601  9083  2712  3850 17826

#request_gene <- MPO$geneid[1]
#request_gene <- "21693" 

#sink("test.txt")

#request_MPO <- uni_MPO[1:5]

#for(j in 1:length(request_MPO)){

#	tmp <- MPO[MPO$MPO == request_MPO[j], ]$geneid
#	tmp_dist = 0
	
#	for(i in 1:length(tmp)){
#		if(request_gene == tmp[i])
#		{
#			tmp_dist = 0.0 + tmp_dist
#		}
#		else{
#			tmp_mpo_list1 <- d[d$geneid1 == tmp[i], ]
#			tmp_dist1 <- tmp_mpo_list1[tmp_mpo_list1$geneid2 == request_gene, ][,3]
#			if(length(tmp_dist1) == 0){tmp_dist1 = 1.0}
#
#			tmp_mpo_list2 <- d[d$geneid2 == tmp[i], ]
#			tmp_dist2 <- tmp_mpo_list2[tmp_mpo_list2$geneid2 == request_gene, ][,3]
#			if(length(tmp_dist2) == 0){tmp_dist2 = 1.0}
#	
#			tmp_dist <- sum(tmp_dist, min(tmp_dist1, tmp_dist2))	
#		}
#	}
#	
#	cat(paste(request_MPO[j], request_gene, round(tmp_dist/length(tmp), 5), sep = "\t"))
#	cat("\n")
#}

#sink()

############################# Triple loop test ############################################

#################### MPO - GENE distance Calculator with confident index ##################

sink("predict.txt")

### building training data:

#request_gene <- uni_gene
#request_gene <- sample(uni_gene, size = 500, replace = F)

request_gene <- sample(uni_gene, size = 10, replace = F)

request_MPO <- uni_MPO

for(k in 1:length(request_gene)){
	
	for(j in 1:length(request_MPO)){

		tmp <- MPO_conf[MPO_conf$MPO == request_MPO[j], ]$geneid
		tmp_dist = 0
		if(length(tmp) >= 5){
			for(i in 1:length(tmp)){
				if(request_gene[k] == tmp[i])
				{
					tmp_dist = 0.0 + tmp_dist
				}
				else{
					tmp_mpo_list1 <- d[d$geneid1 == tmp[i], ]
					tmp_dist1 <- tmp_mpo_list1[tmp_mpo_list1$geneid2 == request_gene[k], ][,3]
					if(length(tmp_dist1) == 0){tmp_dist1 = 1.0}

					tmp_mpo_list2 <- d[d$geneid2 == tmp[i], ]
					tmp_dist2 <- tmp_mpo_list2[tmp_mpo_list2$geneid2 == request_gene[k], ][,3]
					if(length(tmp_dist2) == 0){tmp_dist2 = 1.0}
					
					tmp_dist <- sum(tmp_dist, min(tmp_dist1, tmp_dist2))	
				}
			}
		
			cat(paste(request_MPO[j], request_gene[k], round(tmp_dist/length(tmp), 5), length(tmp), sep = "\t"))
			cat("\n")		
		}		
	}
}

sink()


########################################################################################################
## local test

local_size = 10

sample_rows <- nrow (MPO_conf)

sample_indx <- sample (sample_rows, size = local_size, replace = F) 

sample_data <- MPO_conf[sample_indx, ]

request_gene <- sample_data[,2]

request_MPO <- as.character(sample_data[,1])

sink("loocal_test.txt")


for(k in 1:length(request_gene)){
	
	for(j in 1:length(request_MPO)){

		tmp <- MPO_conf[MPO_conf$MPO == request_MPO[j], ]$geneid
		tmp_dist = 0
		if(length(tmp) >= 5){
			for(i in 1:length(tmp)){
				if(request_gene[k] == tmp[i])
				{
					tmp_dist = 0.0 + tmp_dist
				}
				else{
					tmp_mpo_list1 <- d[d$geneid1 == tmp[i], ]
					tmp_dist1 <- tmp_mpo_list1[tmp_mpo_list1$geneid2 == request_gene[k], ][,3]
					if(length(tmp_dist1) == 0){tmp_dist1 = 1.0}

					tmp_mpo_list2 <- d[d$geneid2 == tmp[i], ]
					tmp_dist2 <- tmp_mpo_list2[tmp_mpo_list2$geneid2 == request_gene[k], ][,3]
					if(length(tmp_dist2) == 0){tmp_dist2 = 1.0}
		
					tmp_dist <- sum(tmp_dist, min(tmp_dist1, tmp_dist2))	
				}
			}
		
			cat(paste(request_MPO[j], request_gene[k], round(tmp_dist/length(tmp), 5), length(tmp), sep = "\t"))
			cat("\n")		
		}		
	}
}

sink()
