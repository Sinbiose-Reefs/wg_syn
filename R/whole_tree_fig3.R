# whole tree of life
require (openxlsx)
require (here)

# classification of Ruggiero et al. 2014
whole_tree <- read.xlsx (here ("data", "Ruggiero_classification.xlsx"))

# class
class_taxon <- (whole_tree$X10)
#unique(whole_tree$X11)

# rm empty rows
class_taxon<- class_taxon[is.na(class_taxon)!=T]
# rm 'class'
class_taxon<- gsub ("Class ", "",class_taxon)
# rm aspas
class_taxon <- noquote(class_taxon)

# rm characters after space (observations)
class_taxon <- strsplit(class_taxon," ")
# extract only the first element
class_taxon <- sapply (class_taxon,"[",1)

# remove not named
class_taxon<- class_taxon[which(class_taxon != "N.N.")]

# remove empty names
class_taxon <- class_taxon[which(nchar(class_taxon) >0)]

# unique names
class_taxon<- unique(class_taxon)

# get the ranks
# require(taxize)
# class_taxize <- classification(class_taxon,
#                              db = 'ncbi')
# 
# # sequence of answers
# 2
# 2
# 2
# 1
# 1
# 1
# 1
# 2
# 2
# 4
# 1
# 3
# 2
# 
# # save this
#save (class_taxa,
#      file = here ("output", "whole_class_taxa.RData"))

load (file = here ("output", "ranks.RData"))
class_taxa_data <- class_taxa
load (file = here ("output", "whole_class_taxa.RData"))
class_taxa_whole <- class_taxa

# bind data
class_taxa_all <- c(class_taxa_whole,
                class_taxa_data)

# rm unclassified taxa
class_taxa_all<- class_taxa_all [which(lapply (class_taxa_all,length)>1)]

# get the rank and taxon name (if not in the level of class, get an upper level)
class_taxa_class <- lapply (class_taxa_all, function (i) {
  
  
  tryCatch(
    
    # if don't find the class
    if(length(grep("*class*",i$rank))>0) {
      
      data.frame(i [1:max(grep("*class*",i$rank)),
                    c("name","rank")])
    } else if (length(grep("*phyl*",i$rank))>0){
      # return the phylum
      data.frame(i [1:max(grep("*phyl*",i$rank)),
                    c("name","rank")])
      
    } else  {
      # return the kingdom
      data.frame(i [1:max(grep("*king*",i$rank)),
                    c("name","rank")])
      
    },
    
    error = function(e) return ("NULL")    
    
  )
})

# all unique taxa names
#unique_taxa_names<-unique(unlist(sapply (class_taxa,"[", "name")))
unique_taxa_names<-unique(unlist(sapply (class_taxa_class,"[", "name")))

# empty table to receive the names
mat_taxa <- matrix(NA,
                   nrow=1,
                   ncol=length(unique_taxa_names),
                   dimnames=list(NULL,unique_taxa_names))

# apply to find each taxa
taxa_per_name  <- lapply (sapply(class_taxa_class,"[", "name"), function (i) {
  
  mat_taxa [which(colnames(mat_taxa) %in% i)] <- 1
  ;
  mat_taxa
}
)

# melt the list
taxa_per_name <- do.call(rbind, taxa_per_name)
# names at the collapsed scale
rownames(taxa_per_name) <- unlist(lapply (class_taxa_class, 
                                          function(i)i[nrow(i),"name"]))
# replace NA by zero
taxa_per_name[is.na(taxa_per_name)]<-0

# aggregate
taxa_per_name <- aggregate (taxa_per_name, by=list(rownames(taxa_per_name)), FUN=mean)
names_tips <- taxa_per_name[,1]# names
rownames(taxa_per_name) <- names_tips
# get a cluster of the taxonomy 
require(vegan)
cluster_taxa_whole <-  hclust(
  
  vegdist(taxa_per_name[,-1],# minus names
          method="jaccard",na.rm=T),
  
  method="average"
  
)
# into phylo
require(phytools)
phylo_taxa_whole<- as.phylo(cluster_taxa_whole)

# this is the tree
plot(phylo_taxa_whole,"fan",
     show.tip.label=T,   
     adj=0.1,srt=25,cex=0.3,no.margin = T)

# now find the edges
# adjacency tree
source("R/functions.R")
adTree <- adjacency.tree (phylo_taxa_whole)
tips <- seq (1,nrow(adTree))
edges <- seq (max(tips)+1, sum(dim(adTree)))
#adTree[1,][which(adTree[1,] >0)]

## df with phylogeny edges
edge_df <- as.data.frame (phylo_taxa_whole$edge)
#internal_teste <- internal_teste[order(internal_teste$V2),]
colnames(edge_df) <- c("parent","child")
# colum to receive taxa names
edge_df$values<-NA

# load review data
load(here("output", "ALL_data_sel.RData"))
require(reshape)

# obtain studied taxa, per paper
study_taxa <- cast (ALL_data_sel,
                    StudyAuthors~TaxaUpperLevel,
                    value = "Realm"
)[,-1]

# remove some taxa to avoid problem (check how to solve after) 
study_taxa<-study_taxa [,which(colnames(study_taxa) %in% rownames(adTree))]
study_taxa <- study_taxa [which(rowSums(study_taxa>0) >1),]

# fill edges with papers' data
study_taxa_edge <- lapply (seq(1,nrow (study_taxa)), function (i) {
  
  # subset
  studied_taxa <- colnames(study_taxa[i,][which(study_taxa[i,] >0)])
  
  # select in the adjacency tree
  sel_taxa <- adTree[which(rownames(adTree) %in% studied_taxa),]
  
  # interesting nodes
  nodes_int <- which( colSums(sel_taxa) >0) + nrow(adTree)
  # non shared nodes (particular of each tip)
  non_shared_nodes <- which( colSums(sel_taxa)> 0 & 
                               colSums(sel_taxa) < nrow(sel_taxa)) + nrow(adTree)## add tips
  # nodes shared by tips
  shared_nodes <- (which( colSums(sel_taxa) == nrow(sel_taxa))) + nrow(adTree)
  # select nodes of interest
  nodes_int <- nodes_int [which(nodes_int %in% c(non_shared_nodes,shared_nodes))]
  
  # getting the edges connecting to the nodes
  edge_parent <- rownames(edge_df)[edge_df[,1] %in% nodes_int]
  edge_child  <- rownames(edge_df)[edge_df[,2] %in% nodes_int]
  
  # modify edges (replace by values)
  mod_edges <- edge_df 
  mod_edges[edge_parent,"values"] <- NA # NA for parents
  mod_edges[edge_child,"values"] <- 1
  # find the tip
  edge_studied_taxa <- which(rownames(adTree) %in% studied_taxa)
  mod_edges [which(mod_edges$child %in% edge_studied_taxa),"values"] <- 1
  
  ; # return
  mod_edges
  
  })

# work?
edge.width <- ifelse (is.na (study_taxa_edge[[1]]$values*3),
                1,
                study_taxa_edge[[1]]$values*3)
edge_color <- ifelse (edge.width == 3,
                       "black",
                       "gray90")

# examples for the first study
plot(phylo_taxa_whole,type="cladogram",
     show.tip.label=T,
     edge.width = edge.width,
     edge.color = edge_color,
     adj=0.1,srt=25,cex=0.1,no.margin = T
)

## edges to plot
values_to_phylo<-do.call (cbind,
                          sapply (study_taxa_edge, 
                                  "[","values",
                                  simplify=F))

## edges to plot
values_to_plot<- apply(values_to_phylo,1,max,na.rm=T)+3
values_to_plot [is.infinite (values_to_plot)]<-2
# distinguish taxa with no study from others with studies
#values_to_plot_adjusted <- ifelse (values_to_plot == 0, 
#                          values_to_plot + 1,
#                          values_to_plot + 5) # gap in the count
# set colors
colfunc <- colorRampPalette(c("gray80", "orange"))
edge_colors <- colfunc(max(values_to_plot)+1)[match (values_to_plot,
                                                     seq (min(values_to_plot), 
                                                          max(values_to_plot)))]

# phylogenyCrossTaxa
#pdf(here("output","CrossTaxaPhylo.pdf"),width=6,heigh=6)

#plot(phylo_taxa_whole,type="fan",
#     show.tip.label=T,
#     edge.width = values_to_plot*1,
#     edge.color = edge_colors,
#     adj=0.5,
#     srt=2,
#     cex=0.3,
#     no.margin = T
#)


#legend("topleft",
#       legend=c("# Studies per edge",seq (1,max(values_to_plot),16)), 
#       lwd=c(NA,seq (1,max(values_to_plot),16)*0.3),
#       bty="n",
#       col = colfunc(max(values_to_plot)+1)[c(NA,seq (1,max(values_to_plot),16))]
#)

#dev.off()
