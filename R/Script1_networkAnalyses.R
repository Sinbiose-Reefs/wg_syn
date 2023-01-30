


##  --------------------------------------  ## 

#      Network analyses and figures
# fig 1, interaction networks and histograms


##  --------------------------------------  ## 



# load packages
source("R/packages.R")
source("R/functions.R")

# Load data
files <- list.files (here("data","from_readers","ReviewedByALLuza"))

# all at once
ALL_data <- lapply (files, function (i) 
  read.xlsx(here("data","from_readers","ReviewedByALLuza",i),
                    sheet = 2, # data descriptors
            colNames = TRUE,detectDates=F)[,-c(27:28)])

## set the colnames of my table to other table
ALL_data <- lapply (seq(2,length(ALL_data)), function (i) {
  colnames (ALL_data[[i]]) <- colnames(ALL_data[[1]])
  ;
  ALL_data[[i]]
}
)

## bind all data into a dataframe
ALL_data<-do.call(rbind,ALL_data)
# ALL_data[is.na(ALL_data$PaperNumber),] # no NA

# bind my data
ALL <- read.xlsx(here("data","from_readers","ReviewedByALLuza",files[1]),
            sheet = 2, # data descriptors
            colNames = TRUE,detectDates=F)[,-c(27:28)]
ALL_data<- rbind (ALL, # data collected by me, with colnames without special characters
                  ALL_data)

## All screened papers
length(unique(ALL_data$StudyAuthors))

## check which paper is lacking here
complete_search <- read.xlsx(here("data","Pooled_answers_Filtering_articles_with_questions.xlsx"),
                 sheet = 1, 
                 colNames = TRUE,detectDates=F)
interesting_research <- complete_search[which(complete_search$`Is.it.a.duplicated.record?` == "nao"),]# not duplicated
interesting_research <- interesting_research[which(interesting_research$`Q1:.PESQUISA.EM.ECOLOGIA` %in% c("sim","sim ", "SIM")),] # ecological research
interesting_research <- interesting_research[which(interesting_research$`Q2:.PESQUISA.EM.ECOLOGIA.FUNCIONAL` %in% c("sim", "SIM")),] # functional ecology research 
interesting_research <- interesting_research[which(interesting_research$`Q3:.PESQUISA.MULTI-TAXA` %in% c("sim","sim ", "SIM")),] # ecological research


# what is missing?

missing<-unique(interesting_research$PaperNumber) [which(unique(interesting_research$PaperNumber) %in% unique(ALL_data$PaperNumber) == F)]
interesting_research[which(interesting_research$PaperNumber %in% missing),]

# sel crosstaxa research
ALL_data_sel <-  ALL_data [which(ALL_data$TypeOfStudy != "Review" &
                                   ALL_data$IsItCrossTaxaFD != "no" &
                                   ALL_data$IsItCrossTaxaFD != "No"),]

# remove NA (paper about ecossystem multifunctionality)
to_rm <- rownames(ALL_data_sel[which(is.na(ALL_data_sel$TaxonomicRank)== T),])
ALL_data_sel <- ALL_data_sel [-which(rownames(ALL_data_sel)%in% to_rm),]
# set of papers
length(unique(ALL_data_sel$PaperNumber))


# studied systems

obs_studies <- table (ALL_data_sel$PaperNumber,
       ALL_data_sel$TypeOfStudy)
# observational
sum(rowSums(obs_studies [,c("Case study", "observation", "Observation", "observational", "theoretical/observation")]>0))/nrow(obs_studies)
# experiments
sum(rowSums(obs_studies [,c("experiment","Experiment","experimental","Mesocosmos")]>0))/nrow(obs_studies)
# meta
sum(rowSums(obs_studies [,c("meta-analysis","Meta-analysis","Meta-analytic framework")]>0))/nrow(obs_studies)
# paleo
sum((obs_studies [,"Palaeoecology"]>0))/nrow(obs_studies)


# -----------------------------------------------------------------------------#
## Analysis 1: what's the main taxonomic rank of these cross-taxa research?

source("R/adjustment.R")
ALL_data_sel <- adjust_rank (ALL_data_sel)# adjust ranks
# ranks
unique(ALL_data_sel$TaxonomicRank)[order(unique(ALL_data_sel$TaxonomicRank))]
# check
# ALL_data_sel[which(is.na(ALL_data_sel$TaxonomicRank)),]
# taxa
ALL_data_sel <- adjust_names (ALL_data_sel)# adjust taxa names
unique(ALL_data_sel$WhichOrganism)[order(unique(ALL_data_sel$WhichOrganism))] # organisms
unique(ALL_data_sel$WhichTrait)[order(unique(ALL_data_sel$WhichTrait))] # traits 

# check
# ALL_data_sel[which(ALL_data_sel$WhichOrganism == "Caenidae"),]

# entries
length(unique(ALL_data_sel$PaperNumber)) * 
length(unique(ALL_data_sel$WhichOrganism)[order(unique(ALL_data_sel$WhichOrganism))])*  
length(unique(ALL_data_sel$WhichTrait)[order(unique(ALL_data_sel$WhichTrait))]) 

# number of studies per taxonomic rank
summ_data <- data.matrix(cast (ALL_data_sel, 
                              PaperNumber~TaxonomicRank,
                              value="Realm"))
summ_data[summ_data>1] <- 1
summ_data <- summ_data[,which(colnames(summ_data) != "PaperNumber" 
                              &
                                colnames(summ_data) != "NA")]
# barplot
tax.level <- colSums(summ_data)
tax.level<- tax.level[match( (c(
                    "Domain",
                    "Kingdom",
                     "Phylum",
                    "Subphylum",
                     "Class",
                    "Subclass",
                    "Superorder",
                     "Order",
                    "Suborder",
                     "Infraorder",
                    "Superfamily",
                     "Family")),
                   names(tax.level))]

# number of ranks per study
ranksPerStudy <- rowSums (summ_data)

# ------------------------------------------------------ #
# composition 1
# studies per ranks

tax.level.df <- data.frame (taxa=names(tax.level),
                            val = tax.level)
tax.level.df$taxa<-factor(tax.level.df$taxa,
                          levels = c("Domain",
                                     "Kingdom",
                                     "Phylum",
                                     "Subphylum",
                                     "Class",
                                     "Subclass",
                                     "Superorder",
                                     "Order",
                                     "Suborder",
                                     "Infraorder",
                                     "Superfamily",
                                     "Family"))

# plot of N studies per rank
plot1 <- ggplot (data=tax.level.df, 
                 aes (fill=val,
                      y=taxa,
                      x=val))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_gradient2(low="gray80", mid= "#BB8760", high="orange",midpoint = 11,
                       limits = c(1,50))+
  theme (axis.text.x = element_text(angle = 90 ,
                                    vjust = 0.5, hjust=1),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "top"
  )+
  ylab("Taxonomic Rank") +
  xlab ("Frequency") +
  labs(fill="") + 
  coord_flip()
plot1

# plot of N studies per number of ranks
ranksPerStudyDF <- table(ranksPerStudy)
ranksPerStudyDF <- data.frame(ranksPerStudyDF)

# plot
plot2 <- ggplot (data=ranksPerStudyDF, aes (fill=Freq,
                                      y=Freq,
                                      x=ranksPerStudy))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_gradient2(low="gray80", mid= "#BB8760", high="orange",midpoint = 11,
                       limits = c(1,50))+
  theme (axis.text.x = element_text(angle = 0, 
                                    vjust = 0, 
                                    hjust=0),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "top"
  )+
  xlab("# Taxonomic Ranks") +
  ylab ("Frequency") +
  labs(fill=" ")

# obtain studied taxa, per paper
study_taxa <- cast (ALL_data_sel,
                    StudyAuthors~WhichOrganism)[,-1]

# example
ex <- cast (ALL_data_sel,
      StudyAuthors~WhichOrganism)
ex[which(ex$Araneae>0),]
## find the most studied taxa
taxonMoreSt<-table(
  
  unlist(lapply (seq(1,nrow(study_taxa)), function (i) {
    
    groups_per_study <- colnames(study_taxa[i,][which(study_taxa[i,]>0)])
    
    
  })))

mostOftenStudiedTaxa <- (taxonMoreSt[order(taxonMoreSt,decreasing=T)])
mostOftenStudiedTaxa <- mostOftenStudiedTaxa[which(mostOftenStudiedTaxa >=2)]

# most often studied taxa
plot3 <- ggplot (data=data.frame(mostOftenStudiedTaxa), aes (fill=Freq,
                                                             y=Freq,
                                                             x=Var1))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_gradient2(low="gray80", mid= "#BB8760", high="orange",midpoint = 11,
                       limits = c(1,50))+
  theme (axis.text.x = element_text(angle = 90, 
                                    vjust = 0.5, hjust=1),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "top"
  )+
  xlab("Taxa") +
  ylab ("Frequency") +
  labs(fill="# Studies")

pdf(here("output","fig1.pdf"),width=6,heigh=8)

grid.arrange(plot1+theme(legend.position = "none"),
             plot2+theme(legend.position = "none"),
             plot3+theme(legend.position = c(0.5,0.8),
                         legend.direction = "horizontal"),
             ncol=2,nrow=5,
             layout_matrix = rbind (c(1,2),
                                    c(1,2),
                                     c(3,3),
                                    c(3,3),
                                    c(3,3)
                              ))


dev.off()


# ------------------------
# links in the phylogeny: what are the main connections across studies

# taxonomic ranking (run once (slow) and save)
#class_taxa <- classification(unique(ALL_data_sel$WhichOrganism),
#                      db = 'ncbi')
# save
#save(class_taxa,
#     file=here("output","ranks.RData"))
# load

load(file=here("output","ranks.RData"))


# rm unclassified taxa
class_taxa<- class_taxa [which(lapply (class_taxa,length)>1)]

# get the rank and taxon name (if not in the level of class, get an upper level)

class_taxa_ranks <- lapply (class_taxa, function (i) {
          
            
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
unique_taxa_names<-unique(unlist(sapply (class_taxa_ranks,"[", "name")))

# empty table to receive the names
mat_taxa <- matrix(NA,
                   nrow=1,
                   ncol=length(unique_taxa_names),
                   dimnames=list(NULL,unique_taxa_names))

# apply to find each taxa
taxa_per_name  <- lapply (sapply(class_taxa_ranks,"[", "name"), function (i) {
  
  mat_taxa [which(colnames(mat_taxa) %in% i)] <- 1
  ;
  mat_taxa
}
)

# melt the list
taxa_per_name <- do.call(rbind, taxa_per_name)
# names at the collapsed scale
rownames(taxa_per_name) <- unlist(lapply (class_taxa_ranks, 
                                          function(i)i[nrow(i),"name"]))
# replace NA by zero
taxa_per_name[is.na(taxa_per_name)]<-0

# aggregate
taxa_per_name <- aggregate (taxa_per_name, by=list(rownames(taxa_per_name)), FUN=mean)
names_tips <- taxa_per_name[,1]# names
rownames(taxa_per_name) <- names_tips

# get a cluster of the taxonomy 
cluster_taxa <-  hclust(
  
  vegdist(taxa_per_name[,-1],# minus names
          method="jaccard",na.rm=T),
  
  method="average"
  
)
# into phylo
phylo_taxa<- as.phylo(cluster_taxa)

# this is the tree
plot(phylo_taxa,"fan",
     show.tip.label=T,   
     adj=0.1,srt=25,cex=0.7,no.margin = T)

# now find the edges
# adjacency tree
adTree <- adjacency.tree (phylo_taxa)
tips <- seq (1,nrow(adTree))
edges <- seq (max(tips)+1, sum(dim(adTree)))
#adTree[1,][which(adTree[1,] >0)]

## df with phylogeny edges
edge_df <- as.data.frame (phylo_taxa$edge)
#internal_teste <- internal_teste[order(internal_teste$V2),]
colnames(edge_df) <- c("parent","child")
# colum to receive taxa names
edge_df$values<-NA

# edges are not in the same taxonomic rank as data
#select taxon
taxa_upper_level <- lapply ((ALL_data_sel$WhichOrganism), function (i)
  
  tryCatch({
  
    tax <- class_taxa_ranks[names(class_taxa_ranks) %in% i ] [[1]] # inside the list
    
    tax <- tax[nrow(tax),] # select the last line (level)
    
    
},
error = function(e) return ("NULL")))

# melt
taxa_upper_level <- do.call(rbind,taxa_upper_level)
# bind to the data
ALL_data_sel$TaxaUpperLevel<- unlist(taxa_upper_level[,"name"])
ALL_data_sel$RankUpperLevel<- unlist(taxa_upper_level[,"rank"])


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
# examples for the first study
plot(phylo_taxa,type="cladogram",
     show.tip.label=T,
     edge.width = study_taxa_edge[[1]]$values*3,
     #edge.color = edge_colors,
     adj=0.1,srt=25,cex=0.7,no.margin = T
)

## edges to plot
values_to_phylo<-do.call (cbind,
                          sapply (study_taxa_edge, "[","values",simplify=F))

## edges to plot
values_to_plot<- rowSums(values_to_phylo,na.rm=T)

# set colors
colfunc <- colorRampPalette(c("gray", "orange"))
edge_colors <- colfunc(max(values_to_plot)+1)[match (values_to_plot,
                                                     seq (min(values_to_plot), 
                                                          max(values_to_plot)))]

# phylogenyCrossTaxa
# pdf(here("output","CrossTaxaPhylo"),width=6,heigh=6)

plot(phylo_taxa,type="fan",
     show.tip.label=T,
     edge.width = values_to_plot*0.3,
     edge.color = edge_colors,
     adj=0.5,
     srt=2,
     cex=0.6,
     no.margin = T
)


legend("topleft",
       legend=c("# Studies per edge",seq (1,66,16)), 
       lwd=c(NA,seq (1,66,16)*0.3),
       bty="n",
       col = colfunc(max(values_to_plot)+1)[c(NA,seq (1,66,16))]
)

#dev.off()


# ====================================================================
# interaction plot
# help here: https://r-inspirations.blogspot.com/2016/08/create-bipartite-graph-with-igraph.html
# all combinations of pairs of taxa
comb_taxa <- t(combn(unique(ALL_data_sel$TaxonomicRank),2))

# pasting the cols make it easier to compare
comb_taxa<- data.frame(t1 = comb_taxa[,1], 
                       t2= comb_taxa[,2],
                       comb = paste (comb_taxa[,1],comb_taxa[,2],
                                     sep="."))

# now find the combination per article
comb_taxa_study <- lapply (unique (ALL_data_sel$PaperNumber), function (i)
  
  tryCatch(
    t(combn(unique(ALL_data_sel$TaxonomicRank[which(ALL_data_sel$PaperNumber == i)]),
            2)), 
    error = function(e) return ("NULL")
  )
)

# melt the list
comb_taxa_study <- do.call (rbind,comb_taxa_study)
# rm NULL (these are all possible combinations of taxa in data)
#comb_taxa_study<- comb_taxa_study[-grep("NULL",comb_taxa_study[,1]),]
#comb_taxa_study<- comb_taxa_study[-grep("NULL",comb_taxa_study[,2]),]
# paste make it easier to compare
# pasting the cols make it easier to compare
comb_taxa_study<- data.frame(t1 = comb_taxa_study[,1], 
                             t2= comb_taxa_study[,2],
                             comb = paste (comb_taxa_study[,1],comb_taxa_study[,2],
                                           sep="."))
# how many times each combination appears
Ntimes <- table (comb_taxa_study$comb)

# matching with all combinations
comb_edges <- cbind (comb_taxa,
                     Ntimes[match (comb_taxa$comb, names(Ntimes))])
# rm columns we don't want
comb_edges <- comb_edges[,-c(3,4)]
colnames(comb_edges)<- c("t1","t2", "weight")
# replace NA by zero
comb_edges$weight[is.na(comb_edges$weight)] <-0

# transform
m_web <- cast (formula = t2~t1,
      value = "weight",
      fun.aggregate = sum,
  data = comb_edges)
labels_web <- m_web[,1]
m_web<- m_web[,-1]
# adjust
rownames(m_web)<- labels_web
# ordering
m_web<-sortweb(m_web,sort.order="dec")

# set colors
test <-unlist ((m_web))
colfunc <- colorRampPalette(c("gray", "orange"))
edge_colors <- colfunc(max(test)+1)[match (test,
                                                  seq (min(test), 
                                                      max(test)))]

# set colors

Int_cols<- matrix(edge_colors,
                  nrow = nrow (m_web),
                  ncol= ncol (m_web),
                  byrow=T)



pdf(here ("output", "web_ranks"),height=7,width=7)

# plot
plotweb(data.matrix(m_web), method = "normal",empty=F,
        col.low="gray",
        col.high="gray",
        text.high.col="blue", 
        col.interaction=Int_cols,
        bor.col.interaction="brown",
        labsize = 2,
        text.rot=90, 
        low.lablength=10, 
        high.lablength=5)


dev.off()

# nestedness in this bipartite network
(nestedness_bipartite <- oecosimu(m_web[order (rowSums(m_web),decreasing=T),order (colSums(m_web),decreasing=T)], 
                                nestednodf, 
                                "swap", 
                                nsimul = 999,
                                alternative = "two.sided")
)




# save the dataset

save (ALL_data_sel,phylo_taxa, file= here ("output", "ALL_data_sel.RData"))



# end
rm(list=ls())





