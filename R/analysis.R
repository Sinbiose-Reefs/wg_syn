
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
#interesting_research <- interesting_research[which(interesting_research$`Q4:.REVISAO.BIBLIOGRAFICA` %in% c("nao","nao ", "NAO")),] # not review
#interesting_research <- interesting_research[which(interesting_research$`Q5:.META-ANALISE` %in% c("NAO","nao")),] # ecological research

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
                                    vjust = 0, 
                                    hjust=0),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "top"
  )+
  ylab("Taxonomic Rank") +
  xlab ("Frequency") +
  labs(fill="") + 
  coord_flip()

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
                                    vjust = 0, 
                                    hjust=0),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "top"
  )+
  xlab("Taxa") +
  ylab ("Frequency") +
  labs(fill="# Studies")


pdf(here("output","fig1"),width=6,heigh=8)

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

# taxonomic ranking
#class_taxa <- classification(unique(ALL_data_sel$WhichOrganism),
#                      db = 'ncbi')
# save
#save(class_taxa,
#     file=here("output","ranks.RData"))

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
pdf(here("output","CrossTaxaPhylo"),width=6,heigh=6)

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

dev.off()


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
# how many tumes each combination appears
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

pdf(here ("output", "web_ranks"),height=7,width=7)

# plot
plotweb((m_web), method = "normal",empty=F,
        col.low="gray",
        col.high="gray",
        text.high.col="blue", 
        col.interaction="orange",
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

# ====================================================================
# interaction plot
# help here: https://r-inspirations.blogspot.com/2016/08/create-bipartite-graph-with-igraph.html
# all combinations of pairs of taxa
comb_taxa <- t(combn(unique(ALL_data_sel$TaxaUpperLevel),2))
# rm NULL (these are all possible combinations of taxa)
comb_taxa<- comb_taxa[-grep("NULL",comb_taxa[,1]),]
comb_taxa<- comb_taxa[-grep("NULL",comb_taxa[,2]),]
# pasting the cols make it easier to compare
comb_taxa<- data.frame(t1 = comb_taxa[,1], 
                       t2= comb_taxa[,2],
                         comb = paste (comb_taxa[,1],comb_taxa[,2],
                                sep="."))

# now find the combination per article
comb_taxa_study <- lapply (unique (ALL_data_sel$PaperNumber), function (i)
  
  tryCatch(
    t(combn(unique(ALL_data_sel$TaxaUpperLevel[which(ALL_data_sel$PaperNumber == i)]),
            2)), 
    error = function(e) return ("NULL")
  )
)

# melt the list
comb_taxa_study <- do.call (rbind,comb_taxa_study)
# rm NULL (these are all possible combinations of taxa in data)
comb_taxa_study<- comb_taxa_study[-grep("NULL",comb_taxa_study[,1]),]
comb_taxa_study<- comb_taxa_study[-grep("NULL",comb_taxa_study[,2]),]
# paste make it easier to compare
# pasting the cols make it easier to compare
comb_taxa_study<- data.frame(t1 = comb_taxa_study[,1], 
                            t2= comb_taxa_study[,2],
                             comb = paste (comb_taxa_study[,1],comb_taxa_study[,2],
                                     sep="."))
# how many tumes each combination appears
Ntimes <- table (comb_taxa_study$comb)

# matching with all combinations
comb_edges <- cbind (comb_taxa,
                      Ntimes[match (comb_taxa$comb, names(Ntimes))])
# rm columns we don't want
comb_edges <- comb_edges[,-c(3,4)]
colnames(comb_edges)<- c("t1","t2", "weight")
# replace NA by zero
comb_edges$weight[is.na(comb_edges$weight)] <-0
# removing missing combinations
comb_edges <-comb_edges[which (comb_edges$weight >0),]

#igraph
g <- graph.data.frame(comb_edges, 
                      directed = F)

#V(g)$type <- V(g)$name %in% comb_edges[,2] #the second column of edges is TRUE type
E(g)$weight <- as.numeric(comb_edges$weight)
colfunc <- colorRampPalette(c("gray", "orange"))
edge_colors <- colfunc(max(E(g)$weight)+1)[match (E(g)$weight,
                                                     seq (min(E(g)$weight), 
                                                          max(E(g)$weight)))]

# plot
#V(g)$color <- V(g)$type
#V(g)$color=gsub("FALSE","red",V(g)$color)
#V(g)$color=gsub("TRUE","blue",V(g)$color)

# plot
# help here: https://www.r-graph-gallery.com/248-igraph-plotting-parameters.html

pdf (here ("output", "net_taxa.pdf"))
plot(g, 
	edge.color=edge_colors,
	edge.width=E(g)$weight, 
	layout=layout_nicely,
	axes=F,
	vertex.size=4,
	vertex.label.cex=1,
	vertex.color="gray60",                          
	vertex.label.dist=1,                           # Distance between the label and the vertex
    vertex.label.degree=-pi/2,
	vertex.label.color="black",
	vertex.label.family = "sans",
	vertex.label.dist=100)


legend("topright",
       legend=c("# Studies per link",seq (1,16,5)), 
       lwd=c(NA,seq (1,16,5)),
       bty="n",
       col = colfunc(max(E(g)$weight)+1)[c(NA,seq (1,16,5))]
)


dev.off()


# modularity in this network
wtc <- cluster_walktrap(g)
obs_modularity_wtc <- modularity(g, membership(wtc))

# Make null models for all sites using the swap.web null
# matrix
test_mat <- cast(data = comb_edges, formula = t1~t2, value= "weight",
                 na.rm=T,fill=0)
rownames(test_mat)<-test_mat[,1]; test_mat<-test_mat[,-1]


# compute Modularity
m_network<-computeModules(data.matrix(test_mat), method="Beckett")
m_network_df <- data.frame (value=m_network@likelihood,test="obs")

# Check the components of each module
printoutModuleInformation(m_network)

png(here ("output","modules"),width = 12, height = 12, units = "cm",res=300)
plotModuleWeb(m_network,labsize = 0.6)
dev.off()

# Set Null Model
nulls <- nullmodel(data.matrix(test_mat), N=100, method="vaznull") 
modules.nulls <- sapply(nulls, computeModules, method="Beckett")
like.nulls <- sapply(modules.nulls, function(x) x@likelihood)
like.nulls<-data.frame (value = like.nulls,
                        random=seq(1,length(like.nulls)))
z <- (m_network@likelihood - mean(like.nulls$value))/sd(like.nulls$value)
p <- 2*pnorm(-abs(z))
quantile(like.nulls$value, probs = c(0.025,0.975))


#  density plot 
p<- ggplot(like.nulls, aes(x=value)) +
  geom_density(fill="gray")+
  geom_vline(data=m_network_df, aes(xintercept=value),
             linetype="dashed",size=1)+
  labs(title="Density curve",x="Modularity(Q)", 
       y = "Density")
p<-p + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))+
  theme_classic() + 
  xlim(0.3, 0.6) + 
  geom_text (aes (x=m_network_df$value-0.04,
                  y=20,
                  label=paste ("Q=",round(m_network_df$value,2))))


png(here ("output","modularity"),width = 9, height = 9, units = "cm",res=300)
p
dev.off()
## ----------------------------------------------------------------------- #

# research context

# sankey por n taxa
ALL_data_sel$ResearchContextEdited<-ALL_data_sel$ResearchContext
ALL_data_sel$ResearchContextEdited<-gsub (";", ",", ALL_data_sel$ResearchContextEdited)
ALL_data_sel$ResearchContextEdited<-gsub ("and", ",", ALL_data_sel$ResearchContextEdited)
ALL_data_sel$ResearchContextEdited <- tolower(ALL_data_sel$ResearchContextEdited)
#ResearchContext <- unique(ALL_data_sel$ResearchContextEdited)

# adjust context
context <- lapply (unique (ALL_data_sel$PaperNumber), function (paper) 
    lapply (seq (1,nrow (ALL_data_sel[which(ALL_data_sel$PaperNumber == paper),])), function (line) {
    
      # subset data (each entry)
  testA <- ALL_data_sel[which(ALL_data_sel$PaperNumber == paper),][line,"ResearchContextEdited"]
  # split based on ','
  testB <- unlist(strsplit(testA,","))
  # replicate the row based on the number of elements in testB (contexts)
  testC <- replicate(length(testB),
            ALL_data_sel[which(ALL_data_sel$PaperNumber == paper),][line,],
            simplify=F)
  # melt 
  testC <- do.call(rbind, testC)
  # bind unique context
  testC <- cbind (testC,
                  uniqueContext=testB)

  }))
  
# melt 
context<- lapply (context, function (paper)
  
  do.call(rbind, paper))
# melt the last level
context <- do.call(rbind,context)

# replace spaces
context$uniqueContext <- ifelse (substring(context$uniqueContext, 1, 1) == " ",
                             substring(context$uniqueContext, 2),
                             substring(context$uniqueContext, 1))

# change levels
context$uniqueContext[which(context$uniqueContext == "community assembly")]<- "community structure"
context$uniqueContext[which(context$uniqueContext == "community structure ")]<- "community structure"
context$uniqueContext[which(context$uniqueContext == "ecosystem function ")]<- "ecosystem functioning"
context$uniqueContext[which(context$uniqueContext == "functioning")]<- "ecosystem functioning"
context$uniqueContext[which(context$uniqueContext == "conservation ")]<- "conservation"
context$uniqueContext[which(context$uniqueContext == "bioassessment")]<- "ecosystem monitoring"
context$uniqueContext[which(context$uniqueContext == "environmental monitoring")]<- "ecosystem monitoring"
context$uniqueContext[which(context$uniqueContext == "services")]<- "ecosystem services"

# meta-analysis here is not ok
# given the study content, ecosystem monitoring is ok
context$uniqueContext[which(context$uniqueContext == "meta-analysis")] <- "ecosystem monitoring"

# table to worldcloud
paper_context <- cast ( PaperNumber ~uniqueContext,
               value = "Realm",
      
    data=context)[,-1]

# plot
ResearchContext <- data.frame(ResearchContext=names(colSums(paper_context>0)), 
                               freq= colSums(paper_context>0))

# world cloud

par(mfrow=c(1,1))
set.seed(1234)
pdf (file=here ("output","word_cloud_ResContext"),
	width=10,height=10)

wordcloud(words = ResearchContext$ResearchContext, freq = ResearchContext$freq,
          min.freq = 1,scale=c(1.5,0.5),
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.2, 
          colors=brewer.pal(8, "Dark2"))

dev.off()

# ========================================================================
### used indexes

# sankey por n taxa
context$Indices <- context$IndexesMeasuresIndicators
context$Indices<-gsub (";", ",", context$Indices)
context$Indices<-gsub (fixed(" and "), ",", context$Indices)
context$Indices <- tolower(context$Indices)
#ResearchContext <- unique(ALL_data_sel$ResearchContextEdited)

# adjust context
indices <- lapply (unique (context$PaperNumber), function (paper) 
  lapply (seq (1,nrow (context[which(context$PaperNumber == paper),])), function (line) {
    
    # subset data (each entry)
    testA <- context[which(context$PaperNumber == paper),][line,"Indices"]
    # split based on ','
    testB <- unlist(strsplit(testA,","))
    # replicate the row based on the number of elements in testB (contexts)
    testC <- replicate(length(testB),
                       context[which(context$PaperNumber == paper),][line,],
                       simplify=F)
    # melt 
    testC <- do.call(rbind, testC)
    # bind unique context
    testC <- cbind (testC,
                    uniqueIndex=testB)
    
  }))

# melt 
indices<- lapply (indices, function (paper)
  
  do.call(rbind, paper))

# melt the last level
indices <- do.call(rbind,indices)

# replace spaces
# first char
indices$uniqueIndex <- ifelse (substring(indices$uniqueIndex, 1, 1) == " ",
                                 substring(indices$uniqueIndex, 2),
                                 substring(indices$uniqueIndex, 1))

# last char
indices$uniqueIndex <- ifelse (substring(indices$uniqueIndex, nchar (indices$uniqueIndex), nchar (indices$uniqueIndex)) == " ",
                               substring(indices$uniqueIndex, 1,nchar(indices$uniqueIndex)-1),
                               substring(indices$uniqueIndex, 1,nchar(indices$uniqueIndex)))


# empty
(indices[which( indices$uniqueIndex == ""),"IndexesMeasuresIndicators"]) # empty things are cases of "richness, and t ..." which was replaced by "richness, , t"

# remove special characters
indices$uniqueIndex <- gsub("[']", "", indices$uniqueIndex) 
indices$uniqueIndex <- gsub("[[:punct:]]", " ", indices$uniqueIndex)
indices$uniqueIndex <- gsub('[[:digit:]]+', " ", indices$uniqueIndex)
indices$uniqueIndex <- gsub("[[:space:]]+", " ", indices$uniqueIndex)

# adjusting
# correct index names
# SR 
indices$uniqueIndex[which(indices$uniqueIndex == "richness")] <- "species richness"
indices$uniqueIndex[grep("species richness in*",indices$uniqueIndex)] <- "species richness"
indices$uniqueIndex[which(indices$uniqueIndex == "multiple metrics (richness")] <- "species richness"
indices$uniqueIndex[which(indices$uniqueIndex == "sr")] <- "species richness"

# functional diversity
indices$uniqueIndex[which(indices$uniqueIndex == "fric")] <- "functional richness"
indices$uniqueIndex[which(indices$uniqueIndex == "functional diversity such as fric")] <- "functional richness"
indices$uniqueIndex[which(indices$uniqueIndex == "functional analysis fric")] <- "functional richness"
indices$uniqueIndex[which(indices$uniqueIndex == "trait space")] <- "functional richness"
indices$uniqueIndex[which(indices$uniqueIndex == "feve")]<- "functional evenness"
indices$uniqueIndex[which(indices$uniqueIndex == "fdiv")]<- "functional divergence"
indices$uniqueIndex[grep("tdap",indices$uniqueIndex)] <- "trait divergence"
indices$uniqueIndex[grep("tcap",indices$uniqueIndex)] <- "trait convergence"
indices$uniqueIndex[grep("dendrogram-based fd",indices$uniqueIndex)] <- "functional diversity"
indices$uniqueIndex[grep("dendrogram based fd",indices$uniqueIndex)] <- "functional diversity"
indices$uniqueIndex[grep("average trait diversity per species",indices$uniqueIndex)] <- "functional diversity"
indices$uniqueIndex[grep("functional dispersion",indices$uniqueIndex)] <- "functional dispersion"
indices$uniqueIndex[grep("fdis",indices$uniqueIndex)] <- "functional dispersion"
indices$uniqueIndex[grep("rao",indices$uniqueIndex)] <- "functional dispersion" ####
indices$uniqueIndex[grep("null models",indices$uniqueIndex)] <- "functional diversity"
indices$uniqueIndex[grep("mean effect size",indices$uniqueIndex)] <- "functional diversity"
indices$uniqueIndex[grep("ardized effect sizes of fd",indices$uniqueIndex)] <- "functional diversity"
indices$uniqueIndex[grep("ardized effect sizes",indices$uniqueIndex)] <- "functional diversity"

# functional composition
indices$uniqueIndex[grep("cwm",indices$uniqueIndex)]<- "functional composition"
indices$uniqueIndex[grep("community weighted *",indices$uniqueIndex)]<- "functional composition"
indices$uniqueIndex[grep("community-weighted *",indices$uniqueIndex)]<- "functional composition"
indices$uniqueIndex[grep("functional composition analysis",indices$uniqueIndex)] <- "functional composition"
indices$uniqueIndex[grep("trait frequency",indices$uniqueIndex)] <- "functional composition"

# taxonomic diversity
indices$uniqueIndex[grep("taxonom*",indices$uniqueIndex)] <- "taxonomic diversity"
indices$uniqueIndex[grep("shannon*",indices$uniqueIndex)] <- "taxonomic diversity"
indices$uniqueIndex[grep("margalef*",indices$uniqueIndex)] <- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "alpha")]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "alpha diversity")]<- "taxonomic diversity"
indices$uniqueIndex[grep("alfa*", indices$uniqueIndex)]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "simpson")]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "simpsons")]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "simpson diversity")]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "evenness")]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "evenness ")]<- "taxonomic diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "diversity")]<- "taxonomic diversity"

# relationships
indices$uniqueIndex[grep("correlation",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("covariance",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("glm",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("pft richness ratio",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("path analysis",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("svn",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("scaling of body mass",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("linear models",indices$uniqueIndex)] <- "relationships"
indices$uniqueIndex[grep("ratio",indices$uniqueIndex)] <- "relationships"

# ecomorpho space
indices$uniqueIndex[grep("ecometric space pca",indices$uniqueIndex)] <- "ecomorphospace"
indices$uniqueIndex[grep("geometric morphometrics analysis",indices$uniqueIndex)] <- "ecomorphospace"

# taxonomic composition
indices$uniqueIndex[which(indices$uniqueIndex == "composition")]<- "taxonomic composition"
indices$uniqueIndex[which(indices$uniqueIndex == "indicator species analysis")]<- "taxonomic composition"
indices$uniqueIndex[grep("biological quality soil",indices$uniqueIndex)]<- "taxonomic composition"
indices$uniqueIndex[grep("ant quality assessment",indices$uniqueIndex)]<- "taxonomic composition"
indices$uniqueIndex[grep("floristic quality*",indices$uniqueIndex)]<- "taxonomic composition"

# multivariate analysis
indices$uniqueIndex[grep("composition (cca)",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("multivariate statistics",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("permanova npmanova",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("simper",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("manova",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("cpa",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("pca",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("multivariate analysis of composition",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("multivariate analysis discriminant analysis",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("multivariate analysis discriminant analysis ",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("metamds",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("ordination",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[grep("multivariate dispersion",indices$uniqueIndex)] <- "taxonomic composition"
indices$uniqueIndex[which(indices$uniqueIndex == "da")] <- "taxonomic composition"
indices$uniqueIndex[which(indices$uniqueIndex == "da ")] <- "taxonomic composition"
indices$uniqueIndex[which(indices$uniqueIndex == "radar plot")] <- "taxonomic composition"

# beta diversity
indices$uniqueIndex[grep("jaccard dissimilarity",indices$uniqueIndex)] <- "beta diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "beta")] <- "beta diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "beta diversity sorensen index")] <- "beta diversity"

# functional beta diversity
indices$uniqueIndex[which(indices$uniqueIndex == "trait beta diversity")] <- "functional beta diversity"

# phylogenetic diversity
indices$uniqueIndex[which(indices$uniqueIndex == "mean phylogenetic distance")] <- "phylogenetic beta diversity"

# occurrence
indices$uniqueIndex[which(indices$uniqueIndex == "occupancy multi taxa site occupancy modeling ")] <- "occurrence"
indices$uniqueIndex[which(indices$uniqueIndex == "multi species community model bayesian hierarchical framework ")] <- "occurrence"
indices$uniqueIndex[which(indices$uniqueIndex == "occurence")] <- "occurrence"

# abudance/density
indices$uniqueIndex[which(indices$uniqueIndex == "relative abundance")] <- "abundance/density/demography"
indices$uniqueIndex[which(indices$uniqueIndex == "abundance")] <- "abundance/density/demography"
indices$uniqueIndex[grep("density",indices$uniqueIndex)] <- "abundance/density/demography"

# ecosystem metrics
indices$uniqueIndex[which(indices$uniqueIndex == "energy flux")] <- "ecosystem metrics"
indices$uniqueIndex[which(indices$uniqueIndex == "ecosystem primary productivity")] <- "ecosystem metrics"
indices$uniqueIndex[grep("biomass", indices$uniqueIndex)] <- "ecosystem metrics"

# decomposition of diversity
indices$uniqueIndex[which(indices$uniqueIndex == "hill numbers")] <- "decomposition of diversity"
indices$uniqueIndex[which(indices$uniqueIndex == "variance decomposition")] <- "decomposition of diversity"
unique(indices$uniqueIndex)

# table to worldcloud
paper_index <- cast ( PaperNumber ~uniqueIndex,
                        value = "Realm",
                        
                        data=indices)
# filter
paper_index <- paper_index[,-which (colnames(paper_index)  == "V1")]
paper_index <- paper_index[,-1]
# plot
paper_index <- data.frame(ResearchContext=names(colSums(paper_index>0)), 
                              freq= colSums(paper_index>0))
# more than 1 study
paper_index <- paper_index[which(paper_index$freq>1),]
paper_index <- paper_index[order(paper_index$freq),]# order
# world cloud

# plot
plot2 <- ggplot (data=paper_index, aes (fill=freq,
                                            y=freq,
                                            x=reorder(ResearchContext,-freq)))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_gradient2(low="gray80", mid= "#BB8760", high="orange",midpoint = 11,
                       limits = c(1,50))+
  theme (axis.text.x = element_text(angle = 90 ,
                                    vjust = 0, 
                                    hjust=0),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "none"
  )+
  xlab("Analytical approaches") +
  ylab ("Frequency") +
  labs(fill=" ")



pdf(here("output","fig_analytical_procedures"),width=4,heigh=5)

plot2

dev.off()

## calling ALL_data_sel the new table with indices
ALL_data_sel <- indices
# -----------------------------------------------
# compare vs. combine
# ajust

ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Compare (they analyze each taxon separately)")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Compare responses with environmetal gradient")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Compare responses with structural complexity as a prox of management")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "They compared richness, diversity, functional redundancy patterns across different taxonomic groups (plants, ants and small mammals) along a gradient of livestock grazing, using both effect and response traits.")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Combine (they gather all the taxa for analyses)")] <- "combine"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Different mesofauna groups had a different set of traits and were assessed through a single eco-morphological index" )] <- "combine"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Different mesofauna groups had a different set of traits and were assessed through a single eco-morphological index (QBS)" )] <- "combine"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "They compared the richness and abundance of taxonomic groups within forest reserves and managed sites")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Large fragments were associated to forest interior species, irregular shaped fragments were associated with greater abundances of edge-tolerant species")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Combine within plants (they gather all the taxa for analyses)")] <- "combine"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "Combine and compare")] <- "both"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "combined")] <- "combine"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "compared (correlated birds and insects)")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "compared")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "compared")] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[grep("compare",ALL_data_sel$HowTheyUseCrossTaxaData)] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[grep("compared",ALL_data_sel$HowTheyUseCrossTaxaData)] <- "compare"
ALL_data_sel$HowTheyUseCrossTaxaData[grep("combine",ALL_data_sel$HowTheyUseCrossTaxaData)] <- "combine"
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "compared")] <- "compare"
# to lower
ALL_data_sel$HowTheyUseCrossTaxaData <- tolower (ALL_data_sel$HowTheyUseCrossTaxaData)
ALL_data_sel$HowTheyUseCrossTaxaData[which(ALL_data_sel$HowTheyUseCrossTaxaData == "compared")] <- "compare"

# use of phylogeny
ALL_data_sel$UsePhylogeny <- tolower (ALL_data_sel$UsePhylogeny )

## functional analogous traits
ALL_data_sel$FunctionallyAnalogousTraits <- tolower (ALL_data_sel$FunctionallyAnalogousTraits)
ALL_data_sel$FunctionallyAnalogousTraits[which(ALL_data_sel$FunctionallyAnalogousTraits == "yes (resource acquisition)")] <- "yes"
ALL_data_sel$FunctionallyAnalogousTraits[which(ALL_data_sel$FunctionallyAnalogousTraits == "partially")] <- "yes"

# phylogeny
phylo_use <- table(ALL_data_sel$UsePhylogeny,
                             ALL_data_sel$PaperNumber)
phylo_use <- data.frame (value=(rowSums(phylo_use>0)),use=c("No","Yes"))

# use phylo
plot5 <- ggplot (data= phylo_use, 
                 aes (fill=use,
                      y=value,
                      x=use))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("gray70","#E69F00"))+
  theme (axis.text.x = element_text(angle =0, 
                                    vjust = 1, 
                                    hjust=1),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "none"
  )+
  xlab("Use of phylogeny") +
  ylab ("Frequency") +
  labs(fill="N. studies")

# functionally analogous
func_anal <- table(ALL_data_sel$FunctionallyAnalogousTraits,
                   ALL_data_sel$PaperNumber)
func_anal <- data.frame (value=(rowSums(func_anal>0)),use=c("No","Yes"))

# use phylo
plot6 <- ggplot (data= func_anal, 
                 aes (fill=use,
                      y=value,
                      x=use))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("gray70","#E69F00"))+
  theme (axis.text.x = element_text(angle = 0, 
                                    vjust = 1, 
                                    hjust=1),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "none"
  )+
  xlab("Use of functionally analogous traits") +
  ylab ("Frequency") +
  labs(fill="N. studies")

# abundance weighted
ALL_data_sel$AbundanceWasUsed <- tolower (ALL_data_sel$AbundanceWasUsed )

ab_use <- table(ALL_data_sel$AbundanceWasUsed,
                   ALL_data_sel$PaperNumber)
ab_use_tab <- data.frame (value=(rowSums(ab_use>0)),use=c("No","Yes"))

# use phylo
plot7 <- ggplot (data= ab_use_tab, 
                 aes (fill=use,
                      y=value,
                      x=use))+
  geom_bar(position="stack",stat="identity")+
  scale_fill_manual(values=c("gray70","#E69F00"))+
  theme (axis.text.x = element_text(angle = 0, 
                                    vjust = 1, 
                                    hjust=1),
         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "none"
  )+
  xlab("Use of abundance") +
  ylab ("Frequency") +
  labs(fill="N. studies")


# abundance and FEve, FDis and FDiv
# lines with indexes
ALL_data_sel$weightedIndices <- ifelse (ALL_data_sel$uniqueIndex %in% c(
  "functional evenness","functional dispersal", "functional divergence",
  "functional composition", "trait convergence", "trait divergence",
  "functional beta diversity"),
  "Abudance Weighted Indices",
  "Other"
)
# table
ab_use_FD <- cast (formula = PaperNumber ~ weightedIndices,
                   value = "Realm",
                   data = ALL_data_sel)[,-1]
# transform
ab_use_FD <- ifelse (ab_use_FD>0,1,0)
# 
colSums(ab_use_FD)
table(rowSums(ab_use_FD)==2)

# abundance and indices weighted by abundance
table((ifelse (ab_use["yes",]>0,1,0) + ab_use_FD[,1]) ==2)

pdf(here ("output","approach"))
grid.arrange (plot6,
              plot5+theme(axis.title.y = element_blank()),
              plot7+theme(axis.title.y = element_blank()),
              ncol=3,nrow=2)
dev.off()


# ecolgical system
ALL_data_sel$EcologicalSystem<- gsub (" ","",tolower(ALL_data_sel$EcologicalSystem))
ecosystem <- table(ALL_data_sel$EcologicalSystem,
                   ALL_data_sel$PaperNumber)
ecosystem <- data.frame (value=(rowSums(ecosystem>0)))
ecosystem$system <- rownames(ecosystem)
ecosystem$value/sum(ecosystem$value)
# ------------------------------------------
# which traits

### relationship between traits and trait-based approaches
ALL_data_sel$VerbatinWhichTrait <- ALL_data_sel$WhichTrait
ALL_data_sel$WhichTrait <- tolower (ALL_data_sel$WhichTrait)

# diet
ALL_data_sel$WhichTrait [grep("diet", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("feeding", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("foraging", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("food", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("guild", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("trophic", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("herbivores", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("hypsodonty", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("predators", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("omnivores", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("detritiv*", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("tooth*", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("lower jaw morphometrics", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("bill lenght", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("setae*", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("photobiont*", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("oxygenic photosynthetic", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("jaw", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("microwear", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("buccolingual", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("curvature", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("enamel", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("mesiodistal", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("occlusal", (ALL_data_sel$WhichTrait))] <- "diet"
ALL_data_sel$WhichTrait [grep("lecty", (ALL_data_sel$WhichTrait))] <- "diet"

# habitat
ALL_data_sel$WhichTrait [grep("habitat", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("Habitat", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("shade*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("specializat*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("strata", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("light*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("stratum", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("soil position", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("disturbance", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("forest", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("aridity score", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("brachial*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("sensillus*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("humidity preference", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("substrate", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("sucessional*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("saproxylic*", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("water dependence", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("zonation", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("altitude", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("hydrologic", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("ph preference", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("plant parts eaten", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("plant parts and", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("hypogeic", (ALL_data_sel$WhichTrait))] <- "habitat"
ALL_data_sel$WhichTrait [grep("wetland use", (ALL_data_sel$WhichTrait))] <- "habitat"

# size/shape/weight/height
ALL_data_sel$WhichTrait [grep("size", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("weight", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("height", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("heigh", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("mass", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("basal area", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("volum*", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("length", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("body ratio", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("shape", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("thallus form", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("form", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("stem diameter", (ALL_data_sel$WhichTrait))] <- "size"
ALL_data_sel$WhichTrait [grep("intertegular", (ALL_data_sel$WhichTrait))] <- "size"

# reproduction / demography
ALL_data_sel$WhichTrait [grep("reproduction", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("nesting*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("nest*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("life*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("reproductive*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("asexual*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("prothallus*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("spore*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("fecundity", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("voltinism", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("*basidiomes", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("ascomata", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("breeding*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("fruit*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("galertic*", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("propagation predominant vegetative", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("seed bank", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("spawning preference", (ALL_data_sel$WhichTrait))] <- "reproduction"
ALL_data_sel$WhichTrait [grep("legumino", (ALL_data_sel$WhichTrait))] <- "reproduction"

# dispersal
ALL_data_sel$WhichTrait [grep("migrat*", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("dispersal", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("dispersal", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("balloning", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("ballooning", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("walk*", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("locomotion", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("*chorous", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("conidia", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("wing*", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("mobility", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("dissemination*", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("movement", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("aerial", (ALL_data_sel$WhichTrait))] <- "dispersal" # ?? aerial
ALL_data_sel$WhichTrait [grep("dispersed", (ALL_data_sel$WhichTrait))] <- "dispersal"
ALL_data_sel$WhichTrait [grep("dispersion syndrome", (ALL_data_sel$WhichTrait))] <- "dispersal"

# behavior
ALL_data_sel$WhichTrait [grep("activit*", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("overwinter strategy", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("home range", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("escape*", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("diurnal", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("mean laying date", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("active period", (ALL_data_sel$WhichTrait))] <- "behavior"
ALL_data_sel$WhichTrait [grep("adult active", (ALL_data_sel$WhichTrait))] <- "behavior"

# habit/ behavior
ALL_data_sel$WhichTrait [grep("semi-fossorial", (ALL_data_sel$WhichTrait))] <- "habit"
ALL_data_sel$WhichTrait [grep("sociability", (ALL_data_sel$WhichTrait))] <- "habit"
ALL_data_sel$WhichTrait [grep("sociality", (ALL_data_sel$WhichTrait))] <- "habit"
ALL_data_sel$WhichTrait [grep("hunting*", (ALL_data_sel$WhichTrait))] <- "habit"
ALL_data_sel$WhichTrait [grep("prey capture*", (ALL_data_sel$WhichTrait))] <- "habit"
ALL_data_sel$WhichTrait [grep("eye width", (ALL_data_sel$WhichTrait))] <- "habit"
ALL_data_sel$WhichTrait [grep("visual acuity", (ALL_data_sel$WhichTrait))] <- "habit"

# growth/energy/thermal tolerance
ALL_data_sel$WhichTrait [grep("growth*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("wood density", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("storage organs", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("trunk xylem density*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("leaf*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("foliar", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("temperature*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("*thermal*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("metabolism*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("climatic*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("niche*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("tolerance*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("moisture*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("humidity requirement", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("morphology*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("torpor", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("lateral spread", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("nitrogen-fixing", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("laminar surface area", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("perennial", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("number of claws", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("*root*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("hypha", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("foliag*", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("foliage persistance", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("cellulose percentage", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("vertical stratification", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("structural complexity", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("laminar total chlorophyll", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("fiber percentage", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("carbon percentage", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("crown width", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("flexibility", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("minimum dissolved oxygen", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("head fraction", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("plant functional type", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("colony type", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("ability to overgrow other colonies", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("nutrient uptake", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("ability to capture p", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("ability to degradate c", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("tiller", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("longevity", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("duration", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("larval development", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("stages", (ALL_data_sel$WhichTrait))] <- "growth"
ALL_data_sel$WhichTrait [grep("respiration", (ALL_data_sel$WhichTrait))] <- "growth"

# defense(primary and secondary)
ALL_data_sel$WhichTrait [grep("thorns", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("wood density", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("*compounds", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("color", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("color", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("defence", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("palatability", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("trichome", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("spine", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("resistance", (ALL_data_sel$WhichTrait))] <- "defense"
ALL_data_sel$WhichTrait [grep("mimicry", (ALL_data_sel$WhichTrait))] <- "defense"

# distribution
ALL_data_sel$WhichTrait [grep("geographic*", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("distribution*", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("red listed species", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("conservation status", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("origin", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("invasibility", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("endemism", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("rarity", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("native", (ALL_data_sel$WhichTrait))] <- "distribution"
ALL_data_sel$WhichTrait [grep("historic", (ALL_data_sel$WhichTrait))] <- "distribution"

# interaction
ALL_data_sel$WhichTrait [grep("*host*", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("competitive*", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("parasitoid*", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("flower visitor", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("flowering", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("pollinat*", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("social organization", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("pollen", (ALL_data_sel$WhichTrait))] <- "interactions"
ALL_data_sel$WhichTrait [grep("number of queens", (ALL_data_sel$WhichTrait))] <- "interactions"

#unique(ALL_data_sel$WhichTrait)[order(unique(ALL_data_sel$WhichTrait))]
#ALL_data_sel[which(ALL_data_sel$WhichTrait == "surface structure"),]

classes<-c("interactions","diet","habitat","habit","size","defense","distribution","growth","activity",
           "behavior","habit","dispersal","reproduction")
ALL_data_sel$WhichTrait[which(ALL_data_sel$WhichTrait %in% classes == F)] <- "other"

# what is in other
unique(ALL_data_sel$VerbatinWhichTrait[which(ALL_data_sel$WhichTrait == "other")] )

################################################
# rm missing data
ALL_data_sel <- ALL_data_sel [is.na(ALL_data_sel$HowTheyUseCrossTaxaData)!=T,]

# number of papers per approach
colSums(table(ALL_data_sel$PaperNumber,
      ALL_data_sel$HowTheyUseCrossTaxaData)>0)
rowSums(table(ALL_data_sel$PaperNumber,
              ALL_data_sel$HowTheyUseCrossTaxaData)>0) # only one approach per article

# per trait
colSums(table(ALL_data_sel$PaperNumber,
              ALL_data_sel$WhichTrait)>0) # only one approach per article

# tab
approach_trait <- cast (formula = PaperNumber ~ HowTheyUseCrossTaxaData+WhichTrait,
    
                          data = ALL_data_sel)

# transforming
approach_trait<- data.frame(freq=colSums(ifelse (approach_trait[,-1]>0,1,0)))
approach_trait$approach <- sapply (strsplit (rownames(approach_trait),"_"), "[",1)
approach_trait$trait <- sapply (strsplit (rownames(approach_trait),"_"), "[",2)

# melt this df to long format
df_approach_trait <- melt(approach_trait,
                          variable ="WhichTrait", 
                          as.is=T)

# standardize
df_approach_trait[which(df_approach_trait$approach == "combine"),"value"] <- df_approach_trait[which(df_approach_trait$approach == "combine"),"value"]/sum(df_approach_trait[which(df_approach_trait$approach == "combine"),"value"])*100
df_approach_trait[which(df_approach_trait$approach == "compare"),"value"] <- df_approach_trait[which(df_approach_trait$approach == "compare"),"value"]/sum(df_approach_trait[which(df_approach_trait$approach == "compare"),"value"])*100
df_approach_trait[which(df_approach_trait$approach == "both"),"value"] <- df_approach_trait[which(df_approach_trait$approach == "both"),"value"]/sum(df_approach_trait[which(df_approach_trait$approach == "both"),"value"])*100
# order of traits
set_order <- aggregate(df_approach_trait$value,by=list(df_approach_trait$trait),FUN="sum")
set_order <- set_order [order(set_order$x,decreasing=T),]

# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(
  name=c(as.character(df_approach_trait$approach), 
         as.character(set_order$Group.1)) %>% unique()
)

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
df_approach_trait$IDsource <- match(df_approach_trait$approach, 
                                    nodes$name)-1 
df_approach_trait$IDtarget <- match(df_approach_trait$trait, 
                                      nodes$name)-1


plot8 <- sankeyNetwork(Links = df_approach_trait, 
                   Nodes = nodes,
                   Source = "IDsource", 
                   Target = "IDtarget",
                   Value = "value", 
                   NodeID = "name", 
                   sinksRight=F,
                   fontSize= 20, 
                   nodeWidth = 40
                   )

# save the widget
library(htmlwidgets)
saveWidget(plot8, file=here ("output","sankeyDiagram.html"))

# papers per approach
papers_approach <- cast (formula = PaperNumber ~ HowTheyUseCrossTaxaData,
      
      data = ALL_data_sel)[,-1]

# transf
(colSums (ifelse (papers_approach>0,1,0)))

# ----------------------------------------
# relatinship between PD, N taxa and N traits

list_papers <- unique(ALL_data_sel$PaperNumber)

ct_taxa_traits <- lapply (list_papers, function (i) # apply to each study
  data.frame (
  # count the number of taxa
  Ntaxa=length(unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"WhichOrganism"])),
  # count the number of traits
  Ntraits=length(unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"WhichTrait"])),
  # mean pairwise phylo distance between taxa
  mpd = mean (as.dist (cophenetic(phylo_taxa)[which(phylo_taxa$tip.label %in% unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"TaxaUpperLevel"])),
                                              which(phylo_taxa$tip.label %in% unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"TaxaUpperLevel"]))]))
  
  )
  )

# melt
ct_taxa_traits<-do.call(rbind, ct_taxa_traits)
# summary statistics
summary(ct_taxa_traits)
# finding outliers
ct_taxa_traits_outliers <- ct_taxa_traits[which(ct_taxa_traits$Ntaxa<15),] # outliers
ct_taxa_traits<-rbind (data.frame (ct_taxa_traits, # bind into the df
             KeepOutliers = T),
       data.frame (ct_taxa_traits_outliers,
                   KeepOutliers = F))
ct_taxa_traits$KeepOutliers<-factor (ct_taxa_traits$KeepOutliers,
                                 levels = c("FALSE",
                                            "TRUE"))

# test of diff models
m1 <- gam (Ntraits~s(mpd),
           data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
           family="poisson")
#m1 <- glm (Ntraits~Ntaxa,data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
#           family="poisson")
#m2 <- glm (Ntraits~poly(Ntaxa,2),data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),],
#           family="poisson")
#m3 <- glm (Ntraits~poly(Ntaxa,3),data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
#           family="poisson")
#require(MuMIn)
#model.sel(m1,m2,m3)

# poison smooth 
poison_smooth <- function(...) {
  geom_smooth(method = "gam", method.args = list(family = "poisson"), ...)
  # geom_smooth(method = "glm", method.args = list(family = "poisson"), ...) # glm option
}

p1<-ggplot(ct_taxa_traits, aes (x=Ntaxa,
                            y=Ntraits,
                            group= KeepOutliers,
                            fill = KeepOutliers)) + 
  geom_point(size=3,alpha = 0.2) + 
  poison_smooth(
    formula = y ~ s(x),
    #formula = y ~ splines::ns(x, 3),
    se=T,
    size=1,
    colour = "black",
    alpha=0.3) + theme_classic()+  
  scale_x_continuous(name="Number of taxa per study", limits=c(0, 30)) +
  scale_y_continuous(name="Number of traits per study", limits=c(0, 10))+ 
  theme(axis.title = element_text(size=16),
        legend.position = "none") +
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black"))

p1<-p1 + ggplot2::annotate("text", x = 25, y = 10, label = "n=96",fontface = 'italic')

# MPD
# test of diff models
m1 <- gam (Ntraits~s(mpd),
           data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
                       family="poisson")
#m1 <- glm (Ntraits~mpd,data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F & is.nan(ct_taxa_traits$mpd)!= T),], 
#           family="poisson")
#m2 <- glm (Ntraits~poly(Ntaxa,2),data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F & is.nan(ct_taxa_traits$mpd)!= T),], 
#           family="poisson")
#m3 <- glm (Ntraits~poly(Ntaxa,3),ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F & is.nan(ct_taxa_traits$mpd)!= T),], 
#           family="poisson")
#require(MuMIn)
#model.sel(m1,m2,m3)

p2<-ggplot(ct_taxa_traits, aes (x=mpd,
                            y=Ntraits,
                            group = KeepOutliers,
                            fill = KeepOutliers)) +
  geom_point(size=3,alpha = 0.2) + 
  poison_smooth(
    formula = y ~ s(x),
    #formula = y ~ splines::ns(x, 2),
    se=T,
    size=1,
    colour = "black",
    alpha=0.3) + theme_classic()+ 
  scale_x_continuous(name="MPD between taxa within a study", limits=c(0, 1)) +
  scale_y_continuous(name="Number of traits per study", limits=c(0, 10))+ 
  theme(axis.title.y  = element_blank(),
        axis.title.x = element_text(size=16),
        legend.position = c(0.2,0.9))+
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black"))
p2<-p2 + ggplot2::annotate("text", x = 0.75, y = 10, label = "n=94",fontface = 'italic')

png(here ("output", "mpd_Ntaxa_traits"),
    res = 300,units = "px",width=2500,height=1500)
grid.arrange(p1,p2,
             ncol =2)
dev.off()

pdf (here ("output", "mpd_Ntaxa_traitspdf"),height=4,width=9)
grid.arrange(p1,p2,
             ncol =2)
dev.off()


# randomly remove one trait category in one observation

set.seed(1234)

# 10% of the dataset

# randomly remove one trait category in one observation
set.seed(1234)
data_to_randomize <- ct_taxa_traits
test1 <- lapply (seq (1,100), function (i){
  
  test_data <- sample (nrow (ct_taxa_traits),nrow (ct_taxa_traits)*0.1) # from this proportion of the dataset
  data_to_randomize [test_data, "Ntraits"]<-data_to_randomize[test_data, "Ntraits"]-1 # subtract one trait
  data_to_randomize$randomization <- i
  data_to_randomize<-data_to_randomize[which(data_to_randomize$Ntraits>0),]
  data_to_randomize
})
test1<-(do.call (rbind,test1))


# plot
ggplot(test1, aes (x=Ntaxa,
                   y=Ntraits,
                   group = randomization)) +
  
  facet_wrap(~KeepOutliers)+
  geom_point(data=test1 [which(test1$randomization==1),], 
             mapping = aes (x=Ntaxa,
                            y=Ntraits),size=2,alpha = 0.2) + 
  poison_smooth(
    formula = y ~ s(x),
    #formula = y ~ splines::ns(x, 2),
    se=F,
    size=0.5,
    colour="black") 


# mpd

ggplot(test1, aes (x=mpd,
                   y=Ntraits,
                   group = randomization)) +
  
  facet_wrap(~KeepOutliers)+
  geom_point(data=test1 [which(test1$randomization==1),], 
             mapping = aes (x=mpd,
                            y=Ntraits),size=2,alpha = 0.2) + 
  poison_smooth(
    formula = y ~ s(x),
    #formula = y ~ splines::ns(x, 2),
    se=F,
    size=0.5,
    colour="black") 




## 20% of the dataset


data_to_randomize <- ct_taxa_traits
test1 <- lapply (seq (1,100), function (i){
  
  test_data <- sample (nrow (ct_taxa_traits),nrow (ct_taxa_traits)*0.2) # from this proportion of the dataset
  data_to_randomize [test_data, "Ntraits"]<-data_to_randomize[test_data, "Ntraits"]-1 # subtract one trait category
  data_to_randomize$randomization <- i
  data_to_randomize<-data_to_randomize[which(data_to_randomize$Ntraits>0),]
  data_to_randomize
})
test1<-(do.call (rbind,test1))


# plot
ggplot(test1, aes (x=Ntaxa,
                            y=Ntraits,
                            group = randomization)) +
  
  facet_wrap(~KeepOutliers)+
  geom_point(data=test1 [which(test1$randomization==1),], 
             mapping = aes (x=Ntaxa,
                            y=Ntraits),size=2,alpha = 0.2) + 
  poison_smooth(
    formula = y ~ s(x),
    #formula = y ~ splines::ns(x, 2),
    se=F,
    size=0.5,
    colour="black") 


# mpd

ggplot(test1, aes (x=mpd,
                   y=Ntraits,
                   group = randomization)) +
  
  facet_wrap(~KeepOutliers)+
  geom_point(data=test1 [which(test1$randomization==1),], 
             mapping = aes (x=mpd,
                            y=Ntraits),size=2,alpha = 0.2) + 
  poison_smooth(
    formula = y ~ s(x),
    #formula = y ~ splines::ns(x, 2),
    se=F,
    size=0.5,
    colour="black") 

