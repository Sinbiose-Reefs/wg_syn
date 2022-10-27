


##  --------------------------------------  ## 

#     Meta Network analyses and figures
# fig 2, interaction networks 


##  --------------------------------------  ## 



# load packages
source("R/packages.R")
source("R/functions.R")
# save the dataset

load (here ("output", "ALL_data_sel.RData"))



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
# links
comb_edges <-comb_edges[which (comb_edges$weight >0),]


# need a complete ranking of taxa

load (file = here ("output", "ranks.RData"))
class_taxa_data <- class_taxa
load (file = here ("output", "whole_class_taxa.RData"))
class_taxa_whole <- class_taxa


# bind data
class_taxa_all <- c(class_taxa_whole,
                    class_taxa_data)

# rm unclassified taxa
class_taxa_all<- class_taxa_all [which(lapply (class_taxa_all,length)>1)]
class_taxa_df<- do.call(rbind, class_taxa_all)


# the nodes
list_names<- unique(c(comb_edges$t1,comb_edges$t2))

# have broad groups
ranks_df <- lapply (list_names, function (k) {
  
  select_this <- class_taxa_all [which(
    unlist(
      lapply (class_taxa_all, function(i)
        
        sum(i$name %in% k) ))>0)]  # find the taxon
  select_this <- select_this[[1]]
  select_this <- select_this$name [grep ("*kingdom*",select_this$rank)]
  select_this <- if (length(select_this) == 1) {
    select_this
  } else {
    
    select_this[length(select_this)]
    
  }
  
  # result
  res <- data.frame (name = k,
                     taxon =select_this)
  res
  
})
ranks_df<- do.call(rbind, ranks_df)   # melt

# match to have the groups

test <- ranks_df
colnames(test) <- c("name", "group")


# adjust
colnames(comb_edges) <- c("source", "target", "value")

# try numbers
numbers_to_match <- data.frame (name = unique(c(comb_edges$source, comb_edges$target)),
                                number = as.numeric(as.factor(unique(c(comb_edges$source, comb_edges$target))))-1)

# match 

comb_edges$source1 <- numbers_to_match$number[match (comb_edges$source, numbers_to_match$name)]
comb_edges$target1 <- numbers_to_match$number[match (comb_edges$target, numbers_to_match$name)]



forceNetwork(Links = comb_edges,
             Nodes = test,
             Source = "source1",
             Target = "target1",
             Value = "value",
             NodeID = "name", 
             Group = "group",
             opacity = 0.7, 
             zoom = F,
             legend = TRUE,
             arrows = T,
             bounded = T)


#igraph
g <- graph.data.frame(comb_edges[,1:3], 
                      directed = F)

#V(g)$type <- V(g)$name %in% comb_edges[,2] #the second column of edges is TRUE type
E(g)$weight <- as.numeric(comb_edges$value)
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

pdf (here ("output", "net_taxa"))
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
plotModuleWeb(m_network,labsize = 0.2)
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






# end
rm(list=ls())



