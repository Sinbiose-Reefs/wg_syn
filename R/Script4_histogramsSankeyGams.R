



##  --------------------------------------  ## 

#      Text analyses and figures
#       figs 4 and 5, Sankey and GAMs


##  --------------------------------------  ## 



# load packages and functions
source("R/packages.R")
source("R/functions.R")


# load review data
load(here("output", "ALL_data_sel.RData"))




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

ResearchContext[order(ResearchContext$freq,decreasing = T),]
write.csv (ResearchContext, file = here ("output","ResearchContext.csv"))

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
# relationship between PD, N taxa and N traits

list_papers <- unique(ALL_data_sel$PaperNumber)

ct_taxa_traits <- lapply (list_papers, function (i) # apply to each study
  data.frame (
  # count the number of taxa
  Ntaxa=length(unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"WhichOrganism"])),
  
  # count the number of trait categories
  Ntraits=length(unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"WhichTrait"])),
  
  # count the number of trait categories
  NtraitsRaw=length(unique(ALL_data_sel[which(ALL_data_sel$PaperNumber == i),"VerbatinWhichTrait"])),
  
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

# test 
m1 <- gam (Ntraits~s(mpd),
           data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
           family="poisson")

# plot
p1.a<-ggplot(ct_taxa_traits, aes (x=Ntaxa,
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
  scale_x_continuous(name="Number of taxonomic ranks per study", limits=c(0, 30)) +
  scale_y_continuous(name="Number of trait categories per study", limits=c(0, 10))+ 
  ggtitle ("A")+
  theme(axis.title = element_text(size=16),
        legend.position = "none",
        plot.title = element_text(size=35,face="bold")) +
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black")) 

p1.a<-p1.a + ggplot2::annotate("text", x = 25, y = 10, 
                               label = "n=96",fontface = 'italic')

# MPD
# test of diff models
m1 <- gam (Ntraits~s(mpd),
           data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
                       family="poisson")

p2.a<-ggplot(ct_taxa_traits, aes (x=mpd,
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
  ggtitle("B")+
  scale_x_continuous(name="MPD between taxa within a study", limits=c(0, 1)) +
  scale_y_continuous(name="Number of trait categories per study", limits=c(0, 10))+ 
  theme(plot.title = element_text(size=35,face="bold"),
          axis.title.y  = element_blank(),
        axis.title.x = element_text(size=16),
        legend.position = c(0.2,0.9))+
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black"))
p2.a<-p2.a + ggplot2::annotate("text", x = 0.75, y = 10, label = "n=94",fontface = 'italic')


png(here ("output", "mpd_Ntaxa_traits"),
    res = 300,units = "cm",width=30,height=15)

grid.arrange(p1.a,p2.a,
             ncol =2,nrow=1)

dev.off()




# the raw number of  traits


# test 
m1 <- gam (NtraitsRaw~s(mpd),
           data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
           family="poisson")

# plot
p1<-ggplot(ct_taxa_traits, aes (x=Ntaxa,
                                y=NtraitsRaw,
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
  scale_x_continuous(name="Number of taxonomic ranks per study", limits=c(0, 30)) +
  scale_y_continuous(name="Number of traits per study", limits=c(0, 20))+ 
  theme(axis.title = element_text(size=16),
        legend.position = "none") +
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black"))

p1<-p1 + ggplot2::annotate("text", x = 25, y = 10, label = "n=96",fontface = 'italic')

# MPD
# test of diff models
m1 <- gam (NtraitsRaw~s(mpd),
           data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
           family="poisson")

p2<-ggplot(ct_taxa_traits, aes (x=mpd,
                                y=NtraitsRaw,
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
  scale_y_continuous(name="Number of traits per study", limits=c(0, 20))+ 
  theme(axis.title.y  = element_blank(),
        axis.title.x = element_text(size=16),
        legend.position = c(0.2,0.9))+
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black"))
p2<-p2 + ggplot2::annotate("text", x = 0.75, y = 10, label = "n=94",fontface = 'italic')

png(here ("output", "mpd_Ntaxa_RawTraits",height=4,width=9),
    res = 300,units = "px",width=2500,height=1500)

grid.arrange(p1,p2,
             ncol =2,nrow=1)

dev.off()





# randomly remove one trait category in one observation


# 10% of the dataset

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
p1a<-ggplot(test1, aes (x=Ntaxa,
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
    colour="#DC5F00") +
  theme_classic()


# mpd

p1b<-ggplot(test1, aes (x=mpd,
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
    colour="#DC5F00") +
  theme_classic()




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
p1c<-ggplot(test1, aes (x=Ntaxa,
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
    colour="#DC5F00") +
  theme_classic()


# mpd

p1d<-ggplot(test1, aes (x=mpd,
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
    colour="#DC5F00") +
  theme_classic()


# arrange 

png(file=here ("output", "sens_randomiz"),height=12,width=20,units = "cm",res=300)
grid.arrange(p1a+xlab ("Number of taxonomic ranks") + ylab ("Number of trait categories"),
             p1b+xlab ("Mean Pairwise Distance between taxa") + ylab (""),
             p1c+xlab ("Number of taxonomic ranks") + ylab ("Number of trait categories"),
             p1d+xlab ("Mean Pairwise Distance between taxa") + ylab (""),
             ncol =2,nrow=2)

dev.off()


# end

rm (list=ls())