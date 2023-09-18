

##  --------------------------------------  ## 

#           GAMs


##  --------------------------------------  ## 



# load packages and functions
source("R/packages.R")
source("R/functions.R")

# load the processed dataset at the end of analysis
load(here ("processed_data", "ALL_data_sel.RData")); rm(ALL_data_sel)
load (here ("processed_data", "ALL_data_sel_processed_data.RData"))



# ----------------------------------------
# relationship between PD, N taxa and N traits



list_papers <- unique(ALL_data_sel$PaperNumber)

ALL_data_sel [grep ("Gwinn, DC; Middleton",ALL_data_sel$StudyAuthors),"StudyAuthors"]

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
summary(m1 <- gam (Ntraits~s(Ntaxa),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
                   family="poisson"))
# test 
summary(m1 <- gam (Ntraits~s(mpd),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
                   family="poisson"))

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
    alpha=0.5) + theme_classic()+  
  scale_x_continuous(name="Number of taxonomic ranks per study", limits=c(0, 30)) +
  scale_y_continuous(name="Number of trait types per study", limits=c(0, 10))+ 
  ggtitle ("A")+
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size=14),
        legend.position = "none",
        plot.title = element_text(size=35,face="bold")) +
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "gray")) 

p1.a<-p1.a + 
  ggplot2::annotate("text", x = 25, y = 10, label = "AdjustedR2=0.07**/0.11***",fontface = 'italic')+
  ggplot2::annotate("text", x = 25, y = 9.5, label = "edf=4.68/6.85",fontface = 'italic')+
  ggplot2::annotate("text", x = 25, y = 9, label = "DE=13.3%/20.3%",fontface = 'italic')+
  ggplot2::annotate("text", x = 25, y = 8.5, label = "n=96",fontface = 'italic')
  

# MPD
# test of diff models
summary(m1 <- gam (Ntraits~s(Ntaxa),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
                   family="poisson"))
summary(m1 <- gam (Ntraits~s(mpd),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
                   family="poisson"))


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
    alpha=0.5) + theme_classic()+ 
  ggtitle("B")+
  scale_x_continuous(name="MPD between taxa within a study", limits=c(0, 1)) +
  scale_y_continuous(name="Number of trait types per study", limits=c(0, 10))+ 
  theme(plot.title = element_text(size=35,face="bold"),
        axis.title.y  = element_blank(),
        axis.title.x = element_text(size=16),
        axis.text = element_text(size=14),
        legend.position = c(0.2,0.9))+
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "gray"))
p2.a<-p2.a + 
  ggplot2::annotate("text", x = 0.75, y = 10, label = "AdjustedR2=0.04./0.4.",fontface = 'italic')+
  ggplot2::annotate("text", x = 0.75, y = 9.5, label = "edf=3.86/3.39",fontface = 'italic')+
  ggplot2::annotate("text", x = 0.75, y = 9, label = "DE=9.88%/8.99%",fontface = 'italic')+
  ggplot2::annotate("text", x = 0.75, y = 8.5, label = "n=94",fontface = 'italic')


pdf(here ("output","figs", "fig5.pdf"),
    width=12,height=6)

grid.arrange(p1.a,p2.a,
             ncol =2,nrow=1)

dev.off()




# the raw number of  traits


# test 
summary(m1 <- gam (NtraitsRaw~s(Ntaxa),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
                   family="poisson"))
summary(m1 <- gam (NtraitsRaw~s(mpd),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==T),], 
                   family="poisson"))

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

p1<-p1 + 
  ggplot2::annotate("text", x = 25, y = 20, label = "AdjustedR2=0.03***/0.06***",fontface = 'italic')+
  ggplot2::annotate("text", x = 25, y = 19, label = "edf=6.23/8.26",fontface = 'italic')+
  ggplot2::annotate("text", x = 25, y = 18, label = "DE=14.8%/21.6%",fontface = 'italic')+
  ggplot2::annotate("text", x = 25, y = 17, label = "n=96",fontface = 'italic')


# MPD
# test of diff models
summary(m1 <- gam (NtraitsRaw~s(Ntaxa),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
                   family="poisson"))
summary(m1 <- gam (NtraitsRaw~s(mpd),
                   data=ct_taxa_traits[which(ct_taxa_traits$KeepOutliers==F),], 
                   family="poisson"))

# plot MPD
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
  scale_y_continuous(name="Number of trait types per study", limits=c(0, 20))+ 
  theme(axis.title.y  = element_blank(),
        axis.title.x = element_text(size=16),
        legend.position = c(0.2,0.9))+
  scale_fill_manual(
    values = c("FALSE" = "orange",
               "TRUE" = "black"))
p2<-p2 +
  ggplot2::annotate("text", x = 0.75, y = 20, label = "AdjustedR2=-0.05***/-0.06***",fontface = 'italic')+
  ggplot2::annotate("text", x = 0.75, y = 19, label = "edf=7.92/8.00",fontface = 'italic')+
  ggplot2::annotate("text", x = 0.75, y = 18, label = "DE=10.2%/10.3%",fontface = 'italic')+
  ggplot2::annotate("text", x = 0.75, y = 17, label = "n=94",fontface = 'italic')


# save
pdf(here ("output", "figs","figS1.5.pdf"),
    width=12,height=6)

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

# test R2 of each simulated dataset
rd_test_keep <- lapply (test1, function (i) {
  
  
  # test 
  summary(m1NT <- gam (Ntraits~s(Ntaxa),
                       data=i[which(i$KeepOutliers == T),], 
                       family="poisson"))
  # test 
  summary(m1MPD <- gam (Ntraits~s(mpd),
                        data=i[which(i$KeepOutliers == T),], 
                        family="poisson"))
  
  df_res <- data.frame (nt=summary(m1NT)$r.sq, 
                        mpd=summary(m1MPD)$r.sq)
  
  df_res
  
})

apply (do.call(rbind, rd_test_keep),2,mean)

# remove outliers
# test R2 of each simulated dataset
rd_test_rmout <- lapply (test1, function (i) {
  
  
  # test 
  summary(m1NT <- gam (Ntraits~s(Ntaxa),
                       data=i[which(i$KeepOutliers == F),], 
                       family="poisson"))
  # test 
  summary(m1MPD <- gam (Ntraits~s(mpd),
                        data=i[which(i$KeepOutliers == F),], 
                        family="poisson"))
  
  df_res <- data.frame (nt=summary(m1NT)$r.sq, 
                        mpd=summary(m1MPD)$r.sq)
  
  df_res
  
})

apply (do.call(rbind, rd_test_rmout),2,mean)




# melt the data to plot
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
  theme_classic() + 
  xlab ("Number of taxonomic ranks per study")+
  ylab ("Number of trait types")


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
  theme_classic()+
  xlab ("MPD between taxa within a study")+
  ylab ("Number of trait types")



png(here ("output","figs", "figS1.6.png"),
    width=20,height=10,units="cm",res=300)

grid.arrange(p1a,p1b,
             ncol =2,nrow=1)

dev.off()



## 20% of the dataset

data_to_randomize <- ct_taxa_traits
test1 <- lapply (seq (1,100), function (i){
  
  test_data <- sample (nrow (ct_taxa_traits),nrow (ct_taxa_traits)*0.2) # from this proportion of the dataset
  data_to_randomize [test_data, "Ntraits"]<-data_to_randomize[test_data, "Ntraits"]-1 # subtract one trait category
  data_to_randomize$randomization <- i
  data_to_randomize<-data_to_randomize[which(data_to_randomize$Ntraits>0),]
  data_to_randomize
})

# test R2 of each simulated dataset
rd_test_keep <- lapply (test1, function (i) {
  
  
  # test 
  summary(m1NT <- gam (Ntraits~s(Ntaxa),
                       data=i[which(i$KeepOutliers == T),], 
                       family="poisson"))
  # test 
  summary(m1MPD <- gam (Ntraits~s(mpd),
                        data=i[which(i$KeepOutliers == T),], 
                        family="poisson"))
  
  df_res <- data.frame (nt=summary(m1NT)$r.sq, 
                        mpd=summary(m1MPD)$r.sq)
  
  df_res
  
})

apply (do.call(rbind, rd_test_keep),2,mean)

# remove outliers
# test R2 of each simulated dataset
rd_test_rmout <- lapply (test1, function (i) {
  
  
  # test 
  summary(m1NT <- gam (Ntraits~s(Ntaxa),
                       data=i[which(i$KeepOutliers == F),], 
                       family="poisson"))
  # test 
  summary(m1MPD <- gam (Ntraits~s(mpd),
                        data=i[which(i$KeepOutliers == F),], 
                        family="poisson"))
  
  df_res <- data.frame (nt=summary(m1NT)$r.sq, 
                        mpd=summary(m1MPD)$r.sq)
  
  df_res
  
})
apply (do.call(rbind, rd_test_rmout),2,mean)

# melt to plot
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
  theme_classic() + 
  xlab ("Number of taxonomic ranks per study")+
  ylab ("Number of trait types")


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
  theme_classic() +
  xlab ("MPD between taxa within a study")+
  ylab ("Number of trait types")


# save plot
png(here ("output","figs", "figS1.7.png"),
    width=20,height=10,units="cm",res=300)

grid.arrange(p1c,p1d,
             ncol =2,nrow=1)

dev.off()

# end

rm (list=ls())

