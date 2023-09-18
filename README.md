Going across taxa in functional ecology: review and perspectives of an
emerging field
================
Reef Synthesis Working Group - ReefSYN
(September, 2023)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

Brief description of the folders in this repository:

### Root

##### \|

##### \|– data: contains the raw data. It includes the life classification by Ruggiero et al. (2014), the preliminary filtering of articles (file “Pooled_answers_Filtering_articles_with_questions.xlsx”), and raw data from readers (folder “from_readers”).

##### \|

##### \|– processed_data: data processed after running the Rscripts

##### \|

##### \|– output: figures and tables.

##### \|

##### \|– R: scripts of R used in data analyses. Each script comprises one step of analysis. —- \| – adjustment.R: taxonomic adjustment. This script is called in the “Script1_networkAnalysis.R”

\| — functions.R: useful functions

\| — packages.R: required packages

\| — Script1_networkAnalysis.R: R script used to edit the data and
create the bipartite network shown in Fig. 2. The script will produce
figures and the processed data “ALL_data_sel.RData”. Also the script
uses taxa names cited in the reviewed research to obtain their taxonomic
ranks using functions of ‘taxize’ package (processed data:
“ranks.RData”).

\| — Script2_wholeTree.R: R script used to edit the data and create the
taxonomic tree shown in Fig. 3. The script calls the processed data
“ALL_data_sel.RData” and the classification of Ruggiero et al. (2014)
hosted in the folder ‘data’. The figure 3 was edited in Inkscape v1.
This script did not update the processed data “ALL_data_sel.RData”. The
script uses taxa names cited in Ruggiero et al. (2014) to obtain their
taxonomic ranks using functions of ‘taxize’ package (processed data:
“whole_class_taxa.RData”).

\| — Script3_metanetworkAnalyses.R: R script used to edit the data and
create the metanetwork. The script calls the processed data
“ALL_data_sel.RData” to produce the metanetwork and estimate network
structure statistics (e.g., degree statistics).

\| — Script4_histogarmSankey.R: R script used to edit the data and
create the Figs 1 and 4. The script calls the processed data
“ALL_data_sel.RData” to produce the histograms and Sankey plot. Several
changes on variables needed to analyses were made in the dataset
“ALL_data_sel.RData”. At the end of the script we updated the data to
processed data “ALL_data_sel_processed_data.RData”.

\| — Script5_GAM: Script used to run Generalized Additive Models and
sensitivity analysis (Fig. 5). It uses the processed data
“ALL_data_sel_processed_data.RData”.

#### Tree topology of taxa within multiple taxa research in functional ecology (orange branches).

##### We assessed the coverage of the reviewed research across the Tree of Life using the classification of Ruggiero et al. (2015) to all organisms on Earth. The phylogeny was based on the taxonomic ranks of studied organisms. Taxonomic nomenclature follows the NCBI (National Center for Biotechnology Information), and is above Subclass level. As some studies have identified varying taxonomic levels for the same organism (e.g., Acari, Arachnida), we have opted to display all taxonomic ranks instead of solely showcasing the higher-level rank as the phylogeny’s edges. Silhouettes were obtained from Google images.

<img src="phylo.png" width="100%" height="80%" style="display: block; margin: auto;" />

#### This paper was produced using the following software and associated packages:

    ## R version 4.3.0 (2023-04-21 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19045)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## time zone: America/Sao_Paulo
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] mgcv_1.8-42          nlme_3.1-162         bipartite_2.18      
    ##  [4] sna_2.7-1            network_1.18.1       statnet.common_4.9.0
    ##  [7] igraph_1.4.2         phytools_1.5-1       maps_3.4.1          
    ## [10] vegan_2.6-4          lattice_0.21-8       permute_0.9-7       
    ## [13] networkD3_0.4        rbokeh_0.5.2         gridExtra_2.3       
    ## [16] viridis_0.6.3        viridisLite_0.4.2    wordcloud_2.6       
    ## [19] RColorBrewer_1.1-3   SnowballC_0.7.1      tm_0.7-11           
    ## [22] NLP_0.2-1            taxize_0.9.100       ape_5.7-1           
    ## [25] rotl_3.1.0           taxa_0.4.2           vctrs_0.6.2         
    ## [28] lubridate_1.9.2      forcats_1.0.0        stringr_1.5.0       
    ## [31] dplyr_1.1.2          purrr_1.0.1          readr_2.1.4         
    ## [34] tidyr_1.3.0          tibble_3.2.1         ggplot2_3.4.2       
    ## [37] tidyverse_2.0.0      reshape_0.8.9        openxlsx_4.2.5.2    
    ## [40] here_1.0.1          
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] mnormt_2.1.1            phangorn_2.11.1         rlang_1.1.1            
    ##  [4] magrittr_2.0.3          compiler_4.3.0          combinat_0.0-8         
    ##  [7] quadprog_1.5-8          httpcode_0.3.0          pkgconfig_2.0.3        
    ## [10] crayon_1.5.2            fastmap_1.1.1           utf8_1.2.3             
    ## [13] rmarkdown_2.21          tzdb_0.4.0              xfun_0.39              
    ## [16] clusterGeneration_1.3.7 jsonlite_1.8.4          progress_1.2.2         
    ## [19] gistr_0.9.0             highr_0.10              uuid_1.1-0             
    ## [22] pryr_0.1.6              parallel_4.3.0          prettyunits_1.1.1      
    ## [25] cluster_2.1.4           R6_2.5.1                stringi_1.7.12         
    ## [28] numDeriv_2016.8-1.1     Rcpp_1.0.10             assertthat_0.2.1       
    ## [31] iterators_1.0.14        knitr_1.42              fields_14.1            
    ## [34] optimParallel_1.0-2     zoo_1.8-12              rentrez_1.2.3          
    ## [37] Matrix_1.6-1            splines_4.3.0           timechange_0.2.0       
    ## [40] tidyselect_1.2.0        rstudioapi_0.14         yaml_2.3.7             
    ## [43] doParallel_1.0.17       codetools_0.2-19        curl_5.0.0             
    ## [46] plyr_1.8.8              withr_2.5.0             coda_0.19-4            
    ## [49] evaluate_0.21           zip_2.3.0               xml2_1.3.4             
    ## [52] pillar_1.9.0            foreach_1.5.2           generics_0.1.3         
    ## [55] rprojroot_2.0.3         hms_1.1.3               munsell_0.5.0          
    ## [58] scales_1.2.1            rncl_0.8.7              glue_1.6.2             
    ## [61] slam_0.1-50             scatterplot3d_0.3-44    lazyeval_0.2.2         
    ## [64] tools_4.3.0             hexbin_1.28.3           data.table_1.14.8      
    ## [67] dotCall64_1.0-2         XML_3.99-0.14           fastmatch_1.1-3        
    ## [70] grid_4.3.0              plotrix_3.8-2           bold_1.3.0             
    ## [73] colorspace_2.1-0        conditionz_0.1.0        cli_3.6.1              
    ## [76] spam_2.9-1              fansi_1.0.4             expm_0.999-7           
    ## [79] gtable_0.3.3            digest_0.6.31           crul_1.3               
    ## [82] htmlwidgets_1.6.2       htmltools_0.5.5         lifecycle_1.0.3        
    ## [85] httr_1.4.6              MASS_7.3-58.4
