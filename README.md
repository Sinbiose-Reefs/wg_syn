Going across taxa in functional ecology: review and perspectives of an
emerging field
================
Reef Synthesis Working Group - ReefSYN
(March, 2023)

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->
<!-- badges: end -->

#### Tree topology of taxa within multiple taxa research in functional ecology (orange branches).

##### The phylogeny was based on the taxonomic ranks of studied organisms. Taxonomic nomenclature follows the NCBI (National Center for Biotechnology Information), and is above Subclass level. As some studies have identified varying taxonomic levels for the same organism (e.g., Acari, Arachnida), we have opted to display all taxonomic ranks instead of solely showcasing the higher-level rank as the phylogeny’s edges. Silhouettes were obtained from Google images.

<img src="phylo.png" width="100%" height="80%" style="display: block; margin: auto;" />

#### This paper was produced using the following software and associated packages:

    ## R version 4.2.2 (2022-10-31 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 19044)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=Portuguese_Brazil.utf8  LC_CTYPE=Portuguese_Brazil.utf8   
    ## [3] LC_MONETARY=Portuguese_Brazil.utf8 LC_NUMERIC=C                      
    ## [5] LC_TIME=Portuguese_Brazil.utf8    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] mgcv_1.8-41          nlme_3.1-160         bipartite_2.18      
    ##  [4] sna_2.7-1            network_1.18.1       statnet.common_4.8.0
    ##  [7] igraph_1.3.5         phytools_1.2-0       maps_3.4.1          
    ## [10] vegan_2.6-4          lattice_0.20-45      permute_0.9-7       
    ## [13] networkD3_0.4        rbokeh_0.5.2         gridExtra_2.3       
    ## [16] viridis_0.6.2        viridisLite_0.4.1    wordcloud_2.6       
    ## [19] RColorBrewer_1.1-3   SnowballC_0.7.0      tm_0.7-10           
    ## [22] NLP_0.2-1            taxize_0.9.100       ape_5.6-2           
    ## [25] rotl_3.0.14          taxa_0.4.2           vctrs_0.5.2         
    ## [28] forcats_0.5.2        stringr_1.5.0        dplyr_1.1.0         
    ## [31] purrr_1.0.1          readr_2.1.3          tidyr_1.3.0         
    ## [34] tibble_3.1.8         ggplot2_3.4.0        tidyverse_1.3.2     
    ## [37] reshape_0.8.9        openxlsx_4.2.5.1     here_1.0.1          
    ## 
    ## loaded via a namespace (and not attached):
    ##   [1] spam_2.9-1              readxl_1.4.1            uuid_1.1-0             
    ##   [4] backports_1.4.1         fastmatch_1.1-3         plyr_1.8.8             
    ##   [7] lazyeval_0.2.2          splines_4.2.2           rncl_0.8.7             
    ##  [10] pryr_0.1.6              digest_0.6.31           foreach_1.5.2          
    ##  [13] htmltools_0.5.4         fansi_1.0.4             magrittr_2.0.3         
    ##  [16] optimParallel_1.0-2     googlesheets4_1.0.1     cluster_2.1.4          
    ##  [19] tzdb_0.3.0              modelr_0.1.10           timechange_0.2.0       
    ##  [22] prettyunits_1.1.1       colorspace_2.1-0        rvest_1.0.3            
    ##  [25] haven_2.5.1             xfun_0.36               crayon_1.5.2           
    ##  [28] jsonlite_1.8.4          hexbin_1.28.2           zoo_1.8-11             
    ##  [31] phangorn_2.11.1         iterators_1.0.14        glue_1.6.2             
    ##  [34] gtable_0.3.1            gargle_1.2.1            rentrez_1.2.3          
    ##  [37] scales_1.2.1            DBI_1.1.3               Rcpp_1.0.10            
    ##  [40] plotrix_3.8-2           progress_1.2.2          bold_1.2.0             
    ##  [43] dotCall64_1.0-2         htmlwidgets_1.6.1       httr_1.4.4             
    ##  [46] ellipsis_0.3.2          pkgconfig_2.0.3         XML_3.99-0.13          
    ##  [49] dbplyr_2.3.0            utf8_1.2.2              conditionz_0.1.0       
    ##  [52] crul_1.3                tidyselect_1.2.0        rlang_1.0.6            
    ##  [55] munsell_0.5.0           cellranger_1.1.0        tools_4.2.2            
    ##  [58] cli_3.6.0               generics_0.1.3          broom_1.0.3            
    ##  [61] evaluate_0.20           fastmap_1.1.0           yaml_2.3.7             
    ##  [64] knitr_1.42              fs_1.6.0                zip_2.2.2              
    ##  [67] slam_0.1-50             xml2_1.3.3              compiler_4.2.2         
    ##  [70] rstudioapi_0.14         curl_5.0.0              reprex_2.0.2           
    ##  [73] clusterGeneration_1.3.7 stringi_1.7.12          gistr_0.9.0            
    ##  [76] highr_0.10              fields_14.1             Matrix_1.5-1           
    ##  [79] pillar_1.8.1            lifecycle_1.0.3         combinat_0.0-8         
    ##  [82] data.table_1.14.6       R6_2.5.1                codetools_0.2-18       
    ##  [85] MASS_7.3-58.1           assertthat_0.2.1        rprojroot_2.0.3        
    ##  [88] withr_2.5.0             httpcode_0.3.0          mnormt_2.1.1           
    ##  [91] expm_0.999-7            parallel_4.2.2          hms_1.1.2              
    ##  [94] quadprog_1.5-8          grid_4.2.2              coda_0.19-4            
    ##  [97] rmarkdown_2.20          googledrive_2.0.0       numDeriv_2016.8-1.1    
    ## [100] scatterplot3d_0.3-42    lubridate_1.9.1
