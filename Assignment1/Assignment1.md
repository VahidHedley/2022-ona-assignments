Assignment1
================

``` r
library("tidyverse")
```

    ## Warning: package 'tidyverse' was built under R version 4.1.3

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library("igraph")
```

    ## Warning: package 'igraph' was built under R version 4.1.3

    ## 
    ## Attaching package: 'igraph'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     as_data_frame, groups, union

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     compose, simplify

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     crossing

    ## The following object is masked from 'package:tibble':
    ## 
    ##     as_data_frame

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
library(readr)
Connections <- read_csv("C:/Users/0/OneDrive/McGill - Summer 2022/ORGB 672 - Org Network Analysis/Data/Connections.csv")
```

    ## Rows: 512 Columns: 6

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (6): First Name, Last Name, Email Address, Company, Position, Connected On

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
View(Connections)
```

``` r
Connections %>% 
  count(Company) %>% 
  arrange(-n)
```

    ## # A tibble: 392 x 2
    ##    Company                                                 n
    ##    <chr>                                               <int>
    ##  1 McGill University - Desautels Faculty of Management    18
    ##  2 <NA>                                                   18
    ##  3 Global Affairs Canada | Affaires mondiales Canada       9
    ##  4 Rogers Communications                                   6
    ##  5 Scotiabank                                              6
    ##  6 University of Waterloo                                  6
    ##  7 Air Transat                                             5
    ##  8 Novartis                                                5
    ##  9 Sia Partners                                            5
    ## 10 TJX Canada/Winners Merchants International L.P.         5
    ## # ... with 382 more rows

List of Nodes:

List of Edges:
