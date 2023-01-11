
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homodatum

<!-- badges: start -->
<!-- badges: end -->

<br>

## Overview

homodatum helps to manage dataframes in a more human way. This package
mainly adds information to data frames (metadata) by creating new
classes for variables (hdTypes) and dataframes (hdFringe) and add them
more descriptive properties.

## Installation

Install the development version of makeup from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("datasketch/homodatum")
```

## Example

This is a basic example which shows you how this packages work:

Let´s load `homodatum` package

``` r
library(homodatum)
```

### New type of values

One of the main properties of this package is to add new type of
variables, in order to offer ones with (more) metadata and information.
The valid available variable new types from `homodatum` can be viewed
with `available_hdTypes()`:

<table>
<caption>
Available hdTypes for variables
</caption>
<thead>
<tr>
<th style="text-align:left;">
id
</th>
<th style="text-align:left;">
label
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
\_\_\_
</td>
<td style="text-align:left;">
Null
</td>
</tr>
<tr>
<td style="text-align:left;">
Uid
</td>
<td style="text-align:left;">
Uid
</td>
</tr>
<tr>
<td style="text-align:left;">
Cat
</td>
<td style="text-align:left;">
Categorical
</td>
</tr>
<tr>
<td style="text-align:left;">
Bin
</td>
<td style="text-align:left;">
Binary
</td>
</tr>
<tr>
<td style="text-align:left;">
Seq
</td>
<td style="text-align:left;">
Sequential
</td>
</tr>
<tr>
<td style="text-align:left;">
Num
</td>
<td style="text-align:left;">
Numeric
</td>
</tr>
<tr>
<td style="text-align:left;">
Pct
</td>
<td style="text-align:left;">
Percentage
</td>
</tr>
<tr>
<td style="text-align:left;">
Dst
</td>
<td style="text-align:left;">
Distribution
</td>
</tr>
<tr>
<td style="text-align:left;">
Dat
</td>
<td style="text-align:left;">
Date
</td>
</tr>
<tr>
<td style="text-align:left;">
Yea
</td>
<td style="text-align:left;">
Year
</td>
</tr>
<tr>
<td style="text-align:left;">
Mon
</td>
<td style="text-align:left;">
Month
</td>
</tr>
<tr>
<td style="text-align:left;">
Day
</td>
<td style="text-align:left;">
Day
</td>
</tr>
<tr>
<td style="text-align:left;">
Wdy
</td>
<td style="text-align:left;">
Day of week
</td>
</tr>
<tr>
<td style="text-align:left;">
Ywe
</td>
<td style="text-align:left;">
Week in year
</td>
</tr>
<tr>
<td style="text-align:left;">
Dtm
</td>
<td style="text-align:left;">
Date time
</td>
</tr>
<tr>
<td style="text-align:left;">
Hms
</td>
<td style="text-align:left;">
Time HMS
</td>
</tr>
<tr>
<td style="text-align:left;">
Min
</td>
<td style="text-align:left;">
Minutes
</td>
</tr>
<tr>
<td style="text-align:left;">
Sec
</td>
<td style="text-align:left;">
Seconds
</td>
</tr>
<tr>
<td style="text-align:left;">
Hie
</td>
<td style="text-align:left;">
Hierarchy
</td>
</tr>
<tr>
<td style="text-align:left;">
Grp
</td>
<td style="text-align:left;">
Group
</td>
</tr>
<tr>
<td style="text-align:left;">
Txt
</td>
<td style="text-align:left;">
Text
</td>
</tr>
<tr>
<td style="text-align:left;">
Mny
</td>
<td style="text-align:left;">
Money
</td>
</tr>
<tr>
<td style="text-align:left;">
Gnm
</td>
<td style="text-align:left;">
Geo name
</td>
</tr>
<tr>
<td style="text-align:left;">
Gcd
</td>
<td style="text-align:left;">
Geo code
</td>
</tr>
<tr>
<td style="text-align:left;">
Glt
</td>
<td style="text-align:left;">
Geo latitude
</td>
</tr>
<tr>
<td style="text-align:left;">
Gln
</td>
<td style="text-align:left;">
Geo longitude
</td>
</tr>
<tr>
<td style="text-align:left;">
Img
</td>
<td style="text-align:left;">
Image
</td>
</tr>
<tr>
<td style="text-align:left;">
Aud
</td>
<td style="text-align:left;">
Audio
</td>
</tr>
</tbody>
</table>

<br>

### New type of data frame

In order to offer a more detailed information about a data frame,
`homodatum` offers the function `fringe()`, which takes a data frame and
converts it into a more informative object adding properties such as a
dictionary, value type information, data frame name and description and
several summary calculation from de variables, depending on their type.

### Creating a *fringe* object:

``` r
# Create a dataframe
df <- data.frame(name = c("Roberta", "Ruby", "Roberta", "Maria"),
                 age  = c(98, 43, 98, 12))

# Create a fringe object
fr <- fringe(df)
```

This is how it looks with all the properties added:

``` r
str(fr)
#> List of 9
#>  $ data       : tibble[,2] (S3: tbl_df/tbl/data.frame/hd_tbl)
#>   ..$ name: Cat [1:4] Roberta, Ruby, Roberta, Maria
#>    .. ..@ categories  : chr [1:3] "Roberta" "Ruby" "Maria"
#>    .. ..@ n_categories: int 3
#>    .. ..@ stats       :List of 4
#>    .. .. ..$ n_unique: int 3
#>    .. .. ..$ n_na    : int 0
#>    .. .. ..$ pct_na  : num 0
#>    .. .. ..$ summary : tibble [4 × 4] (S3: tbl_df/tbl/data.frame)
#>    .. .. .. ..$ category: chr [1:4] "Maria" "Roberta" "Ruby" NA
#>    .. .. .. ..$ n       : int [1:4] 1 2 1 0
#>    .. .. .. ..$ dist    : num [1:4] 0.25 0.5 0.25 0
#>    .. .. .. ..$ names   : logi [1:4] NA NA NA NA
#>   ..$ age : Num [1:4] 98, 43, 98, 12
#>    .. ..@ stats:List of 5
#>    .. .. ..$ n_unique: int 3
#>    .. .. ..$ n_na    : int 0
#>    .. .. ..$ pct_na  : num 0
#>    .. .. ..$ min     : num 12
#>    .. .. ..$ max     : num 98
#>  $ dic        : tibble [2 × 3] (S3: tbl_df/tbl/data.frame)
#>   ..$ id    : chr [1:2] "name" "age"
#>   ..$ label : chr [1:2] "name" "age"
#>   ..$ hdType: hdType [1:2] Cat, Num
#>  $ frtype     : frType [1:1] Cat-Num
#>    ..@ hdTypes: hdType [1:2] Cat, Num
#>    ..@ group  : chr "Cat-Num"
#>  $ group      : chr "Cat-Num"
#>  $ name       : chr "df"
#>  $ description: chr ""
#>  $ slug       : chr "df"
#>  $ meta       : list()
#>  $ stats      :List of 3
#>   ..$ nrow     : int 4
#>   ..$ ncol     : int 2
#>   ..$ col_stats:List of 2
#>   .. ..$ name:List of 4
#>   .. .. ..$ n_unique: int 3
#>   .. .. ..$ n_na    : int 0
#>   .. .. ..$ pct_na  : num 0
#>   .. .. ..$ summary : tibble [4 × 4] (S3: tbl_df/tbl/data.frame)
#>   .. .. .. ..$ category: chr [1:4] "Maria" "Roberta" "Ruby" NA
#>   .. .. .. ..$ n       : int [1:4] 1 2 1 0
#>   .. .. .. ..$ dist    : num [1:4] 0.25 0.5 0.25 0
#>   .. .. .. ..$ names   : logi [1:4] NA NA NA NA
#>   .. ..$ age :List of 5
#>   .. .. ..$ n_unique: int 3
#>   .. .. ..$ n_na    : int 0
#>   .. .. ..$ pct_na  : num 0
#>   .. .. ..$ min     : num 12
#>   .. .. ..$ max     : num 98
#>  - attr(*, "class")= chr "fringe"
```

<br>

#### You can inspect specifics attibutes of the fringe object such as:

- Data:

``` r
fr$data
#> # A tibble: 4 × 2
#>   name      age
#>   <Cat>   <Num>
#> 1 Roberta    98
#> 2 Ruby       43
#> 3 Roberta    98
#> 4 Maria      12
```

- Dictionary:

``` r
fr$dic
#> # A tibble: 2 × 3
#>   id    label hdType  
#>   <chr> <chr> <hdType>
#> 1 name  name  Cat     
#> 2 age   age   Num
```

- Summary stats for the fringe object and its variables:

``` r
fr$stats
#> $nrow
#> [1] 4
#> 
#> $ncol
#> [1] 2
#> 
#> $col_stats
#> $col_stats$name
#> $col_stats$name$n_unique
#> [1] 3
#> 
#> $col_stats$name$n_na
#> [1] 0
#> 
#> $col_stats$name$pct_na
#> [1] 0
#> 
#> $col_stats$name$summary
#> # A tibble: 4 × 4
#>   category     n  dist names
#>   <chr>    <int> <dbl> <lgl>
#> 1 Maria        1  0.25 NA   
#> 2 Roberta      2  0.5  NA   
#> 3 Ruby         1  0.25 NA   
#> 4 <NA>         0  0    NA   
#> 
#> 
#> $col_stats$age
#> $col_stats$age$n_unique
#> [1] 3
#> 
#> $col_stats$age$n_na
#> [1] 0
#> 
#> $col_stats$age$pct_na
#> [1] 0
#> 
#> $col_stats$age$min
#> [1] 12
#> 
#> $col_stats$age$max
#> [1] 98
```

Learn about the many ways to work with formatting dates values in
`vignette("set-name")`
