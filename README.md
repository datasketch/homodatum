
<!-- README.md is generated from README.Rmd. Please edit that file -->

# homodatum

<!-- badges: start -->
<!-- badges: end -->

<br>

## Overview

“fringe: un dataset adornado”

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

<div id="wmwdcbbean" style="padding-left:0px;padding-right:0px;padding-top:10px;padding-bottom:10px;overflow-x:auto;overflow-y:auto;width:auto;height:auto;">
<style>html {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, 'Helvetica Neue', 'Fira Sans', 'Droid Sans', Arial, sans-serif;
}

#wmwdcbbean .gt_table {
  display: table;
  border-collapse: collapse;
  margin-left: auto;
  margin-right: auto;
  color: #333333;
  font-size: 16px;
  font-weight: normal;
  font-style: normal;
  background-color: #FFFFFF;
  width: auto;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #A8A8A8;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #A8A8A8;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
}

#wmwdcbbean .gt_heading {
  background-color: #FFFFFF;
  text-align: center;
  border-bottom-color: #FFFFFF;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wmwdcbbean .gt_caption {
  padding-top: 4px;
  padding-bottom: 4px;
}

#wmwdcbbean .gt_title {
  color: #333333;
  font-size: 125%;
  font-weight: initial;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-color: #FFFFFF;
  border-bottom-width: 0;
}

#wmwdcbbean .gt_subtitle {
  color: #333333;
  font-size: 85%;
  font-weight: initial;
  padding-top: 0;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-color: #FFFFFF;
  border-top-width: 0;
}

#wmwdcbbean .gt_bottom_border {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wmwdcbbean .gt_col_headings {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
}

#wmwdcbbean .gt_col_heading {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 6px;
  padding-left: 5px;
  padding-right: 5px;
  overflow-x: hidden;
}

#wmwdcbbean .gt_column_spanner_outer {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: normal;
  text-transform: inherit;
  padding-top: 0;
  padding-bottom: 0;
  padding-left: 4px;
  padding-right: 4px;
}

#wmwdcbbean .gt_column_spanner_outer:first-child {
  padding-left: 0;
}

#wmwdcbbean .gt_column_spanner_outer:last-child {
  padding-right: 0;
}

#wmwdcbbean .gt_column_spanner {
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: bottom;
  padding-top: 5px;
  padding-bottom: 5px;
  overflow-x: hidden;
  display: inline-block;
  width: 100%;
}

#wmwdcbbean .gt_group_heading {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  text-align: left;
}

#wmwdcbbean .gt_empty_group_heading {
  padding: 0.5px;
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  vertical-align: middle;
}

#wmwdcbbean .gt_from_md > :first-child {
  margin-top: 0;
}

#wmwdcbbean .gt_from_md > :last-child {
  margin-bottom: 0;
}

#wmwdcbbean .gt_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  margin: 10px;
  border-top-style: solid;
  border-top-width: 1px;
  border-top-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 1px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 1px;
  border-right-color: #D3D3D3;
  vertical-align: middle;
  overflow-x: hidden;
}

#wmwdcbbean .gt_stub {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
}

#wmwdcbbean .gt_stub_row_group {
  color: #333333;
  background-color: #FFFFFF;
  font-size: 100%;
  font-weight: initial;
  text-transform: inherit;
  border-right-style: solid;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
  padding-left: 5px;
  padding-right: 5px;
  vertical-align: top;
}

#wmwdcbbean .gt_row_group_first td {
  border-top-width: 2px;
}

#wmwdcbbean .gt_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmwdcbbean .gt_first_summary_row {
  border-top-style: solid;
  border-top-color: #D3D3D3;
}

#wmwdcbbean .gt_first_summary_row.thick {
  border-top-width: 2px;
}

#wmwdcbbean .gt_last_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wmwdcbbean .gt_grand_summary_row {
  color: #333333;
  background-color: #FFFFFF;
  text-transform: inherit;
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmwdcbbean .gt_first_grand_summary_row {
  padding-top: 8px;
  padding-bottom: 8px;
  padding-left: 5px;
  padding-right: 5px;
  border-top-style: double;
  border-top-width: 6px;
  border-top-color: #D3D3D3;
}

#wmwdcbbean .gt_striped {
  background-color: rgba(128, 128, 128, 0.05);
}

#wmwdcbbean .gt_table_body {
  border-top-style: solid;
  border-top-width: 2px;
  border-top-color: #D3D3D3;
  border-bottom-style: solid;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
}

#wmwdcbbean .gt_footnotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wmwdcbbean .gt_footnote {
  margin: 0px;
  font-size: 90%;
  padding-left: 4px;
  padding-right: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmwdcbbean .gt_sourcenotes {
  color: #333333;
  background-color: #FFFFFF;
  border-bottom-style: none;
  border-bottom-width: 2px;
  border-bottom-color: #D3D3D3;
  border-left-style: none;
  border-left-width: 2px;
  border-left-color: #D3D3D3;
  border-right-style: none;
  border-right-width: 2px;
  border-right-color: #D3D3D3;
}

#wmwdcbbean .gt_sourcenote {
  font-size: 90%;
  padding-top: 4px;
  padding-bottom: 4px;
  padding-left: 5px;
  padding-right: 5px;
}

#wmwdcbbean .gt_left {
  text-align: left;
}

#wmwdcbbean .gt_center {
  text-align: center;
}

#wmwdcbbean .gt_right {
  text-align: right;
  font-variant-numeric: tabular-nums;
}

#wmwdcbbean .gt_font_normal {
  font-weight: normal;
}

#wmwdcbbean .gt_font_bold {
  font-weight: bold;
}

#wmwdcbbean .gt_font_italic {
  font-style: italic;
}

#wmwdcbbean .gt_super {
  font-size: 65%;
}

#wmwdcbbean .gt_footnote_marks {
  font-style: italic;
  font-weight: normal;
  font-size: 75%;
  vertical-align: 0.4em;
}

#wmwdcbbean .gt_asterisk {
  font-size: 100%;
  vertical-align: 0;
}

#wmwdcbbean .gt_indent_1 {
  text-indent: 5px;
}

#wmwdcbbean .gt_indent_2 {
  text-indent: 10px;
}

#wmwdcbbean .gt_indent_3 {
  text-indent: 15px;
}

#wmwdcbbean .gt_indent_4 {
  text-indent: 20px;
}

#wmwdcbbean .gt_indent_5 {
  text-indent: 25px;
}
</style>
<table class="gt_table">
  <thead class="gt_header">
    <tr>
      <td colspan="2" class="gt_heading gt_title gt_font_normal gt_bottom_border" style><strong>Available hdTypes for variables</strong></td>
    </tr>
    
  </thead>
  <thead class="gt_col_headings">
    <tr>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="id">id</th>
      <th class="gt_col_heading gt_columns_bottom_border gt_center" rowspan="1" colspan="1" scope="col" id="label">label</th>
    </tr>
  </thead>
  <tbody class="gt_table_body">
    <tr><td headers="id" class="gt_row gt_center">___</td>
<td headers="label" class="gt_row gt_center">Null</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Uid</td>
<td headers="label" class="gt_row gt_center">Uid</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Cat</td>
<td headers="label" class="gt_row gt_center">Categorical</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Bin</td>
<td headers="label" class="gt_row gt_center">Binary</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Seq</td>
<td headers="label" class="gt_row gt_center">Sequential</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Num</td>
<td headers="label" class="gt_row gt_center">Numeric</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Pct</td>
<td headers="label" class="gt_row gt_center">Percentage</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Dst</td>
<td headers="label" class="gt_row gt_center">Distribution</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Dat</td>
<td headers="label" class="gt_row gt_center">Date</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Yea</td>
<td headers="label" class="gt_row gt_center">Year</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Mon</td>
<td headers="label" class="gt_row gt_center">Month</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Day</td>
<td headers="label" class="gt_row gt_center">Day</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Wdy</td>
<td headers="label" class="gt_row gt_center">Day of week</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Ywe</td>
<td headers="label" class="gt_row gt_center">Week in year</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Dtm</td>
<td headers="label" class="gt_row gt_center">Date time</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Hms</td>
<td headers="label" class="gt_row gt_center">Time HMS</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Min</td>
<td headers="label" class="gt_row gt_center">Minutes</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Sec</td>
<td headers="label" class="gt_row gt_center">Seconds</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Hie</td>
<td headers="label" class="gt_row gt_center">Hierarchy</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Grp</td>
<td headers="label" class="gt_row gt_center">Group</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Txt</td>
<td headers="label" class="gt_row gt_center">Text</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Mny</td>
<td headers="label" class="gt_row gt_center">Money</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Gnm</td>
<td headers="label" class="gt_row gt_center">Geo name</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Gcd</td>
<td headers="label" class="gt_row gt_center">Geo code</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Glt</td>
<td headers="label" class="gt_row gt_center">Geo latitude</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Gln</td>
<td headers="label" class="gt_row gt_center">Geo longitude</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Img</td>
<td headers="label" class="gt_row gt_center">Image</td></tr>
    <tr><td headers="id" class="gt_row gt_center">Aud</td>
<td headers="label" class="gt_row gt_center">Audio</td></tr>
  </tbody>
  
  
</table>
</div>

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
