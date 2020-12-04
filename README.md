autoschema – R package for a simple diagram of relationships between
data.frames
================

This is a one-function package. Its sole function
[autoschema](https://github.com/alekrutkowski/autoschema)::[schema](https://rdrr.io/github/alekrutkowski/autoschema/man/schema.html)
shows the linkages between data.frames and/or csv/tsv files based on
their column names and column types/classes. See the usage example
below.

The `schema` function uses the column names and column types/classes of
data.frames and/or csv/tsv files to produce a schema diagram of
relations between these tables. By default, “correct” relations (same
column names and same types/classes) are drawn as solid black lines. If
there are evident type/class mismatches despite the same column names,
the relation is drawn as a dashed red line. If there is a slight
mismatch that can be overcome by automatic coercion (integer-double, but
both columns numeric) the relation line is dashed but black.

#### Dependencies

[data.table](https://CRAN.R-project.org/package=data.table)::[fread](https://rdrr.io/cran/data.table/man/fread.html),
[DiagrammeR](https://CRAN.R-project.org/package=DiagrammeR)::[grViz](https://rdrr.io/cran/DiagrammeR/man/grViz.html),
[DiagrammeRsvg](https://CRAN.R-project.org/package=DiagrammeRsvg)::[export\_svg](https://rdrr.io/cran/DiagrammeR/man/export_svg.html).

#### Installation

``` r
remotes::install_github('alekrutkowski/autoschema')
```

## Example

#### Prepare some fake data

``` r
DF <- function(...)
  data.frame(...,
             stringsAsFactors=FALSE,
             check.names=FALSE) # to allow for spaces in some column names
FirstDataFrame <-
  DF(RowNum = 1:10,
     Id1 = as.factor(letters[1:10]), # factors are actually integers
     Id2 = letters[1:10],
     Share = seq(0.73, 1.00, 0.03), # as fraction [0,1]
     AnotherVarX = runif(10),
     AnotherVarX2 = runif(10))
SecondDataFrame <-
  DF(RowNum = 5:14,
     Id2 = letters[5:14],
     Id1 = letters[5:14],
     Share = as.integer(100*seq(0.73, 1.00, 0.03)), # as % [0,100]
     AnotherVarY = runif(10))
`DataFrame Number 3` <-
  DF(Id1 = letters[15:24],
     `ID #3` = LETTERS[1:10],
     Expenditure = runif(10),
     `Another Var Z` = runif(10))
SimpleDF_1 <-
  DF(Id1 = letters[3:12],
     `ID #4` = LETTERS[6:15],
     Share = runif(10),
     `Another Var Q` = runif(10))
SimpleDF_2 <-
  DF(Id2 = letters[3:12],
     `ID #3` = LETTERS[6:15],
     `Another Var S` = runif(10))
write.csv(SimpleDF_1, 'SimpleDF_1.csv',
          row.names=FALSE)
write.table(SimpleDF_2, 'SimpleDF_2.tsv', sep='\t',
            row.names=FALSE)
Last_DF <-
  DF(`ID #4` = LETTERS[2:5],
     # Id2 = runif(4), # some numeric data vector accidentally called Id2
     `Another Var R` = runif(4))
```

#### Plot the linkages

![](plot.svg)

#### Alternative output options

`autoschema::schema` can alternatively produce [GraphViz DOT
code](https://en.wikipedia.org/wiki/DOT_(graph_description_language))
(with `output_type='gv'`) or [SVG XML
code](https://en.wikipedia.org/wiki/Scalable_Vector_Graphics#Example)
(with `output_type='svg'`) for further tweaks in the appropriate apps
(e.g. <https://graphviz.org/resources> and
<https://inkscape.org/develop/about-svg>).