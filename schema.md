# `schema`: Prepare a schema diagram of relations between data.frames and/or csv/tsv files

## Description


 This function uses the column names and column types/classes of data.frames
 and/or csv files to produce a schema diagram of relations between these
 tables. By default, "correct" relations (same column names and same types/classes)
 are drawn as solid black lines. If there are evident type/class mismatches
 despite the same column names, the relation is drawn as a striped red line.
 If there is a slight mismatch that can be overcome by automatic coercion
 (integer-double, but both columns numeric) the relation line is dashed but black.

 At least one data.frame or csv/tsv file needs to be specified. In other words,
 either ``data_frames`` or ``data_frame_names`` or ``csv_files`` parameter
 needs to be specified. It is allowed to use all the three arguments/parameters
 simultaneously i.e. have some data.frames provided directly (as values),
 some others indirectly (as object names) and yet some others as csv/tsv
 file names/paths/urls to be imported via [`fread`](https://rdrr.io/cran/data.table/man/fread.html) .


## Usage

```r
schema(
  data_frames = list(),
  data_frame_names = character(0),
  csv_files = character(0),
  output_type = "grViz"
)
```


## Arguments

Argument      |Description
------------- |----------------
```data_frames```     |     A list of data.frames.
```data_frame_names```     |     A character vector of names of data.frame objects.
```csv_files```     |     A character vector of file names, file paths, or urls pointing to csv/tsv files. This value is passed to [`fread`](https://rdrr.io/cran/data.table/man/fread.html) 's argument/parameter `input` .
```output_type```     |     A string -- one of the three options: `"grViz"` or `"gv"` or `"svg"` . See the return value description below.

## Value


 One of the three possibilities:
   

*  if `output_type` is `"grViz"` : a diagram plot produced by  [`grViz`](https://rdrr.io/cran/DiagrammeR/man/grViz.html) (object of class `htmlwidget` )  

*  if `output_type` is `"gv"` : a string (character vector of length 1) with the GraphViz code (which can be later edited for tweaks)  

*  if `output_type` is `"svg"` : a string (character vector of length 1) with the svg (xml) code produced by [`export_svg`](https://rdrr.io/cran/DiagrammeRsvg/man/export_svg.html)  


