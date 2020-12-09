#' @importFrom DiagrammeRsvg export_svg
#' @importFrom DiagrammeR grViz
#' @import data.table
NULL

#' Prepare a schema diagram of relations between data.frames and/or csv/tsv files
#'
#' This function uses the column names and column types/classes of data.frames
#' and/or csv files to produce a schema diagram of relations between these
#' tables. By default, "correct" relations (same column names and same types/classes)
#' are drawn as solid black lines. If there are evident type/class mismatches
#' despite the same column names, the relation is drawn as a striped red line.
#' If there is a slight mismatch that can be overcome by automatic coercion
#' (integer-double, but both columns numeric) the relation line is dashed but black. \cr \cr
#' At least one data.frame or csv/tsv file needs to be specified. In other words,
#' either \code{`data_frames`} or \code{`data_frame_names`} or \code{`csv_files`} parameter
#' needs to be specified. It is allowed to use all the three arguments/parameters
#' simultaneously i.e. have some data.frames provided directly (as values),
#' some others indirectly (as object names) and yet some others as csv/tsv
#' file names/paths/urls to be imported via \code{\link[data.table]{fread}}.
#'
#' @param data_frames A list of data.frames.
#' @param data_frame_names A character vector of names of data.frame objects.
#' @param csv_files A character vector of file names, file paths, or urls pointing
#' to csv/tsv files. This value is passed to \code{\link[data.table]{fread}}'s
#' argument/parameter \code{input}.
#' @param output_type A string -- one of the three options: \code{"grViz"} or
#' \code{"gv"} or \code{"svg"}. See the return value description below.
#' @return One of the three possibilities:
#' \itemize{
#'  \item if \code{output_type} is \code{"grViz"}: a diagram plot produced by
#'        \code{\link[DiagrammeR]{grViz}} (object of class \code{htmlwidget})
#'  \item if \code{output_type} is \code{"gv"}: a string (character vector of length 1)
#'        with the GraphViz code (which can be later edited for tweaks)
#'  \item if \code{output_type} is \code{"svg"}: a string (character vector of length 1)
#'        with the svg (xml) code produced by \code{\link[DiagrammeRsvg]{export_svg}}
#' }
#' @export
schema <- function(data_frames=list()
                   ,data_frame_names=character(0)
                   ,csv_files=character(0)
                   ,output_type='grViz') {

  # Sanity checks for the parameters/arguments
  stopifnot(
    is.list(data_frames)
    ,is.character(data_frame_names)
    ,is.character(csv_files)
    ,is.character(output_type)
    ,length(output_type)==1
    ,output_type %in% c('grViz','gv','svg')
  )
  if (
    identical(data_frames, list()) &&
    identical(data_frame_names,character(0)) &&
    identical(csv_files,character(0))
  ) stop('At least one of the following parameters must be specified:\n'
         ,'`data_frames`, `data_frame_names`, or `csv_files`.')
  `length(data_frames)>0` <- length(data_frames)>0
  if (`length(data_frames)>0` && !all(sapply(data_frames, is.data.frame)))
    stop("One or more of the `data_frames` isn't/aren't data.frame(s).")
  if (length(data_frame_names)>0 && !all(sapply(data_frame_names, exists)))
    stop("One or more of the `data_frame_names` point(s) to non-existing object(s).")
  `length(csv_files)>0` <- length(csv_files)>0
  if (`length(csv_files)>0` && !all(file.exists(csv_files)))
    stop("One or more of the `csv_files` doesn't/don't exist.")

  # Gathering data.frames:
  `mget(data_frame_names, inherits=TRUE)` <- mget(data_frame_names, inherits=TRUE)
  `names(data_frames)` <- names(data_frames)
  df_obj_names <-
    sapply(substitute(data_frames), as.character)[-1]
  # on.exit(detach("package:data.table", unload=TRUE))
  # library(data.table) # needed, otherwise error: could not find function ":="
  dfs <-
    c(
      setNames(data_frames
               ,if (is.null(`names(data_frames)`) && `length(data_frames)>0`)
                 df_obj_names else
                   ifelse(`names(data_frames)`=="",df_obj_names,`names(data_frames)`))
      ,setNames(`mget(data_frame_names, inherits=TRUE)`, data_frame_names)
      ,setNames(if (`length(csv_files)>0`)
        lapply(csv_files, data.table::fread) else list()
        , csv_files)
    )

  # helpers
  quo <- function(x) paste0('`',x,'`')
  quoIfSpaceInside <- function(x)
    ifelse(grepl('[[:space:]]',x),quo(x),x)
  surroundWithSpaces <- function(s,v)
    sapply(v, function(x) {
      n <- nchar(x)
      s. <- paste(rep.int(s,round(.3*n)+4),collapse="")
      paste0(s.,x,s.)
    })
  colnamesInfo <- function(df_name) {
    `quo(df_name)` <- quo(df_name)
    .df <- dfs[[df_name]]
    if (ncol(.df)==0)
      stop(`quo(df_name)`,' has 0 columns.', call.=FALSE)
    col_names <- colnames(.df)
    `duplicated(col_names)` <- duplicated(col_names)
    if (any(`duplicated(col_names)`))
      stop(`quo(df_name)`,'` has duplicated column names:\n'
           ,paste(quo(unique(col_names[`duplicated(col_names)`]))
                  ,collapse=', ')
           ,call.=FALSE)
    data.table::data.table(
      .colname=col_names
      ,.classes=sapply(col_names, function(x) paste(class(.df[[x]]), collapse=', '))
      ,.type=sapply(col_names, function(x) typeof(.df[[x]]))
    )[, .type_class :=
        ifelse(.type==.classes
               ,.type
               ,paste(.type,.classes
                      ,sep='; '))]
  }

  colnames_db <-
    data.table::rbindlist(
      sapply(names(dfs)
             ,colnamesInfo
             ,simplify=FALSE,USE.NAMES=TRUE)
      ,idcol='.data.frame'
    )[, .id := paste0('"',.data.frame,'__@__',.colname,'"')]
  `setdiff(colnames(colnames_db),'.colname')` <-
    setdiff(colnames(colnames_db),'.colname')
  `newnames(colnames_db_copy)` <-
    paste0(`setdiff(colnames(colnames_db),'.colname')`
           ,'__copy')
  colnames_db_copy <-
    data.table::setnames(data.table::copy(colnames_db)
                         ,`setdiff(colnames(colnames_db),'.colname')`
                         ,`newnames(colnames_db_copy)`)
  conn_db <-
    merge(colnames_db, colnames_db_copy
          , by='.colname'
          ,allow.cartesian=TRUE)[.data.frame!=.data.frame__copy]

  # Graphviz code:
  connections <-
    unique(
      paste(
        ifelse(
          conn_db$.id < conn_db$.id__copy
          ,paste0(conn_db$.id,'--',conn_db$.id__copy)
          ,paste0(conn_db$.id__copy,'--',conn_db$.id))
        ,ifelse(
          conn_db$.type_class!=conn_db$.type_class__copy
          ,paste0('['
                  ,ifelse(conn_db$.type_class=='integer' & conn_db$.type_class__copy=='double; numeric' |
                            conn_db$.type_class=='double; numeric' & conn_db$.type_class__copy=='integer'
                          ,'style=dashed, color=black, penwidth=3'
                          ,'style=dotted, color=red, penwidth=7')
                  ,']')
          ,'[penwidth=2]')))
  connected_nodes <-
    unique(melt(conn_db[,.(.id,.id__copy)]
         ,measure.vars=c('.id','.id__copy'))[['value']])
  labels <-
    paste0(colnames_db$.id
           # all blanks below to ensure enough space for the monospace labels
           ,'[label="'
           ,surroundWithSpaces(" ",quoIfSpaceInside(colnames_db$.colname))
           ,'\\n'
           ,surroundWithSpaces(" ",colnames_db$.type_class)
           ,'", '
           ,'color='
           ,ifelse(colnames_db$.id %in% connected_nodes,'lightblue3','white')
           ,']')
  subgraphs <-
    colnames_db[
      , .(contents=paste(.id,collapse=';\n')), by='.data.frame'][
        , code := paste0('subgraph "cluster_',gsub('[^a-zA-Z0-9]','_',make.names(.data.frame)),'" {\n'
                         ,'label=<<b>',surroundWithSpaces('&nbsp;',quoIfSpaceInside(.data.frame))
                         ,'</b>>;\n' # to ensure enough space for monospace font label
                         ,'style=filled;\n'
                         ,'color=grey90;\n'
                         ,contents,';\n}\n')]$code
  final_code <-
    paste('graph G {'
          ,'rankdir="LR";'
          ,'ranksep=2;'
          ,'graph[fontname="monospace"];'
          ,'node[shape=box, style=filled, fontname="monospace"];\n'
          ,paste(subgraphs,collapse='\n')
          ,paste(c(connections,""),collapse=';\n')
          ,paste(labels,collapse=';\n')
          ,'}'
          ,sep='\n')

  # Return value:
  if (output_type=='grViz')
    DiagrammeR::grViz(final_code) else
      if (output_type=='svg')
        DiagrammeRsvg::export_svg(DiagrammeR::grViz(final_code)) else
          final_code
}
