---
title: "GDP ETL Documentation"
author: "Jaromír Koflák"
# date: "2025-09-01"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib]
url: https://bookdown.org/jaromir_koflak/GDP_ETL_Documentation/
cover-image: images/UNCTAD_Blue.png
description: |
  This is the documentation of the GDP ETL script used for automatic 
  collection and transformation of GDP data for UNCTADstat.
biblio-style: apalike
csl: chicago-fullnote-bibliography.csl
output:
  bookdown::bs4_book:
    includes:
      after_body: "after_body.html"
---


```
#> $knitr
#> $knitr$opts_knit
#> NULL
#> 
#> $knitr$opts_chunk
#> $knitr$opts_chunk$dev
#> [1] "png"
#> 
#> $knitr$opts_chunk$dpi
#> [1] 96
#> 
#> $knitr$opts_chunk$fig.width
#> [1] 7
#> 
#> $knitr$opts_chunk$fig.height
#> [1] 5
#> 
#> $knitr$opts_chunk$fig.retina
#> [1] 2
#> 
#> 
#> $knitr$knit_hooks
#> NULL
#> 
#> $knitr$opts_hooks
#> NULL
#> 
#> $knitr$opts_template
#> NULL
#> 
#> 
#> $pandoc
#> $pandoc$to
#> [1] "html"
#> 
#> $pandoc$from
#> [1] "markdown+autolink_bare_uris+tex_math_single_backslash"
#> 
#> $pandoc$args
#>  [1] "--embed-resources"                                                                                        
#>  [2] "--standalone"                                                                                             
#>  [3] "--variable"                                                                                               
#>  [4] "bs3=TRUE"                                                                                                 
#>  [5] "--section-divs"                                                                                           
#>  [6] "--template"                                                                                               
#>  [7] "C:\\Users\\jaromir.koflak\\AppData\\Local\\Programs\\R\\R-4.5.1\\library\\rmarkdown\\rmd\\h\\default.html"
#>  [8] "--no-highlight"                                                                                           
#>  [9] "--variable"                                                                                               
#> [10] "highlightjs=1"                                                                                            
#> 
#> $pandoc$keep_tex
#> [1] FALSE
#> 
#> $pandoc$latex_engine
#> [1] "pdflatex"
#> 
#> $pandoc$ext
#> NULL
#> 
#> $pandoc$convert_fun
#> NULL
#> 
#> $pandoc$lua_filters
#> [1] "C:\\Users\\jaromir.koflak\\AppData\\Local\\Programs\\R\\R-4.5.1\\library\\rmarkdown\\rmarkdown\\lua\\pagebreak.lua"    
#> [2] "C:\\Users\\jaromir.koflak\\AppData\\Local\\Programs\\R\\R-4.5.1\\library\\rmarkdown\\rmarkdown\\lua\\latex-div.lua"    
#> [3] "C:\\Users\\jaromir.koflak\\AppData\\Local\\Programs\\R\\R-4.5.1\\library\\rmarkdown\\rmarkdown\\lua\\table-classes.lua"
#> 
#> 
#> $keep_md
#> [1] FALSE
#> 
#> $clean_supporting
#> [1] TRUE
#> 
#> $df_print
#> function (data, options = list(), class = "display", callback = JS("return table;"), 
#>     rownames, colnames, container, caption = NULL, filter = c("none", 
#>         "bottom", "top"), escape = TRUE, style = "auto", width = NULL, 
#>     height = NULL, elementId = NULL, fillContainer = getOption("DT.fillContainer", 
#>         NULL), autoHideNavigation = getOption("DT.autoHideNavigation", 
#>         NULL), selection = c("multiple", "single", "none"), extensions = list(), 
#>     plugins = NULL, editable = FALSE) 
#> {
#>     oop = base::options(stringsAsFactors = FALSE)
#>     on.exit(base::options(oop), add = TRUE)
#>     options = modifyList(getOption("DT.options", list()), if (is.function(options)) 
#>         options()
#>     else options)
#>     if (is.character(btnOpts <- options[["buttons"]])) 
#>         options[["buttons"]] = as.list(btnOpts)
#>     params = list()
#>     attr(params, "TOJSON_ARGS") = getOption("DT.TOJSON_ARGS")
#>     if (crosstalk::is.SharedData(data)) {
#>         params$crosstalkOptions = list(key = data$key(), group = data$groupName())
#>         data = data$data(withSelection = FALSE, withFilter = TRUE, 
#>             withKey = FALSE)
#>     }
#>     rn = if (missing(rownames) || isTRUE(rownames)) 
#>         base::rownames(data)
#>     else {
#>         if (is.character(rownames)) 
#>             rownames
#>     }
#>     hideDataTable = FALSE
#>     if (is.null(data) || identical(ncol(data), 0L)) {
#>         data = matrix(ncol = 0, nrow = NROW(data))
#>         hideDataTable = TRUE
#>     }
#>     else if (length(dim(data)) != 2) {
#>         str(data)
#>         stop("'data' must be 2-dimensional (e.g. data frame or matrix)")
#>     }
#>     if (is.data.frame(data)) {
#>         data = as.data.frame(data)
#>         numc = unname(which(vapply(data, is.numeric, logical(1))))
#>     }
#>     else {
#>         if (!is.matrix(data)) 
#>             stop("'data' must be either a matrix or a data frame, and cannot be ", 
#>                 classes(data), " (you may need to coerce it to matrix or data frame)")
#>         numc = if (is.numeric(data)) 
#>             seq_len(ncol(data))
#>         data = as.data.frame(data)
#>     }
#>     if (!is.null(rn)) {
#>         data = cbind(` ` = rn, data)
#>         numc = numc + 1
#>     }
#>     options[["columnDefs"]] = colDefsTgtHandle(options[["columnDefs"]], 
#>         base::colnames(data))
#>     data = boxListColumnAtomicScalars(data)
#>     if (length(numc)) {
#>         undefined_numc = setdiff(numc - 1, classNameDefinedColumns(options, 
#>             ncol(data)))
#>         if (length(undefined_numc)) 
#>             options = appendColumnDefs(options, list(className = "dt-right", 
#>                 targets = undefined_numc))
#>     }
#>     if (is.null(options[["order"]])) 
#>         options$order = list()
#>     if (is.null(options[["autoWidth"]])) 
#>         options$autoWidth = FALSE
#>     if (is.null(options[["orderClasses"]])) 
#>         options$orderClasses = FALSE
#>     cn = base::colnames(data)
#>     if (missing(colnames)) {
#>         colnames = cn
#>     }
#>     else if (!is.null(names(colnames))) {
#>         i = convertIdx(colnames, cn)
#>         cn[i] = names(colnames)
#>         colnames = cn
#>     }
#>     if (ncol(data) - length(colnames) == 1) 
#>         colnames = c(" ", colnames)
#>     if (length(colnames) && colnames[1] == " ") 
#>         options = appendColumnDefs(options, list(orderable = FALSE, 
#>             targets = 0))
#>     for (j in seq_len(ncol(data))) options = appendColumnDefs(options, 
#>         list(name = names(data)[j], targets = j - 1))
#>     style = normalizeStyle(style)
#>     if (grepl("^bootstrap", style)) 
#>         class = DT2BSClass(class)
#>     if (style != "default") 
#>         params$style = style
#>     if (isTRUE(fillContainer)) 
#>         class = paste(class, "fill-container")
#>     if (is.character(filter)) 
#>         filter = list(position = match.arg(filter))
#>     filter = modifyList(list(position = "none", clear = TRUE, 
#>         plain = FALSE, vertical = FALSE, opacity = 1), filter)
#>     filterHTML = as.character(filterRow(data, !is.null(rn) && 
#>         colnames[1] == " ", filter))
#>     if (filter$position == "top") 
#>         options$orderCellsTop = TRUE
#>     params$filter = filter$position
#>     params$vertical = filter$vertical
#>     if (filter$position != "none") 
#>         params$filterHTML = filterHTML
#>     params$filterSettings = filter$settings
#>     if (missing(container)) {
#>         container = tags$table(tableHeader(colnames, escape), 
#>             class = class)
#>     }
#>     else {
#>         params$class = class
#>     }
#>     attr(options, "escapeIdx") = escapeToConfig(escape, colnames)
#>     if (is.list(extensions)) {
#>         extensions = names(extensions)
#>     }
#>     else if (!is.character(extensions)) {
#>         stop("'extensions' must be either a character vector or a named list")
#>     }
#>     params$extensions = if (length(extensions)) 
#>         as.list(extensions)
#>     if ("Responsive" %in% extensions && is.null(options$responsive)) {
#>         options$responsive = TRUE
#>     }
#>     params$caption = captionString(caption)
#>     if (isTRUE(editable)) 
#>         editable = "cell"
#>     if (is.character(editable)) 
#>         editable = list(target = editable, disable = list(columns = NULL))
#>     if (is.list(editable)) 
#>         params$editable = makeEditableField(editable, data, rn)
#>     if (!identical(class(callback), class(JS("")))) 
#>         stop("The 'callback' argument only accept a value returned from JS()")
#>     if (length(options$pageLength) && length(options$lengthMenu) == 
#>         0) {
#>         if (!isFALSE(options$lengthChange)) 
#>             options$lengthMenu = sort(unique(c(options$pageLength, 
#>                 10, 25, 50, 100)))
#>         if (identical(options$lengthMenu, c(10, 25, 50, 100))) 
#>             options$lengthMenu = NULL
#>     }
#>     if (!is.null(options[["search"]]) && !is.list(options[["search"]])) 
#>         stop("The value of `search` in `options` must be NULL or a list")
#>     if (!is.null(fillContainer)) 
#>         params$fillContainer = fillContainer
#>     if (!is.null(autoHideNavigation)) {
#>         if (isTRUE(autoHideNavigation) && length(options$pageLength) == 
#>             0L) 
#>             warning("`autoHideNavigation` will be ignored if the `pageLength` option is not provided.", 
#>                 immediate. = TRUE)
#>         params$autoHideNavigation = autoHideNavigation
#>     }
#>     params = structure(modifyList(params, list(data = data, container = as.character(container), 
#>         options = options, callback = if (!missing(callback)) JS("function(table) {", 
#>             callback, "}"))), colnames = cn, rownames = length(rn) > 
#>         0)
#>     if (inShiny() || length(params$crosstalkOptions)) {
#>         if (is.character(selection)) {
#>             selection = list(mode = match.arg(selection))
#>         }
#>         selection = modifyList(list(mode = "multiple", selected = NULL, 
#>             target = "row", selectable = NULL), selection, keep.null = TRUE)
#>         if (grepl("^row", selection$target) && is.character(selection$selected) && 
#>             length(rn)) {
#>             selection$selected = match(selection$selected, rn)
#>         }
#>         params$selection = validateSelection(selection)
#>         if ("Select" %in% extensions && selection$mode != "none") 
#>             warning("The Select extension can't work properly with DT's own ", 
#>                 "selection implemention and is only recommended in the client mode. ", 
#>                 "If you really want to use the Select extension please set ", 
#>                 "`selection = 'none'`", immediate. = TRUE)
#>     }
#>     deps = DTDependencies(style)
#>     deps = c(deps, unlist(lapply(extensions, extDependency, style, 
#>         options), recursive = FALSE))
#>     if (params$filter != "none") 
#>         deps = c(deps, filterDependencies())
#>     if (isTRUE(options$searchHighlight)) 
#>         deps = c(deps, list(pluginDependency("searchHighlight")))
#>     if (length(plugins)) 
#>         deps = c(deps, lapply(plugins, pluginDependency))
#>     deps = c(deps, crosstalk::crosstalkLibs())
#>     if (isTRUE(fillContainer)) {
#>         width = NULL
#>         height = NULL
#>     }
#>     htmlwidgets::createWidget("datatables", if (hideDataTable) 
#>         NULL
#>     else params, package = "DT", width = width, height = height, 
#>         elementId = elementId, sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE, 
#>             defaultWidth = "100%", defaultHeight = "auto"), dependencies = deps, 
#>         preRenderHook = function(instance) {
#>             data = instance[["x"]][["data"]]
#>             if (object.size(data) > 1500000 && getOption("DT.warn.size", 
#>                 TRUE)) 
#>                 warning("It seems your data is too big for client-side DataTables. You may ", 
#>                   "consider server-side processing: https://rstudio.github.io/DT/server.html")
#>             data = escapeData(data, escape, colnames)
#>             data = unname(data)
#>             instance$x$data = data
#>             instance
#>         })
#> }
#> <bytecode: 0x00000254c9c4dd60>
#> <environment: namespace:DT>
#> 
#> $pre_knit
#> function (...) 
#> {
#>     op(base(...), overlay(...))
#> }
#> <bytecode: 0x00000254c94c0d60>
#> <environment: 0x00000254c9e5bb68>
#> 
#> $post_knit
#> function (...) 
#> {
#>     op(base(...), overlay(...))
#> }
#> <bytecode: 0x00000254c94c0d60>
#> <environment: 0x00000254c9e5b468>
#> 
#> $pre_processor
#> function (...) 
#> {
#>     op(base(...), overlay(...))
#> }
#> <bytecode: 0x00000254c94c0d60>
#> <environment: 0x00000254c9e48c78>
#> 
#> $intermediates_generator
#> function (original_input, intermediates_dir) 
#> {
#>     copy_render_intermediates(original_input, intermediates_dir, 
#>         !self_contained)
#> }
#> <bytecode: 0x00000254cd6c04d8>
#> <environment: 0x00000254c9c4b040>
#> 
#> $post_processor
#> function (metadata, input_file, output_file, ...) 
#> {
#>     original_output_file <- output_file
#>     output_file <- overlay(metadata, input_file, output_file, 
#>         ...)
#>     if (!is.null(attr(output_file, "post_process_original"))) 
#>         base(metadata, input_file, original_output_file, ...)
#>     base(metadata, input_file, output_file, ...)
#> }
#> <bytecode: 0x00000254c94989a8>
#> <environment: 0x00000254c9e47dd0>
#> 
#> $file_scope
#> NULL
#> 
#> $on_exit
#> function () 
#> {
#>     if (is.function(base)) 
#>         base()
#>     if (is.function(overlay)) 
#>         overlay()
#> }
#> <bytecode: 0x00000254c94ab0a8>
#> <environment: 0x00000254c9e471c8>
#> 
#> attr(,"class")
#> [1] "rmarkdown_output_format"
```

# About {-}

This R script performs ETL (Extract, Transform, Load) operations on GDP data sourced from the UN Statistics Division. It handles preprocessing, data aggregation for regions/groups and creates comparison plots. The pipeline process can be broken down into 3 steps:

1.  Data is **extracted** from [UNSD](https://unstats.un.org/unsd/amaapi/swagger/index.html) and [Taiwan NSO](https://nstatdb.dgbas.gov.tw/dgbasall/webMain.aspx?k=engmain) using their API.
2.  Afterwards, the data is **transformed** such that is meets UNCTAD requirements.
3.  Lastly, the data is **loaded** into a csv file which can be uploaded to [UNCTADstat Data centre](https://unctadstat.unctad.org/datacentre/).

---

## Gross Domestic Product {-}
GDP at **current prices** (or **nominal** GDP) measures the value of goods and services produced in an economy using the prices of the same year, so it includes the effects of inflation. In contrast, GDP at **constant prices** (or **real** GDP) adjusts for inflation by using prices from a base year (2015), reflecting only the actual change in the quantity of goods and services produced. This makes real GDP a better measure of economic growth over time. 

## Libraries {-}
The R scripts and this bookdown document require the following packages:


``` r
library(tidyverse)
library(readxl) 
library(httr)
library(plotly)
library(htmltools)
library(gridExtra)
```
