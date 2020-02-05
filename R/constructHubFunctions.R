create_description <- function(type, fields)
{
    fl <- system.file("rmarkdown", "templates", "hubPkg", "DESCRIPTION",
        package = "AnnotationHubData")
    tmpl <- readLines(fl)

    if (type == "AnnotationHub") {
        writeLines(whisker.render(tmpl, data = c(fields, 
            list(
                biocViews = "AnnotationHub",
                imports = c("AnnotationHubData", "AnnotationHub")
            ))), con = "DESCRIPTION")
    }
    else {
        writeLines(whisker.render(tmpl, data = c(fields,
            list(biocViews = "ExperimentHub",
                imports = c("ExperimentHubData", "ExperimentHub")
            ))), con = "DESCRIPTION")
    } 
}

use_news <- function()
{
    fl <- system.file("rmarkdown", "templates", "hubPkg", "NEWS",
        package = "AnnotationHubData")
    tmpl <- readLines(fl)
    writeLines(whisker.render(tmpl), "NEWS")
}

create_script <- function(destination, package)
{
    fileName <- path_file(destination)

    fl <- system.file("rmarkdown", "templates", "hubPkg", fileName,
        package = "AnnotationHubData")
    tmpl <- readLines(fl)
    writeLines(whisker.render(tmpl), 
        paste0(package, destination)) 
}

hub_create_package <- function(package, 
    type = c("AnnotationHub", "ExperimentHub"),
    fields = list(packageName = path_file(package), 
        version = "0.99.0", 
        license = "Artistic-2.0"), 
    check_name = TRUE)
{
    stopifnot(
        length(package) == 1 && is.character(package),
        available_on_cran(package) == TRUE, 
        available_on_bioc(package) == TRUE
    )

    create_package(package)

    create_description(type, fields)

    use_package_doc()
    use_roxygen_md()
    use_news()
    use_directory("man")
    
    use_directory("inst/scripts")
    create_script("/inst/scripts/make-data.R", package)
    create_script("/inst/scripts/make-metadata.R", package)
    create_script("/R/zzz.R", package)

    use_directory("inst/extdata")
    df <- data.frame(matrix(ncol = 17, nrow = 0))
    colnames(df) <- c("Title", "Description", "BiocVersion", "Genome", 
        "SourceType", "SourceUrl", "SourceVersion", "Species", "TaxonomyId", 
        "Coordinate_1_based", "DataProvider", "Maintainer", "RDataClass", 
        "DispatchClass", "Location_Prefix", "RDataPath", "Tags")
    write.table(df, file = paste0(package, "/inst/extdata/metadata.csv"), 
        sep = ",", row.names = FALSE, col.names = TRUE)
}

hub_create_resource <- function(package, title, description, biocversion, 
    genome, sourcetype, sourceurl, sourceversion, species, taxid, coordinate, 
    dataprovider, maintainer, rdataclass, dispatchclass, location, rdatapath, 
    tags) 
{

    ## Check to be sure 'package' is a valid AH/EH package
    stopifnot(available::available_on_bioc(package) == TRUE)

    ## Read in the metadata.csv file
    dat_path <- system.file("extdata", "metadata.csv", package = package)
    dat <- read.csv(dat_path, header = TRUE)

    ## Be sure to validate data along the way...
} 
