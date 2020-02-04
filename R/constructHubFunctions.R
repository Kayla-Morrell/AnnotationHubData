use_scripts <- function(filename)
{
    stopifnot(length(filename) == 1 && is.character(filename))

    use_directory("inst/scripts")
    edit_file(proj_path("inst/scripts", filename), open = FALSE)
}

create_description <- function(type, fields)
{
    fl <- system.file("rmarkdown", 
        "templates",
        "description", 
        "skeleton",
        "DESCRIPTION",
        package = "AnnotationHubData")
    tmpl <- readLines(fl)

    if (type == "AnnotationHub") {
        writeLines(whisker.render(tmpl, data = c(fields, 
            list(biocViews = "AnnotationHub")
        )), con = "DESCRIPTION")
    }
    else {
        writeLines(whisker.render(tmpl, data = c(fields,
            list(biocViews = "ExperimentHub")
        )), con = "DESCRIPTION")
    } 
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

    ## Customization of the DESCRIPTION file (use whisker to render DESCRIPTION)
    create_description(type, fields)

    use_package_doc()
    use_roxygen_md()

    if (type == "AnnotationHub") {
        use_package("AnnotationHubData")
        use_package("AnnotationHub")
    }
    else {
        use_package("ExperimentHubData")
        use_package("ExperimentHub")
    }

    ## Addition of README file
    use_readme_md(open = FALSE)

    ## Addition of NEWS file
    use_news_md(open = FALSE)

    ## Addition of the inst/extdata directory
    use_directory("inst/extdata")

    ## Addition of the inst/script R files
    use_scripts("make-data.R")
    use_scripts("make-metadata.R")

    ## Addition of man/ directory
    use_directory("man")
}
    ## Addition of template csv file in inst/extdata
#    file.create(paste0(getwd(),"/inst/extdata/metadata.csv"))
#    df <- data.frame(matrix(ncol=17, nrow=0))
#    x <- c("Title", "Description", "BiocVersion", "Genome", "SourceType", 
#        "SourceUrl", "SourceVersion", "Species", "TaxonomyId", 
#        "Coordinate_1_based", "DataProvider", "Maintainer", "RDataClass", 
#        "DispatchClass", "Location_Prefix", "RDataPath", "Tags")
#    colnames(df) <- x
#    write.table(df, file = "/inst/extdata/metadata.csv", sep = ",",
#        row.names = FALSE, col.names = TRUE)

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
