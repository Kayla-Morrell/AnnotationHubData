use_scripts <- function(filename)
{
    stopifnot(length(filename) == 1 && is.character(filename))

    use_directory("inst/scripts")
    edit_file(proj_path("inst/scripts", filename), open = FALSE)
}

hub_create_package <- function(package, 
    type = c("AnnotationHub", "ExperimentHub"),
    fields = NULL, check_name = TRUE)
{
    stopifnot(
        length(package) == 1 && is.character(package),
        available_on_cran(package) == TRUE, 
        available_on_bioc(package) == TRUE,
        valid_package_name(package) == TRUE
    )

    create_package(package, fields = fields, check_name = check_name)

    ## Customization of the DESCRIPTION file
    use_description(fields = list(License = "Artistic-2.0",
        LazyData = "false",
        Version = "0.99.0"),
        check_name = FALSE)
    
    use_package_doc()
    use_roxygen_md()

    if (type == "AH") {
        use_package("AnnotationHubData")
        use_package("AnnotationHub")
        use_description(fields = list(biocViews =  "AnnotationHub"))
    }
    else {
        use_package("ExperimentHubData")
        use_package("ExperimentHub")
        use_description(fields = list(biocViews = "ExperimentHub"))
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
