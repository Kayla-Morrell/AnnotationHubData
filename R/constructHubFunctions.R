#create_description <- function(type, fields, destination)
#{
#    fl <- system.file("rmarkdown", "templates", "hubPkg", "DESCRIPTION",
#        package = "AnnotationHubData")
#    tmpl <- readLines(fl)
#
#    if (type == "AnnotationHub")
#        lst <- list(biocViews = "AnnotationHub",
#            imports = c("AnnotationHubData", "AnnotationHub"))
#
#    else
#        lst <- list(biocViews = "ExperimentHub",
#            imports = c("ExperimentHubData", "ExperimentHub")) 
#
#    doc <- list(roxygen = packageVersion("roxygen2"))
#
#    writeLines(whisker.render(tmpl, data = c(fields, lst, doc)), con = destination)
#}

create_base_template <- function(destination)
{
    fileName <- basename(destination)

    fl <- system.file("rmarkdown", "templates", "hubPkg", fileName, 
        package = "AnnotationHubData")
    tmpl <- readLines(fl)
    writeLines(whisker.render(tmpl), destination)
}

create_package_doc <- function(field, path)
{
    fl <- system.file("rmarkdown", "templates", "hubPkg", "pkg-package.R", 
        package = "AnnotationHubData")
    tmpl <- readLines(fl)
    destination <- file.path(path, "R", paste0(field, "-package.R"))
    writeLines(whisker.render(tmpl, data = c(field)), con = destination)
}

hub_create_package <- function(package, 
    type = c("AnnotationHub", "ExperimentHub"),
    use_git = FALSE)
{
    pth <- path.expand(package)
    pkg <- basename(pth)
    stopifnot(
        "roxygen2" %in% loadedNamespaces(),
        !file.exists(pth),
        length(pkg) == 1 && is.character(pkg),
        length(available(pkg)) == 0L, 
        available_on_bioc(pkg),
        valid_package_name(pkg) 
    )

    #dir.create(pth, recursive = TRUE)
    usethis::create_package(pth)
    usethis::proj_set(pth)

    if (use_git) {
        usethis::use_git()
    }
    
    #create_description(type, fields, file.path(pth, "DESCRIPTION"))
    biocthis::use_bioc_description(biocViews = type)

    #create_base_template(file.path(pth, "NAMESPACE")) ## NOT NEEDED ANYMORE!
    #dir.create(file.path(pth, "R"), recursive = TRUE) ## NOT NEEDED ANYMORE!

    create_package_doc(list(package = pkg), pth)
    #create_base_template(file.path(pth, "NEWS"))
    biocthis::use_bioc_news_md(open=FALSE)
    dir.create(file.path(pth, "man"), recursive = TRUE)
    
    dir.create(file.path(pth, "inst", "scripts"), recursive = TRUE)
    create_base_template(file.path(pth, "inst", "scripts", "make-data.R"))
    create_base_template(file.path(pth, "inst", "scripts", "make-metadata.R"))

    if (type == "ExperimentHub")
        create_base_template(file.path(pth, "R", "zzz.R"))

    dir.create(file.path(pth, "inst", "extdata"), recursive = TRUE)
    x <- c("Title", "Description", "BiocVersion", "Genome", "SourceType",
        "SourceUrl", "SourceVersion", "Species", "TaxonomyId", 
        "Coordinate_1_based",    
        "DataProvider", "Maintainer", "RDataClass", "DispatchClass", 
        "Location_Prefix", "RDataPath", "Tags") 
    df <- data.frame(matrix(0, nrow = 0 , ncol = 17, dimnames = list(NULL, x)))
    fl <- file.path(pth, "inst", "extdata", "metadata.csv")
    write.csv(df, file = fl, row.names = FALSE)
    invisible(pth)
}

hub_create_resource <- function(package, title, description, biocversion, 
    genome, sourcetype, sourceurl, sourceversion, species, taxid, coordinate, 
    dataprovider, maintainer, rdataclass, dispatchclass, location, rdatapath, 
    tags)
{

    ## read in the metadata.csv file
    if (available_on_bioc(package))
        dat_path <- file.path(package, "inst", "extdata", "metadata.csv")
    else
        dat_path <- system.file("extdata", "metadata.csv", package = package)

    metadata <- read.csv(file = dat_path)

    ## create a data.frame from the input
    df <- data.frame(title, description, biocversion, genome, sourcetype, 
        sourceurl, sourceversion, species, taxid, coordinate, dataprovider, 
        maintainer, rdataclass, dispatchclass, location, rdatapath, tags, 
        stringsAsFactors = FALSE)

    metadata[dim(metadata)[1]+1,] <- df
    #makeAnnotationHubMetadata(package)
    ## writing back into the csv file
    write.csv(metadata, file = dat_path, row.names = FALSE)
}
