hub_create_package <- function(package, 
    type = c("AnnotationHub", "ExperimentHub"),
    use_git = FALSE)
{
    current_dir <- getwd()
    on.exit(setwd(current_dir))

    pth <- path.expand(package)
    pkg <- basename(pth)
    stopifnot(
        !file.exists(pth),
        length(pkg) == 1 && is.character(pkg),
        length(available(pkg)) == 0L, 
        available_on_bioc(pkg),
        valid_package_name(pkg) 
    )

    usethis::create_package(pth)
    usethis::proj_set(pth)

    if (use_git) {
        usethis::use_git()
    }
    
    biocthis::use_bioc_description(biocViews = type)

    usethis::use_template("pkg-package.R",
        save_as = paste0("/R/",pkg,"-package.R"),
        data = list(package = pkg),
        package = "AnnotationHubData")
    biocthis::use_bioc_news_md(open=FALSE)
    usethis::use_directory("man")
    
    usethis::use_directory("inst/scripts")
    usethis::use_template("make-data.R",
        save_as = "/inst/scripts/make-data.R",
        package = "AnnotationHubData")
    usethis::use_template("make-metadata.R",
        save_as = "/inst/scripts/make-metadata.R",
        package = "AnnotationHubData")

    if (type == "ExperimentHub")
        usethis::use_template("zzz.R",
            save_as = "/R/zzz.R",
            package = "AnnotationHubData")

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
