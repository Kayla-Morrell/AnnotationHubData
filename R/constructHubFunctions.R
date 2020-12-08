#' Create a Bioconductor package
#'
#' This function creates the skeleton of a package that follows the guidelines 
#' for Bioconductor type packages. It is expected of the user to go through and 
#' make any necessary changes or improvements once the packages begins to take 
#' shape. For example, the DESCRIPTION contains very basic requirements, but the 
#' developer should go back in fill in the 'Title:' and 'Description:' fields.
#'
#' @param package A `character(1)` with the path of the package to be created.
#' @param type A `character(1)` to indicate what type of hub package is to be 
#' created. Either `AnnotationHub` or `ExperimentHub` are acceptable.
#' @param use_git A `logical(1)` indicating whether to set up `git` using 
#' `usethis::use_git()`. Default is set to FALSE.
#'
#' @export
#'
#' @examples
#' fl <- tempdir()
#' hub_create_package(paste0(fl,"/tstPkg"), "AnnotationHub", TRUE)
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

    usethis::use_directory("inst/extdata")
    x <- as.list(c(title = "Title", description = "Description",
        biocversion = "BiocVersion", genome = "Genome",
        sourcetype = "SourceType", sourceurl = "SourceUrl",
        sourceversion = "SourceVersion", species = "Species",
        taxonomyid = "TaxonomyId", coordinate1based = "Coordinate_1_based",
        dataprovider = "DataProvider", maintainer = "Maintainer",
        rdataclass = "RDataClass", dispatchclass = "DispatchClass",
        locationprefix = "Location_Prefix", rdatapath = "RDataPath",
        tags = "Tags"))
    usethis::use_template("metadata.csv",
        save_as = "/inst/extdata/metadata.csv",
        data = x,
        package = "AnnotationHubData")
    #df <- data.frame(matrix(0, nrow = 0 , ncol = 17, dimnames = list(NULL, x)))
    #fl <- file.path(pth, "inst", "extdata", "metadata.csv")
    #write.csv(df, file = fl, row.names = FALSE)

    usethis::use_testthat()
    usethis::use_template("test_metadata.R",
        save_as = "/tests/testthat/test_metadata.R",
        data = list(type = type, package = pkg),
        package = "AnnotationHubData")
    invisible(pth)
}

#' Add a hub resource
#'
#' This function adds a hub resource to the AH or EH package metadata.csv file.
#' It can be used while creating a new hub package or for adding data to an 
#' existing package.
#'
#' @param package A `character(1)` with the name of an existing hub package or 
#' the path to a newly created (not yet submitted/accepted) hub package.
#' @param fields A named character list with the data to be added to the 
#' resource. The required entries are:
#' \itemize{
#'   \item title
#'   \item description
#'   \item biocversion
#'   \item genome
#'   \item sourcetype
#'   \item sourceurl
#'   \item sourceversion
#'   \item species
#'   \item taxonomyid
#'   \item coordinate1based
#'   \item dataprovider
#'   \item maintainer
#'   \item rdataclass
#'   \item dispatchclass
#'   \item locationprefix
#'   \item rdatapath
#'   \item tags
#' All these entries are needed in order to add the resource properly to the 
#' metadata.csv file.
#' 
#' @export
#'
#' @examples
#' tst <- list(title = "ENCODE", description = "a test entry", biocversion = "3.9", genome = NA, sourcetype = "JSON", sourceurl = "https://www.encodeproject.org", sourceversion = NA, species = NA, taxonomyid = NA, coordinate1based = NA, dataprovider = "ENCODE Porject", maintainer = "tst person <tst@email.com>", rdataclass = "data.table", dispatchclass = "Rda", locationprefix = NA, rdatapath = "ENCODExplorerData/encode_df_lite.rda", tags = "ENCODE")
#' hub_add_resource("~/Documents/tstPkg", fields = tst)
hub_add_resource <- function(package, fields)
{
    fl <- system.file("inst", "templates", "metadata.csv",
        package = "AnnotationHubData")
    tmpl <- readLines(fl)

    ## read in the metadata.csv file
    if (available_on_bioc(package))
        dat_path <- file.path(package, "inst", "extdata", "metadata.csv")
    else
        dat_path <- system.file("extdata", "metadata.csv", package = package)

    #metadata <- read.csv(file = dat_path)

    dat <- strsplit(whisker.render(tmpl, data = fields), ",")
    #metadata[dim(metadata)[1]+1,] <- dat[[1]]
    write.table(dat, file = dat_path, row.names = FALSE, sep = ",", append = TRUE)
    #test*HubMetadata(package) change for specific hub
}
