### =========================================================================
### AnnotationHubMetadata objects
### -------------------------------------------------------------------------
###

setClass("AnnotationHubMetadata",
    contains="HubMetadata",
    representation(
        AnnotationHubRoot="character"
    ),
    prototype = prototype(
        AnnotationHubRoot=NA_character_
    )
)

## -----------------------------------------------------------------------------
## constructor
##

.derivedFileName <-
    function(originalFile, suffix)
{
    ret <- sub(".gz$", "", basename(originalFile))
    ret <- paste(ret, collapse="-")
    sprintf("%s_%s.%s", ret, suffix)
}

## High level helper used to check metadata in 'Hub' packages.
#
#  The checks in this function are applied per column of metadata.csv
#
.readMetadataFromCsv <- function(pathToPackage, fileName=character())
{
    if (!length(fileName))
        fileName <- "metadata.csv"
    path <- file.path(pathToPackage, "inst", "extdata")
    metadataFile <- file.path(path, fileName)
    .readMetadataFromCsvFile(metadataFile)
}

.readMetadataFromCsvFile <-
    function(metadataFile)
{
    meta <- read.csv(metadataFile, colClasses="character")
    mat <- rbind(c("Title", "character"),
                 c("Description", "character"),
                 c("BiocVersion", "character"),
                 c("Genome", "character"),
                 c("SourceType", "character"),
                 c("SourceUrl", "character"),
                 c("SourceVersion", "character"),
                 c("Species", "character"),
                 c("TaxonomyId", "integer"),
                 c("Coordinate_1_based", "logical"),
                 c("DataProvider", "character"),
                 c("Maintainer", "character"),
                 c("RDataClass", "character"),
                 c("DispatchClass", "character"),
                 c("RDataPath", "character"))

    expected <- mat[,1]
    missing <- !expected %in% names(meta)
    if (any(missing))
        stop("missing fields in metadata file ", fileName, ": ",
             paste(expected[missing], collapse=", "))
    extra<- !names(meta) %in% expected

    ## This does not work as expected! when read in missing fields get populated
    ## with empty character which registers as valid
    ## and some fields can have length > 1
    ## All fields length 1
    apply(meta, 1,
        function(xx) {
            valid <- sapply(xx, function(field) length(field) == 1L)
            if (any(!valid))
                stop("all fields in ", fileName, " must be a character ",
                     "string of length 1")
        }

    )

    ## Populate required fields
    missing <- which(!nchar(meta$DataProvider))
    if (any(missing)) {
        meta$DataProvider[missing] <- "NA"
        message("missing values for 'DataProvider set to 'NA''")
    }
    # if user provided NA convert to character else
    # ERROR when adding to database
    NAvls <- which(is.na(meta$DataProvider))
    if (any(NAvls)){
        meta$DataProvider[NAvls] <- 'NA'
    }
    missingOrNA <- which(is.na(meta$Coordinate_1_based) | !nchar(meta$Coordinate_1_based))
    if (any(missingOrNA)) {
        meta$Coordinate_1_based[missingOrNA] <- TRUE
        meta$Coordinate_1_based[meta$Coordinate_1_based %in% "0"] = "FALSE"
        meta$Coordinate_1_based[meta$Coordinate_1_based %in% "1"] = "TRUE"
        meta$Coordinate_1_based <- as.logical(meta$Coordinate_1_based)
        message("missing or NA values for 'Coordinate_1_based set to TRUE'")
    } else {
        meta$Coordinate_1_based[meta$Coordinate_1_based %in% "0"] = "FALSE"
        meta$Coordinate_1_based[meta$Coordinate_1_based %in% "1"] = "TRUE"
        meta$Coordinate_1_based <- as.logical(meta$Coordinate_1_based)
    }

    missing <- which(!nchar(meta$DispatchClass))
    if (any(missing)) {
        stop("All fields in 'DispatchClass' must be set")
    }
    if(!validDispatchClass(meta$DispatchClass, verbose=TRUE)){
        stop("Found one or more invalid DispatchClass.")
    }


    # Validate Class
    missing <- which(!nchar(meta$RDataClass))
    if (any(missing)) {
        stop("All fields in 'RDataClass' must be set")
    }

    # Validate Species
    missing <- which(!nchar(meta$Species))
    if (any(missing)) {
        meta$Species[missing] <- meta$Species[NAvls] <- NA_character_
        message("missing values for 'Species set to 'NA''")
    }
    meta$Species = as.character(meta$Species)

    if(!validSpecies(meta$Species, verbose=TRUE)){
        stop("Found one or more invalid species.")
    }

    ## Enforce data type
    meta$TaxonomyId <- as.integer(meta$TaxonomyId)
    checkSpeciesTaxId(meta$TaxonomyId, meta$Species, verbose=FALSE)

    missing <- which(!nchar(meta$BiocVersion))
    if (any(missing)) {
        stop("all fields in BiocVersion must be specified")
    } else {
        meta$BiocVersion <- package_version(meta$BiocVersion)
    }

    ## Location_Prefix not specified -> data in S3
    if (all(is.null(Location_Prefix <- meta$Location_Prefix))) {
        meta$Location_Prefix <- 'http://s3.amazonaws.com/annotationhub/'
    ## Location_Prefix specified -> data at other location
    }

    RDataPath <- meta$RDataPath
    RDataPath <- strsplit(RDataPath, split=":")

    if(all(
        (meta$Location_Prefix == 'http://s3.amazonaws.com/annotationhub/') ||
        (meta$Location_Prefix == 'http://s3.amazonaws.com/experimenthub/'))
       ){
        package <- basename(pathToPackage)
        test <- vapply(unlist(RDataPath), startsWith, logical(1), package)
        if ((!all(test)) || (any(is.na(test)))){
            stop("RDataPath must start with package name: ", package)
        }
    }
    meta$RDataPath <- RDataPath

    ## Real time assignments
    meta$RDataDateAdded <- rep(Sys.time(), nrow(meta))
    meta
}


checkSpeciesTaxId <- function(txid, species, verbose=TRUE){
    if (length(txid) != length(species))
        stop("taxonomy id list and species list are not same length")
    txdb <- GenomeInfoDb::loadTaxonomyDb()
    txdb <- rbind(txdb, c(NA, NA, ""))
    combo <- trimws(paste(txdb$genus, txdb$species))
    dx <- (!is.na(species)) & (!is.na(txid))
    species <- species[dx]
    txid <- txid[dx]
    if (!all(species %in% combo))
        stop("species not found in table of available species.\n",
             "    See GenomeInfoDb::loadTaxonomyDb().")
    sp_id <- txdb$tax_id[match(species, combo)]
    dx <- txid == sp_id
    if (!all(dx)){
        err = data.frame(given_species=species[!dx], given_tax_id=txid[!dx],
            expected_tax_id=sp_id[!dx], species_of_giventxid=combo[match(txid[!dx], txdb$tax_id)])
        warning("TaxonomyId does not match expected taxonomy id for given Species.",
             "\n    Run checkSpeciesTaxId(txid, species) to see suggestions.",
             "\n    See GenomeInfoDb::loadTaxonomyDb() table for full list of valid entries.")
        if (verbose) err
    }
}

#################################################################
#
# Below checkes are performed in AnnotationHubMetadata
#  essentially checking a single value
#
#################################################################

## single value and not NA
.checkValidSingleString <- function(value) {
    valStr <- deparse(substitute(value))
    if(!isSingleString(value) || (trimws(value) == ""))
        stop(wmsg(paste0(valStr, " must be defined and not NA")))
}

## single value or NA
.checkThatSingleStringOrNA <- function(value) {
    valStr <- deparse(substitute(value))
    if(!isSingleStringOrNA(value))
        stop(wmsg(paste0(valStr, " must be single value or NA")))
}

## single value or NA, no commas
.checkThatSingleStringOrNAAndNoCommas <- function(value) {
    valStr <- deparse(substitute(value))
    .checkThatSingleStringOrNA(value)
    if(grepl(",",value))
        stop(wmsg(paste0(valStr, " must not contain commas")))
}

## single value, not NA, no commas
.checkThatSingleStringAndNoCommas <- function(value) {
    valStr <- deparse(substitute(value))
    if(!isSingleString(value))
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one ",valStr)))
    if(grepl(",",value))
        stop(wmsg(paste0("The ",valStr," in an AnnotationHubMetdata object",
                         " must not contain any commas")))
}

.checkRDataClassConsistent <- function(value) {

    valStr <- deparse(substitute(value))

    if(length(unique(value)) != 1)
        stop(wmsg(paste0("RDataClass should be the same for all files")))

}

.checkValidMaintainer <- function(value) {

    valStr <- deparse(substitute(value))
    ## valid e-mail address
    emailRegex <-
        "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"
    if (!grepl(emailRegex, value, ignore.case=TRUE))
        stop(wmsg(paste0("'Maintainer' not a valid email address: ",
          value)))
}

.checkFileLengths <- function(RDataPath, DispatchClass)
{

    if(DispatchClass == "BamFile"){
        stopifnot((length(RDataPath) == 2), identical(file_ext(RDataPath),c("bam", "bai")))
    }
    TRUE
}

globalVariables(c("BiocVersion", "Coordinate_1_based", "DataProvider",
                  "Description", "DispatchClass", "Genome", "Location_Prefix",
                  "Maintainer", "RDataClass", "RDataDateAdded", "RDataPath",
                  "SourceType", "SourceUrl", "SourceVersion", "Species",
                  "TaxonomyId", "Title"))

testAnnotationHubMetadata <-
    function(packageName, metadataFile)
{
    ## metadataFile is the full path to the metadata.csv file
    .package <- packageName # alias for consistency with code in other functions
    stopifnot(
        is.character(.package), length(.package) == 1L, !nzchar(.package),
        !is.na(.package),
        is.character(metadataFile), length(metadataFile) == 1L,
        !nzchar(metadataFile), !is.na(metadataFile),
        file.exists(metadataFile)
    )

    meta <- .readMetadataFromCsvFile(metadataFile)
    ## instantiate AnnotationHubMetdata objects for each row, as test
    ## of validity
    lapply(seq_len(nrow(meta)), function(x) {
        with(meta[x, ], {
            AnnotationHubMetadata(
                Title=Title, Description=Description,
                BiocVersion=BiocVersion, Genome=Genome,
                SourceType=SourceType,
                SourceUrl=SourceUrl,
                SourceVersion=SourceVersion,
                Species=Species, TaxonomyId=TaxonomyId,
                Coordinate_1_based=Coordinate_1_based,
                DataProvider=DataProvider,
                Maintainer=Maintainer,
                RDataClass=RDataClass, Tags=.tags[[x]],
                RDataDateAdded=RDataDateAdded,
                RDataPath=.RDataPaths[[x]],
                Recipe=NA_character_,
                DispatchClass=DispatchClass,
                PreparerClass=.package,
                Location_Prefix=Location_Prefix
            )
        })
    })

    ## if we've got this far, the file is valid
    TRUE
}

## Used for contributed packages, not internal recipes.
makeAnnotationHubMetadata <- function(pathToPackage, fileName=character())
{
    path <- file.path(pathToPackage, "inst", "extdata")
    if (!length(fileName))
        fileName <- list.files(path, pattern="*\\.csv")
    ans <- lapply(fileName,
        function(xx) {
            meta <- .readMetadataFromCsv(pathToPackage, xx)
            .package <- basename(pathToPackage)
            if (is.na(meta$Tags) || !length(meta$Tags))
                stop("please add 'Tags' values to metadata")

            .tags <- strsplit(meta$Tags, ":")
            .tags <- lapply(.tags,
                            FUN<- function(x, packageName){
                                sort(unique(c(x, packageName)))},
                            packageName=.package)
            if (any(unlist(lapply(.tags, FUN=length)) <= 1))
                stop("Add 2 or more Tags to each resource.")

            .RDataPaths <- meta$RDataPath

            lapply(seq_len(nrow(meta)), function(x) {
                with(meta[x, ], AnnotationHubMetadata(
                    Title=Title, Description=Description,
                    BiocVersion=BiocVersion, Genome=Genome,
                    SourceType=SourceType,
                    SourceUrl=SourceUrl,
                    SourceVersion=SourceVersion,
                    Species=Species, TaxonomyId=TaxonomyId,
                    Coordinate_1_based=Coordinate_1_based,
                    DataProvider=DataProvider,
                    Maintainer=Maintainer,
                    RDataClass=RDataClass, Tags=.tags[[x]],
                    RDataDateAdded=RDataDateAdded,
                    RDataPath=.RDataPaths[[x]],
                    Recipe=NA_character_,
                    DispatchClass=DispatchClass,
                    PreparerClass=.package,
                    Location_Prefix=Location_Prefix))
            })
    })
    names(ans) <- fileName
    ans
}

AnnotationHubMetadata <-
    function(AnnotationHubRoot=NA_character_, SourceUrl, SourceType,
        SourceVersion,
        SourceLastModifiedDate= as.POSIXct(NA_character_),
        SourceMd5=NA_character_, SourceSize=NA_real_,
        DataProvider, Title, Description,
        Species, TaxonomyId, Genome, Tags, Recipe,
        RDataClass, RDataDateAdded, RDataPath,
        Maintainer, ..., BiocVersion=BiocManager::version(),
        Coordinate_1_based = TRUE, Notes=NA_character_, DispatchClass,
        Location_Prefix='http://s3.amazonaws.com/annotationhub/')
{
    if (missing(TaxonomyId) | is.na(TaxonomyId))
    {
        if (!is.na(Species) &&
            requireNamespace("AnnotationHubData", quietly=TRUE))
            TaxonomyId <- GenomeInfoDb:::lookup_tax_id_by_organism(Species)
        else
            TaxonomyId <- NA_integer_
    }
    TaxonomyId <- as.integer(TaxonomyId)
    if(!(isSingleInteger(TaxonomyId) || is.na(TaxonomyId)))
        stop(wmsg(paste0("AnnotationHubMetdata objects can contain",
                         " only one taxonomy ID or NA")))

    if(any(is.na(SourceUrl)))
        stop(wmsg(paste0("AnnotationHubMetdata SourceUrl slot cannot",
                         " contain NAs")))

    if (missing(RDataPath)) {
        ## Add two characters: one for substr starting AT clipChars
        ## and one for extra slash
        clipChars <- nchar(Location_Prefix) + 2
        RDataPath <- substr(SourceUrl, clipChars, nchar(SourceUrl))
    }

    RDataDateAdded <-
        as.POSIXct(strsplit(
            as.character(RDataDateAdded), " ")[[1]][1], tz="GMT")

    .checkThatSingleStringAndNoCommas(SourceType)
    .checkThatSingleStringAndNoCommas(Location_Prefix)
    .checkThatSingleStringAndNoCommas(DispatchClass)
    .checkThatSingleStringOrNA(Recipe)
    .checkThatSingleStringOrNA(Genome)
    .checkThatSingleStringOrNA(Species)

    .checkThatSingleStringOrNAAndNoCommas(SourceVersion)
    .checkRDataClassConsistent(RDataClass)
    .checkValidMaintainer(Maintainer)
    .checkFileLengths(RDataPath, DispatchClass)
    .checkValidSingleString(Title)
    .checkValidSingleString(Description)

    new("AnnotationHubMetadata",
        AnnotationHubRoot=AnnotationHubRoot,
        HubRoot=AnnotationHubRoot,
        BiocVersion=BiocVersion,
        Coordinate_1_based=Coordinate_1_based,
        DataProvider=DataProvider,
        Description=Description,
        Genome=Genome,
        Maintainer=Maintainer,
        Notes=Notes,
        RDataClass=RDataClass,
        RDataDateAdded=as.POSIXct(RDataDateAdded),
        RDataPath=RDataPath,
        Recipe=Recipe,
        SourceLastModifiedDate=SourceLastModifiedDate,
        SourceMd5=SourceMd5,
        SourceSize=SourceSize,
        SourceUrl=SourceUrl,
        SourceVersion=SourceVersion,
        SourceType=SourceType,
        Species=Species,
        Tags=Tags,
        TaxonomyId=TaxonomyId,
        Title=Title,
        Location_Prefix=Location_Prefix,
        DispatchClass=DispatchClass,
        ...
    )
}

## -----------------------------------------------------------------------------
## validity
##

.checkSourceurlPrefixesAreValid <- function(url){
    safePrefixes <- c('http://','https://','ftp://','rtracklayer://')
    lst <- lapply(safePrefixes, grepl, x=url)
    if(!all(Reduce('|', lst))){
        stop(wmsg(paste0("sourceurl provided has an invalid prefix (missing ",
                        "protocol). Source urls should be full uris that point ",
                        "to the original resources used in a recipe.")))
    }
}

.checkSourceurlsFreeOfDoubleSlashes <- function(url){
    if(any(grepl("\\w//", url, perl=TRUE))){
        stop(wmsg(paste0("sourceurl provided has a double slash outside of the ",
                         "protocol). Source urls should be working uris that ",
                         "point to the original resources used in a recipe.")))
    }
}

## try to make sure genomes do not contain weird suffixes.. (should be short)
.checkThatGenomeLooksReasonable <- function(genome){
    if(!is.na(genome) && nchar(genome) > 30){
        warning(wmsg("genome provided is suspiciously long. ",
                         "Check to make sure that the genome is legitimate and ",
                         "does not contain unnecessary extensions etc."))
    }
}

#############################################################################
#
# Broken
# Because of the tryCatch this actaully does not fail for bogus class
# fails when can't try isClass which is only missing or NA
#
#############################################################################
.checkRdataclassIsReal <- function(class){
    tryCatch(isClass(class), error = function(err){
        stop("The rdataclass must be a valid R data type. \n",
             conditionMessage(err))})
}

.checkThatSourceTypeSoundsReasonable <- function(sourcetype) {
    expectedSourceTypes <- getValidSourceTypes()
    if(!(sourcetype %in% expectedSourceTypes)) {
        stop(paste0("'SourceType' should be one of: ",
                    paste(expectedSourceTypes, collapse=", "),
                    ".\n Found type: ", sourcetype))
  }
}

.checkThatRDataPathIsOK <- function(rdatapath) {
    ## no spaces are allowed in RDataPath field
    if(any(grepl(" ", rdatapath)))
        stop(wmsg("The string for RDataPath cannot contain spaces."))

    protocolPrefixes <- c('^http://','^https://','^ftp://','^rtracklayer://')
    prefixesFound <- unlist(lapply(protocolPrefixes, FUN=grepl, x=rdatapath))
    if(any(prefixesFound))
        stop(wmsg(paste0("The string for an RDataPath should only contain",
                         " the partial path after the location_Prefix",
                         " (including the protocol) has been trimmed off")))
}

setValidity("AnnotationHubMetadata",function(object) {

    # Validity checks used for both Experiment and AnnotationHub
    .ValidHubs(object)
})


.ValidHubs <- function(object){

    .checkSourceurlPrefixesAreValid(object@SourceUrl)
    .checkSourceurlsFreeOfDoubleSlashes(object@SourceUrl)
    .checkThatGenomeLooksReasonable(object@Genome)
    .checkRdataclassIsReal(object@RDataClass)
    .checkThatSourceTypeSoundsReasonable(object@SourceType)
    if(!is.na(object@TaxonomyId)) GenomeInfoDb:::check_tax_id(object@TaxonomyId)
    .checkThatRDataPathIsOK(object@RDataPath)

}

## ------------------------------------------------------------------------------
## run
##

setMethod("run", "AnnotationHubMetadata",
    function(object, recipeFunction, ...) {
       if (missing(recipeFunction)) {
         temp <- strsplit(recipeName(object), ":::")[[1]]
         functionName <- temp[2]
         pkgName <- temp[1]
         recipeFunction <- get(functionName, envir=getNamespace(pkgName))
       }
       stopifnot(is.function(recipeFunction))
       recipeFunction(object) ## disregard return value
       object
})

## ------------------------------------------------------------------------------
## to / from Json
##

.encodeNA <- function(lst)
{
    rapply(lst, function(elt) {
        isNA <- is.na(elt)
        if (any(isNA))
            ## NA token, coerce to 'character'
            elt[isNA] <- sprintf(".__NA__%s_", class(elt[isNA]))
        elt
    }, how="replace")
}

.decodeNA <- function(lst)
{
    .NAmap <- setNames(list(NA, NA_integer_, NA_character_, NA_real_,
                            NA_complex_),
                       c(".__NA__logical_", ".__NA__integer_",
                         ".__NA__character_", ".__NA__numeric_",
                         ".__NA__complex_"))
    rapply(lst, function(elt) {
        isNA <- elt %in% names(.NAmap)
        if (any(isNA)) {
            type <- elt[isNA][[1]]
            ## reverse coercion
            elt[isNA] <- NA
            elt <- as(elt, class(.NAmap[[type]]))
        }
        elt
    }, "character", how="replace")
}

.isComplete <-
    function(object)
{
    rc <- .Message()

    ## required fields must have non-zero length
    requiredFields <- c("HubRoot",
        "SourceUrl", "Title", "Species", "Genome", "Recipe", "Tags",
        "RDataClass", "SourceVersion",
        "Coordinate_1_based", "Maintainer", "DataProvider",
        "RDataDateAdded")
    values <- metadata(object)[requiredFields]
    idx <- sapply(values, length) == 0L
    if (any(idx))
        rc$append("slots(s) must have non-zero length: %s",
                  paste(sQuote(requiredFields[idx]), collapse=", "))

    ## look up species id in data table
    Species <- metadata(object)$Species
    if (!is.na(Species)) {
        taxonomyId <- try(GenomeInfoDb:::lookup_tax_id_by_organism(Species),
                          silent=TRUE)
        if (inherits(taxonomyId, "try-error"))
            rc$append("'Species' unknown: %s", sQuote(Species))
    }

    ## valid e-mail address
    emailRegex <-
        "\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,4}\\b"
    if (!grepl(emailRegex, metadata(object)$Maintainer, ignore.case=TRUE))
        rc$append("'Maintainer' not a valid email address: %s",
          sQuote(metadata(object)$Maintainer))

    rc$isComplete()
}

toJson <-
    function(x)
{
    lst <- metadata(x)
    ## FIXME: does this matter?
    lst$HubRoot <- NULL       # drop AHRoot

    idx <- grep("(RData|Bioc)Version", names(lst))  # version as character
    lst[idx] <- lapply(lst[idx], as.character)

    idx <- grep("Date", names(lst))
    lst[idx] <- lapply(lst[idx], format, "%Y-%m-%d %T UTC")

    ## encode NA to survive json via mangling
    toJSON(.encodeNA(lst))
}

HubMetadataFromJson <-
    function(path, ahroot=NA_character_)
{
    lst <- .decodeNA(fromJSON(file=path))
    lst <- lst[!sapply(lst, is.null)]         # replace with default values

    lst[["BiocVersion"]] <- package_version(lst$BiocVersion)

    idx <- grep("Date", names(lst))
    lst[idx] <- rapply(lst[idx], function(x) {
        x[!nzchar(x)] <- NA_character_
        as.POSIXct(x)
    }, "character", how="list")

    idx <- sapply(lst, is, "AsIs")
    lst[idx] <- lapply(lst[idx], unclass)

    x <- lst[["Recipe"]];
    setNames(as.character(x), names(x))
    idx <- grep("Size", names(lst))
    lst[idx] <- rapply(lst[idx], as.numeric, how="list")

    ## create AnnotationHubMetadata object
    if (1L == length(lst$Title)) {
        ahm <- do.call(AnnotationHubMetadata, c(HubRoot=ahroot, lst))
        ok <- .isComplete(ahm)
    } else {
        args <- c(list(AnnotationHubMetadata, HubRoot=ahroot), lst)
        ahm <- do.call(Map, args)
        ok <- sapply(ahm, .isComplete)
    }
    if (!all(ok))
        stop("some AnnotationHubMetadata objects incomplete")
    ahm
}

writeJSON <- function(ahroot, metadata, flat=FALSE, filename=NULL)
{
    json <- toJson(metadata)
    sourceFile <- metadata(metadata)$SourceFile[1]
    resourceDir <- dirname(sourceFile)
    if (is.null(filename))
    {
        filename <- .derivedFileName(sourceFile, "json")
    }
    if (flat)
        outfile <- file.path(ahroot, filename)
    else
        outfile <- file.path(ahroot, resourceDir, filename)
    cat(json, "\n", file=outfile)
    outfile
}
