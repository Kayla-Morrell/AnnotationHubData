### =========================================================================
### makeNCBIToOrgDbs ('non-standard' OrgDbs)
### -------------------------------------------------------------------------
###

## This recipe makes 'non-standard' OrgDb sqlite files from data
## at ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/. These OrgDbs are less
## comprehensive than the 'standard' packages available in the
## Bioconductor repo. This code generates 1000 sqlite files.

## This recipe should be run right before a new release. The
## biocversion should be the current devel version, soon to roll over
## to the new release.

## The 'standard' OrgDbs are generated with makeStandardOrgDbsToSqlite.R.

.NCBIMetadataFromUrl <- function(baseUrl, justRunUnitTest, biocVersion) {
    load(system.file('extdata','viableIDs.rda', package='AnnotationForge'))
    ids <- results

    if (justRunUnitTest) ids <- head(ids)
    ## FIXME: need different solution; this subset produces NAs
    if (length(biocVersion) > 1) {
        stop(paste("'biocVersion' must be a single value. Make sure new",
                   "'OrgDbs' go into the CORRECT Bioconductor version!"))
    }
    ## Marc's note:
    ## need to find an alternative to this... old school table of tax Ids
    if (!exists("specData")) {
    load(system.file("data", "specData.rda", package = "GenomeInfoDbData"))
    }
    sd <- specData[!is.na(specData[[3]]),]
    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::lookup_organism_by_tax_id(id, all=TRUE)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- as.numeric(ids[ids %in% sd$tax_id])
    res <- lapply(ids,lookup)
    taxonomyId <-
        as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)

    oriSpecies <- paste(genus, species)
    fullSpecies <- gsub(" ", "_", oriSpecies)

    title <- paste0("org.", fullSpecies, ".eg", ".sqlite")
    rDataPath <- paste0("ncbi/uniprot/",biocVersion,"/",title)

    genome <- setNames(rep("NCBI genomes", length(fullSpecies)), title)
    dateMessage <- paste0('NCBI gene annotations as of ', as.character(date()))
    sourceVersion <- rep(dateMessage, length(fullSpecies))
    description <- paste("NCBI gene ID based annotations about", oriSpecies)
    sourceUrls <- c(baseUrl,"ftp://ftp.uniprot.org/pub/databases/uniprot/current_release/knowledgebase/idmapping/idmapping_selected.tab.gz")
    sourceUrl <- rep(list(sourceUrls), length(fullSpecies))

    tryCatch({
        aws <- system2("aws",
                       args=paste0("s3 ls s3://annotationhub/ncbi/uniprot/",
                           biocVersion," --recursive"), stdout=TRUE, stderr=TRUE)
    }, error=function(e){
        aws <- character(0)
    }, finally={
        if (!exists("aws"))  aws <- character(0)
        aws <- gsub("\\s+", " ", stringr::str_trim(aws))
        aws <- aws[-1]
    })

    if (length(aws)){
        s3titles <- sapply(strsplit(sapply(strsplit(aws, " "),"[[", 4), "/"),"[[",4)
        subset <- (title %in% s3titles)
        if(any(subset)){
            lst <- lapply(list(title=title, species = oriSpecies,
                               taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
                               sourceVersion = sourceVersion,
                               description=description, rDataPath=rDataPath), "[", subset)
        }else{
            lst <- list(title=title, species = oriSpecies,
                        taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
                        sourceVersion = sourceVersion,
                        description=description, rDataPath=rDataPath)
        }

    }else{

        lst <- list(title=title, species = oriSpecies,
                    taxonomyId = taxonomyId, genome = genome, sourceUrl=sourceUrl,
                    sourceVersion = sourceVersion,
                    description=description, rDataPath=rDataPath)
    }

    lst
}

needToRerunNonStandardOrgDb <- function(biocVersion =  BiocManager::version(),
                                        baseUrl =
                                        "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/"){
    load(system.file('extdata','viableIDs.rda', package='AnnotationForge'))
    ids <- results

    ## FIXME: need different solution; this subset produces NAs
    if (length(biocVersion) > 1) {
        stop(paste("'biocVersion' must be a single value. Make sure new",
                   "'OrgDbs' go into the CORRECT Bioconductor version!"))
    }
    ## Marc's note:
    ## need to find an alternative to this... old school table of tax Ids
    if (!exists("specData")) {
    load(system.file("data", "specData.rda", package = "GenomeInfoDbData"))
    }
    sd <- specData[!is.na(specData[[3]]),]
    ## need to find offenders
    lookup <- function(id){
        message(paste0("looking up value for: ", id))
        GenomeInfoDb:::lookup_organism_by_tax_id(id, all=TRUE)
    }
    ## Some taxonomy IDs cannot be looked up at all - so discard
    ids <- as.numeric(ids[ids %in% sd$tax_id])
    res <- lapply(ids,lookup)
    taxonomyId <-
        as.integer(as.character(unlist(lapply(res, function(x){x$tax_id}))))
    genus <- unlist(lapply(res, function(x){x$genus}))
    species <- unlist(lapply(res, function(x){x$species}))
    genus <- gsub(" ", "_", genus)
    genus <- gsub("/", "|", genus)
    species <- gsub(" ", "_", species)
    species <- gsub("/", "|", species)

    oriSpecies <- paste(genus, species)
    fullSpecies <- gsub(" ", "_", oriSpecies)

    title <- paste0("org.", fullSpecies, ".eg", ".sqlite")

    tryCatch({
        aws <- system2("aws",
                       args=paste0("s3 ls s3://annotationhub/ncbi/uniprot/",
                           biocVersion," --recursive"), stdout=TRUE, stderr=TRUE)
    }, error=function(e){
        aws <- character(0)
        stop("Cannot access AWS. Unable to determine")
    }, finally={
        if (!exists("aws"))  aws <- character(0)
        aws <- gsub("\\s+", " ", stringr::str_trim(aws))
        aws <- aws[-1]
    })

    if (length(aws)){
        s3titles <- sapply(strsplit(sapply(strsplit(aws, " "),"[[", 4), "/"),"[[",4)
        subset <- !(title %in% s3titles)
        res <- any(subset)
    }else{
        res <- TRUE
    }
    res
}

## STEP 1: make function to process metadata into AHMs
makeNCBIToOrgDbsToAHM <-
    function(currentMetadata, justRunUnitTest = FALSE,
             BiocVersion =  BiocManager::version(),
             baseUrl = "ftp://ftp.ncbi.nlm.nih.gov/gene/DATA/") {
    meta <- .NCBIMetadataFromUrl(baseUrl, justRunUnitTest,
                                 biocVersion=BiocVersion[[1]])

    message("Processing ", length(meta[[1]]), " files.")

    Map(AnnotationHubMetadata,
        Description=meta$description,
        Genome=meta$genome,
        SourceUrl=meta$sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        RDataPath=meta$rDataPath,
        MoreArgs=c(currentMetadata, list(
            BiocVersion=package_version(BiocVersion),
            Coordinate_1_based = TRUE,
            DataProvider = baseUrl,
            Maintainer = paste("Bioconductor Package Maintainer",
                               "<maintainer@bioconductor.org>"),
            RDataClass = "OrgDb",
            DispatchClass = "SQLiteFile",
            SourceType="NCBI/UniProt",
            RDataDateAdded = Sys.time(),
            Recipe = "AnnotationHubData:::NCBIToOrgDbs",
            Tags = c("NCBI", "Gene", "Annotation"))))
}

## STEP 2: Make a recipe function that takes an AnnotationHubRecipe object.
NCBIToOrgDbs <- function(ahm){
    fullSpecies <- ahm@Species
    genus <- unlist(strsplit(fullSpecies,split=" "))[1]
    species <- unlist(strsplit(fullSpecies,split=" "))[2]
    dbname <- makeOrgPackageFromNCBI(version="1.0.0",
                                     maintainer=ahm@Maintainer,
                                     author=ahm@Maintainer,
                                     outputDir=ahm@HubRoot,
                                     tax_id=as.character(ahm@TaxonomyId),
                                     genus=genus,
                                     species=species,
                                     NCBIFilesDir=ahm@HubRoot,
                                     databaseOnly=TRUE,
                                     rebuildCache=TRUE)
    file.rename(from=file.path(ahm@HubRoot, dbname), to=outputFile(ahm))
    outputFile(ahm)
}

## STEP 3: Call the helper to set up the newResources() method
makeAnnotationHubResource("NCBIImportPreparer",
                          makeNCBIToOrgDbsToAHM)
