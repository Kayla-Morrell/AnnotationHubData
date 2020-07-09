### =========================================================================
### makeEnsemblFastaAHM() and ensemblFastaToFaFile()
### -------------------------------------------------------------------------
###

## Adjust this expression in order to save painful-reprocessing of older files.
## .ensemblReleaseRegex <- ".*release-(69|7[[:digit:]]|8[[:digit:]])"
## .ensemblReleaseRegex <- ".*release-(79|8[[:digit:]])"
## for a speed run just do one set
## .ensemblReleaseRegex <- ".*release-81"

## list directories below url/dir satisfying regex
.ensemblDirUrl <-
    function(url, dir, regex)
{
    lst <- .listRemoteFiles(url)
    releases <- paste0(url, lst)
    paste(grep(regex, releases, value=TRUE), dir, sep="/")
}

## NOTE: httr >= 1.2.0 doesn't support ftp last modified date and size
## FIXME: This should be combined with .httrFileInfo() and .ftpFileInfo()
.ensemblMetadataFromUrl <- function(sourceUrl, twobit=FALSE, http=FALSE) {
    releaseRegex <- ".*(release-[[:digit:]]+).*"
    if (!twobit){
        title <- sub("\\.gz$", "", basename(sourceUrl))
    }else{
        title <- sub("\\.fa\\.gz$", ".2bit", basename(sourceUrl))
    }
    root <- setNames(rep(NA_character_, length(sourceUrl)), title)

    releaseNum <- sub("release-", "", sub(releaseRegex, "\\1", sourceUrl[1]))

    # as of release 96 a file is present with species index for mappings
    species_index <- GenomeInfoDb:::fetch_species_index_from_Ensembl_FTP(release=releaseNum)

    species <- vapply(strsplit(sourceUrl, '/'), function(x) x[[7]], character(1))
    genome <- vapply(species, FUN.VALUE=character(1), USE.NAMES=FALSE,
                     FUN=function(spc, tbl){
                         message(spc, "\n")
                         tbl[tbl$species == spc, "assembly"]
                     }, tbl=species_index)
    taxonomyId <- vapply(species, FUN.VALUE=integer(1), USE.NAMES=FALSE,
                     FUN=function(spc, tbl){
                         message(spc, "\n")
                         tbl[tbl$species == spc, "taxonomy_id"]
                     }, tbl=species_index)

    species <- sub("_", " ", species,fixed=TRUE)

    if (http) {
       ftpInfo <- .httrFileInfo(sourceUrl)
       sourceSize <- ftpInfo$size
       sourceLastModDate <- ftpInfo$date
    } else {
        sourceSize <- as.numeric(NA)
        sourceLastModDate <- as.POSIXct(NA)
    }

    list(annotationHubRoot = root, title=title, species = species,
         taxonomyId = as.integer(taxonomyId),
         genome = genome,
         sourceSize=sourceSize,
         sourceLastModifiedDate=sourceLastModDate,
         sourceVersion = sub(releaseRegex, "\\1", sourceUrl))
}

.ensemblFastaTypes <-
    c("cdna\\.all", "dna_rm\\.toplevel", "dna_sm\\.toplevel",
      "dna\\.toplevel", "ncrna", "pep\\.all")

## get urls
.ensemblFastaSourceUrls <-
    function(baseUrl, baseDir, regex, baseTypes=.ensemblFastaTypes)
{
    want <- .ensemblDirUrl(baseUrl, baseDir, regex)

    .processUrl <- function(url) {
        listing <- .ftpDirectoryInfo(url)

        subdirIdx <- grepl(".*/.*:", listing)
        subdir <- sub("^.{2}(.*):$", "\\1", listing[subdirIdx])
        fileTypes <- paste(baseTypes, collapse="|")
        pat <- sprintf(".*(%s)\\.fa\\.gz$", fileTypes)

        fastaIdx <- grepl(pat, listing)
        fasta <- sub(".* ", "", listing[fastaIdx])

        ## match subdir w/ fasta
        subdir <- subdir[cumsum(subdirIdx)[fastaIdx]]

        ## Prefer "primary_assembly" to "toplevel" resources.
        organisms <- unique(sub("(.+?)\\..*", "\\1", fasta, perl=TRUE))
        keepIdxList <- sapply(organisms, function(x) {
            orgFiles <- fasta[grep(paste0("^", x, "\\."), fasta)]
            reBoth <- paste0("dna", c("_rm", "_sm", ""),
                "\\.(primary_assembly|toplevel)\\.")
            toplevelIdx <-
                sapply(reBoth, function(x) length(grep(x, orgFiles)) > 1)
            reToplevel <- paste0("dna", c("_rm", "_sm", ""),
                "\\.toplevel\\.")[toplevelIdx]

            isRedundant <-
                sapply(reToplevel, function(x) grepl(x, orgFiles))
            retVal <- rep(TRUE, length(orgFiles))
            if (!is.null(dim(isRedundant))) {
              retVal <- !apply(isRedundant, 1, any)
            }

            retVal
        })
        keepIdx <- base::unlist(keepIdxList)
        fasta <- fasta[keepIdx]
        subdir <- subdir[keepIdx]

        sprintf("%s%s/%s", url, subdir, fasta)
    }
    res <- base::unlist(lapply(want, .processUrl), use.names=FALSE)

    if (length(res) == 0) {
        txt <- sprintf("no fasta files at %s",
                       paste(sQuote(want), collapse=", "))
        stop(paste(strwrap(txt, exdent=2), collapse="\n"))
    }
    res
}

## metadata generator
makeEnsemblFastaToAHM <-
    function(currentMetadata, baseUrl = "ftp://ftp.ensembl.org/pub/",
             baseDir = "fasta/", release,
             justRunUnitTest = FALSE, BiocVersion = BiocManager::version())
{
    time1 <- Sys.time()
    regex <- paste0(".*release-", release)
    sourceUrl <- .ensemblFastaSourceUrls(baseUrl, baseDir, regex)
    if (justRunUnitTest)
        sourceUrl <- sourceUrl[1:5]

    sourceFile <- sub(baseUrl, "ensembl/", sourceUrl)
    meta <- .ensemblMetadataFromUrl(sourceUrl)
    dnaType <- local({
        x <- basename(dirname(sourceFile))
        sub("(dna|rna)", "\\U\\1", x, perl=TRUE)
    })
    description <- paste("FASTA", dnaType, "sequence for", meta$species)

    ## rdatapaths db table needs an extra row for the index file
    rdataPath <- sub(".gz$", ".bgz", sourceFile)
    rdps <- rep(rdataPath, each=3)
    rdatapaths <- split(rdps, f=as.factor(rep(1:length(rdataPath),each=3)))
    ## second record of each set becomes the '.fai' file
    rdatapaths <- lapply(rdatapaths,
                         function(x){x[2] <- paste0(x[2],".fai") ; x[2] <-
                                         paste0(x[3],".gzi") ; return(x)})

    Map(AnnotationHubMetadata,
        Description=description,
        Genome=meta$genome,
        RDataPath=rdatapaths,
        SourceUrl=sourceUrl,
        SourceVersion=meta$sourceVersion,
        Species=meta$species,
        TaxonomyId=meta$taxonomyId,
        Title=meta$title,
        SourceSize=meta$sourceSize,
        SourceLastModifiedDate=meta$sourceLastModifiedDate,
        MoreArgs=list(
          BiocVersion=BiocVersion,
          Coordinate_1_based = TRUE,
          DataProvider="Ensembl",
          Maintainer = "Bioconductor Maintainer <maintainer@bioconductor.org>",
          SourceType="FASTA",
          DispatchClass="FaFile",
          RDataClass=c("FaFile", "FaFile", "FaFile"),
          RDataDateAdded=Sys.time(),
          Recipe="AnnotationHubData:::ensemblFastaToFaFile",
          Tags=c("FASTA", "ensembl", "sequence")))
}

## Used in makeEnsemblFastaAHM() and makeGencodeFastaToAHM():
## Unzips .gz file, indexes it and saves as .rz and .rz.fai.
.fastaToFaFile <- function(ahm)
{
    ## target output file
    faOut <- outputFile(ahm)[[1]]
    srcFile <- sub('.bgz$','.gz',faOut)
    ## unzip and index
    bgzip(srcFile)
    indexFa(faOut)
}

ensemblFastaToFaFile <- function(ahm)
{
    .fastaToFaFile(ahm)
}

## create dispatch class and newResources() method
makeAnnotationHubResource("EnsemblFastaImportPreparer", makeEnsemblFastaToAHM)
