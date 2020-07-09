################################################################################
## Tests to just see if we can run all of our recipes

ahroot <- getwd()
BiocVersion <- as.character(BiocManager::version())

## No longer used:
## test_HaemCodeImportPreparer_recipe
## test_Inparanoid8ImportPreparer_recipe
## test_BioPaxImportPreparer_recipe


## FIXME:
## Both UCSC broken because location / format of eutils file has changed;
## See .organismToTaxid()
#test_UCSCChainPreparer_recipe <- function() {
#    ahms = updateResources(ahroot, BiocVersion,
#        preparerClasses = "UCSCChainPreparer",
#        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
#    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
#}
#
#test_UCSC2BitPreparer_recipe <- function() {
#    ahms = updateResources(ahroot, BiocVersion,
#        preparerClasses = "UCSC2BitPreparer",
#        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
#    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
#}

test_EncodeImportPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "EncodeImportPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_EpigenomeRoadmapImportPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "EpigenomeRoadMapPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_dbSNPVCFPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "dbSNPVCFPreparer",
        insert = FALSE, metadataOnly=TRUE, justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_RefNetImportPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "RefNetImportPreparer",
        insert = FALSE, metadataOnly=TRUE)
     checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_ChEAPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
        preparerClasses = "ChEAImportPreparer",
        insert = FALSE, metadataOnly=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_NCBIImportPreparer_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
                               preparerClasses = "NCBIImportPreparer",
                               insert = FALSE, metadataOnly=TRUE,
                               justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_Grasp2Db_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
                           preparerClasses = "Grasp2ImportPreparer",
                           insert = FALSE, metadataOnly=TRUE,
                           justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

## FIXME: add test_EnsemblFasta
test_EnsmblFastaTwoBitToAHM <- function() {
    ahms = updateResources(ahroot, BiocVersion,
                           preparerClasses = "EnsemblTwoBitPreparer",
                           insert = FALSE, metadataOnly = TRUE,
                           justRunUnitTest = TRUE, release = 96)
    checkTrue(class(ahms[[1]]) == "AnnotationHubMetadata")
    # fails before ensembl release 96
    checkException(
        updateResources(ahroot, BiocVersion,
                        preparerClasses = "EnsemblTwoBitPreparer",
                        insert = FALSE, metadataOnly = TRUE,
                        justRunUnitTest = TRUE, release = 85)
        )
}


test_EnsemblGtfToGRanges_recipe <- function() {
    ahms = updateResources(ahroot, BiocVersion,
                           preparerClasses = "EnsemblGtfImportPreparer",
                           insert = FALSE, metadataOnly=TRUE,
                           release = "96", justRunUnitTest=TRUE)
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
    # fails before ensembl release 96
    checkException(
        updateResources(ahroot, BiocVersion,
                        preparerClasses = "EnsemblGtfImportPreparer",
                        insert = FALSE, metadataOnly=TRUE,
                        release = "85", justRunUnitTest=TRUE)
        )
}

test_GencodeGFF <- function() {
    ahms = updateResources(ahroot, BiocVersion,
                           preparerClasses = "GencodeGffImportPreparer",
                           insert = FALSE, metadataOnly=TRUE,
                           justRunUnitTest=TRUE, release="31")
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}

test_GencodeFasta <- function() {
    ahms = updateResources(ahroot, BiocVersion,
                           preparerClasses = "GencodeFastaImportPreparer",
                           insert = FALSE, metadataOnly=TRUE,
                           justRunUnitTest=TRUE, species="Human",
                           release="23")
    checkTrue(class(ahms[[1]])=="AnnotationHubMetadata")
}
