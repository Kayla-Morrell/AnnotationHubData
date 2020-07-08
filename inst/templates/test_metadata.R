context("metadata validity")

test_that("metadata is valid",
{
    metadata <- system.file("extdata", "metadata.csv", package = "{{package}}")
    expect_true(test{{type}}Metadata("{{package}}", metadata))


    #fl <- tempfile()
    #dir.create(fl)
    #hub_create_package(fl, {{type}}, TRUE)

    #fl <- tempdir()
    #hub_create_package(paste0(fl, "/tstPkg"), {{type}}, TRUE)
    ## this should error out because there are no resources
    #makeAnnotationHubMetadata(fl)
    #expect_error(do.call(paste0("make", {{type}}, "Metadata"), 
    #    paste0(fl, "/tstPkg")))

    ## the idea is a list of the input for each resource should be the output
    ## from make*HubMetadata()
    ## used examples from the vignettes to get the correct output
    ## try running hub_add_resource() and then test the output?
})
