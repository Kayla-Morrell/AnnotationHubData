### =========================================================================
### HTTP and FTP helpers
### -------------------------------------------------------------------------
###

## Remove remove "\n" when inserting long text (> 80 chars)
.expandLine <- function(x)
    gsub("[[:space:]]{2,}"," ", x)

## -----------------------------------------------------------------------------
## HTTP
## 

## Parses xml from a http page into a data.frame of filenames, 
## List filenames or filenames ending with an extension on http page.
## Also reads the md5sum in "md5sum.txt" on the same http page.
.httrRead <- function(url, xpathString="//pre/a/text()", 
                      extension=NA_character_, getmd5sum=FALSE) {
    tryCatch({
        result <- httpGET(url)
        html <- htmlParse(result, asText=TRUE)

        fls <- getNodeSet(html, xpathString)
        if (is(fls, "XMLNodeSet"))
            fls <- vapply(fls, xmlValue, character(1L))

        md5exists <- length(grep("md5sum.txt", fls))!=0
        remove <- c("Name", "Size", "Last modified", "Description",
                    "Parent Directory", "referenceSequences/",
                    "files.txt", "md5sum.txt", "supplemental/")
        fls <- fls[!fls %in% remove ]

        ## filter by extension
        if(!is.na(extension)){
            fls <- fls[grepl(paste0(extension, "$"), fls)]
        }

        ## UCSC chain and 2bit files have a file called md5sum.txt
        ## col1=md5sum, col2=filename
        ## note : not all chain files have md5sum on UCSC website!
        if(getmd5sum & md5exists & length(fls!=0)) {
            df <- read.table(paste0(url, "/", "md5sum.txt"), header=FALSE,
                             stringsAsFactors=FALSE)
            md5sum <- df[match(fls, df[,2]),1]
            df <- data.frame(files=fls, md5sum=md5sum,  stringsAsFactors=FALSE)
        } else
            df <- data.frame(files=fls, stringsAsFactors=FALSE)
        df

    }, error=function(err) {
        warning(basename(url), ": ", conditionMessage(err))
        url=character()
    })
}


## Returns data.frame with fileurl, last modified date and file size
.httrFileInfo <- function(urls, verbose=TRUE) {
    result <- lapply(urls, function(f){
        if(verbose)
            message(paste0("getting file info: ", basename(f)))
        tryCatch({
            h = suppressWarnings(
              httpGET(f, nobody=TRUE, filetime=TRUE, header=TRUE))

            nams <- names(h$header)
            if("last-modified" %in% nams)
                 h$header[c("last-modified", "content-length")]
	    else
                c("last-modified"=NA, "content-length"=NA)
        }, error=function(err) {
        warning(basename(f), ": ", conditionMessage(err))
        list("last-modified"=character(), "content-length"=character())
        })
    })

    size <- as.numeric(sapply(result, "[[", "content-length"))
    date <- strptime(sapply(result, "[[", "last-modified"),
             "%a, %d %b %Y %H:%M:%S", tz="GMT")

    data.frame(fileurl=urls, date, size, stringsAsFactors=FALSE)
}

## -----------------------------------------------------------------------------
## FTP 
## 

## Returns a data.frame with fileurl, last modified date and file size.
## 'extension' can be a single file name with extension or just the extension.
.ftpFileInfo <- function(url, extension, verbose=FALSE) {

    if (verbose)
        message(paste0("creating urls ..."))

    result <- lapply(url, function(ul) {
        message(ul)
        N.TRIES = 3L
        while (N.TRIES > 0L) {
            con <- tryCatch(getURL(ul), error=identity)
            if (!inherits(con, "error"))
                break
            Sys.sleep(30)
            N.TRIES <- N.TRIES - 1L
        }
        if (N.TRIES == 0L) {
            stop("'getURL()' failed:",
                 "\n  URL: ", ul,
                 "\n  error: ", conditionMessage(con))
        }

        txt <- read.table(text=con, stringsAsFactors=FALSE, fill=TRUE)

        files <- txt[[9]]
        if (verbose)
            message(basename(ul))

        pattern <- paste(paste0(extension, "$"), collapse="|")
        keep <- !grepl("00-", files) & grepl(pattern, files)
        txt <- txt[keep, ]
        if (nrow(txt) == 0L)
            return(data.frame(fileurl=character(), 
                              date=as.POSIXct(character()),
                              size=numeric()))

        # last modified date and size
        dateraw <- apply(txt, 1, function(xx) paste(xx[6], xx[7], xx[8]))
        datestring <- lapply(dateraw, function(xx) {
            as.POSIXct(strptime(xx, format="%b %e %H:%M", tz="GMT"))
        })
        if (any(is.na(datestring))) {
            datestring <- lapply(dateraw, function(xx) {
                as.POSIXct(strptime(xx, format="%b %e %Y", tz="GMT"))
            })
        }

        data.frame(fileurl=paste0(ul, txt[[9]]), date=do.call(c, datestring), 
                   size=as.numeric(txt[[5]]), stringsAsFactors=FALSE)
    })

    do.call(rbind, result)
}

.parseDirInfo <- function(info) {
    readLines(textConnection(trimws(info)))
}

# Return unparsed directory listing as character vector
.ftpDirectoryInfo <- function(someUrl, filesOnly=FALSE) {
    curlHandle <- getCurlHandle(customrequest="LIST -R")
    info <- getURL(someUrl, curl=curlHandle)
    .parseDirInfo(info)
}

## Return just the names of the files in an FTP directory
## Note, this will not do any cleaning of symlinks
.listRemoteFiles <- function(someUrl){
    curlHandle <- getCurlHandle(dirlistonly=TRUE)
    info <- getURL(someUrl, curl=curlHandle)
    .parseDirInfo(info)
}
