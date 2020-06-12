# Build file name, get fileList
fNameBuilder <- function(dgpR){
    fName <- lapply(cacheList(dgpR, fName=TRUE)[[1]],         # intermediate
                    function(x) {eval(parse(text=x))}) %>% 
                unlist(.) 
    fName <- paste0(fName, collapse="_") %>% 
                paste0(dgpR, "_", .)
    
    # Check to see if results in memory
    fList <- list.files("__simRslts", paste0("^", fName))
    
    return(list(fName, fList))    
}
#****************************************************************************
# Check if file exists
fileCheck <- function(fName, fList, dgpR, reps, seed, canLoad, checkOnly=FALSE){
    # ...check next to see whether user's reps is <= saved reps
    ## (can do this before checking seed b/c, if the largest batch we
    ## have on disk has fewer draws than user requested, will have to
    ## rerun everything anyways, regardless of what the canLoad value is.)
    fileReps <- stringr::str_extract_all(fList, "s[0-9]*[^a-zA-z]") %>% 
                gsub("s", "", .) %>% 
                as.numeric %>% max  # there *shouldn't* be multiples of a scenario, but JIC
    
    # Catch condition for no file reps, JIC
    if(is.na(fileReps))  fileReps <-0
    
    # set rerun obj value for later sim run if() chunk
    if(fileReps>=reps){
        # Check to see if any of the seeds match
        fSeed <- stringr::str_extract_all(fList, "r[0-9]*[^a-zA-z]") %>% 
                    gsub("r", "", .) %>% 
                    as.numeric
        
        # If not, check whether canLoad  
        if(!(seed %in% fSeed)){
            # If we're just checking to see if things are possible, 
            # give answer here and break out of the rest
            if(checkOnly==TRUE){
                resExist <- TRUE
                retList <- list(NULL, NULL)
                
                return(list(list(NULL, NULL), 
                            retList, 
                            resExist))
                ## EXITS THE FUNCTION
            }
            else resExist <- NULL
            
            # If true, means user says we can use saved results.
            ## Set seed to match that of the saved file w/most sim results; proceed
            if(canLoad==TRUE){
                # Set rerun object
                rerun <- FALSE
                    # resList, resExist set inside next major if() block
                
                # Grab seed from file w/largest # of sims on disk.
                winSeeds <- fList[stringr::str_detect(fList, paste0("s", fileReps))] %>%
                                stringr::str_extract_all(., "r[0-9]*[^a-zA-z]") %>% 
                                gsub("r", "", .) %>% 
                                as.numeric %>% 
                                sort
                
                # Update seed input widget 
                ## (just take first element of winSeeds list--they
                ## all have the same number of draws at this point.
                ## You've sorted it above s.t. the lowest seed will
                ## be first, just for some degree of attempted
                ## pseudo-consistency.)
                ifelse(dgpR=="true", {stub <- " "}, {stub <- paste0("_", dgpR)})
                str <- paste0("updateNumericInput(session, 'seed", stub, "', value=", winSeeds[1], ")")
                eval(parse(text=str))
            
                # Quick modal to make user aware of switch
                showModal(modalDialog(
                    title=HTML("<i class='fas fa-info-circle'></i> FYI: Seed Switched"),
                    HTML(paste0("<p>Your random seed value has been switched from
                    <span class='seedVal'>", seed, "</span> to <span class='seedVal'>", winSeeds[1],"
                    </span>.</p>
                    
                    <p>The app loaded ", reps," sim draws.</p>
                    
                    <p>These things occurred because you checked the \"Load
                    saved results\" checkbox.</p>
                    
                    <p>If you didn't want
                    this behavior, and instead want simulations
                    with your original seed, set the seed to your
                    preferred value again, uncheck the checkbox,
                    and run the simulations again.</p>
                    
                    <p>(No further action is required on your part.)</p>"))
                ))
                    
                seed <- winSeeds[1]
                
            # If false, user either hit Esc or explicitly said to run new sims.
            ## Prepare to run new sims        
            } else{
                rerun <- TRUE
                retList <- list(NULL, NULL)
                resExist <- NULL
            }
            
        # If the seeds do match, non-event.
        } else  rerun <- FALSE # retList, resExist defined in next if() block

        
        # If we're good to load everything, do it.
        if(rerun==FALSE){
            incProgress(reps, message = "Loading results") 
            
            # load
            load(paste0("__simRslts/", fName, "_r", seed, "_s", fileReps, ".RData"))
    
            # If the req # of reps is fewer than what's saved, chop off the rest
            if(fileReps!=reps){
                processed   <- lapply(processed  , '[', c(1:reps),) 
                vcovMatProc <- lapply(vcovMatProc, '[', c(1:reps) ) 
            }
            
            retList <- list(processed, vcovMatProc)
            resExist <- NULL
        }
    }
    else{
        rerun <- TRUE
        retList <- list(NULL, NULL)
        resExist <- NULL
    }
    
    return(list(list(rerun, fileReps),
                retList, 
                resExist))
}