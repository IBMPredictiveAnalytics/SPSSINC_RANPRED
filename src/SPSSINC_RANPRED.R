# module to run an R program as an Extension command

# author__ = "SPSS, JKP"
# version__ = "1.1.0"

# History
# 30-Sep-2008 Original version
# 12-Apr-2013 Rewrite to eliminate Python dependency


helptext="SPSSINC RANPRED
[FOREST=filespec]
[PREDTYPE={RESPONSE*|PROBABILITIES|VOTES} [CHECKMISSING={NO*|YES}
[ID=varname]

[SAVE PREDVALUESDATASET=datasetname] 
[/PRINT PREDICTIONS]
Split files and weight are not honored by this command.

SPSSINC RANPRED /HELP.  prints this information and does nothing else.

Example:
SPSSINC RANPRED.

Calculate predictions from a random forest object in memory or from a
saved R workspace.
'Random Forest' is a trademark of Breiman and Cutler.

FOREST can be specified to name an R workspace saved by
SPSSINC RANFOR containing the estimation information needed for prediction.
By default, the workspace is expected to already be loaded.  This will be
the case if RETAINFOREST was specified in that command.

Categorical  variables in the prediction dataset, as determined by the estimation dataset,  
are automatically converted to factors using the values found in the estimation dataset.  
Category values for the prediction dataset must all exist in the estimation dataset.

CHECKMISSING.  Missing predictor values are not permitted when predicting.  Any
missing values will cause the prediction process to fail for the
entire dataset.  Use CHECKMISSING=YES to check for and remove any
cases with missing values from the prediction dataset.  An ID variable is
required if CHECKMISSING=YES.

ID can specify the name of a variable in the input dataset
to be written to the prediction dataset as an id variable
and used if predictions are displayed.

By default, the predicted response is computed.  For classification trees,
probabilities or votes can be specified.  In this case, a result is computed for
each category.


PREDVALUES can be specified to name a dataset to hold the predicted values.
The name must not already be in use.

/PRINT PREDICTIONS produces a pivot table listing the predictions.

Either or both of PREDVALUES or PRINT PREDICTIONS must be specified.

This extension command requires the R programmability plug-in and 
the R randomForest package.
"

ranpred = function(workspace=NULL,
    programfile=NULL, predtype="response", predvalues=NULL, 
    printpredictions=FALSE, id=NULL, checkmissing=FALSE) {
    #run R randomForest procedure for predictions.

    #Categorical variables are automatically converted to factors based on levels
    # TODO: how to ensure categorical coding is consistent
    
    setuplocalization()
    
    tryCatch(library(randomForest), error=function(e){
        stop(gtxtf("The R %s package is required but could not be loaded.","randomForest"),call.=FALSE)
        }
    )

    if (!is.null(programfile)) {
        print(gtxt("The programfile keyword is no longer supported"))
    }
    if (is.null(predvalues) && !printpredictions) {
        stop(gtxt("Either or both of the predicted values dataset and printpredictions must be specified"),
            call.=FALSE)
    }
    if (!is.null(predvalues)) {
        alldatasets = spssdata.GetDataSetList()
        if (predvalues %in% alldatasets) {
            stop(gtxt("The predicted values output dataset name is already in use"), call.=FALSE)
        }
    }
    if (checkmissing && is.null(id)) {
        stop(gtxt("An ID variable must be specified if the missing data check was requested."), call.=FALSE)
    }
    predtypes=list(response= "response", probabilities="prob", votes="vote")
    predtype = predtypes[predtype]
    if (!is.null(workspace)) {
        workspace = gsub(pattern="\\",replacement="/", x=workspace, fixed=TRUE)
        tryCatch(load(file=workspace),
            error=function(e) {stop(gtxtf("The specified workspace file could not be opened: %s", workspace),
            call.=FALSE)}
        )
    }
    else {
        tbl1values[14] = gtxt("--In memory--")
    }
    classres = tryCatch(class(res), error=function(e) return(""))
    if (classres != "randomForest") {
        stop(gtxt("The R workspace does not contain the expected random forest object"), call.=FALSE)
    }
    StartProcedure("Ranpred", "SPSSINCRANPRED")
    appendloc = length(tbl1lbls)+1
    tbl1lbls[appendloc] = gtxt("New Prediction Dataset")
    tbl1values[appendloc] = ifelse(is.null(predvalues), "--None--", predvalues)
    spsspivottable.Display(tbl1values, gtxt("Random Forest Used for Prediction"), "RANFORPREDSUMMARY",
        caption=gtxt("Random Forest computed by R randomForest package"),
        isSplit=FALSE,
        rowlabels = tbl1lbls,
        collabels=gtxt("Statistics"))
    spsspkg.EndProcedure()

    allvars = strsplit(tbl1values[3], " ")[[1]]   #tbl1values comes from loaded or preserved workspace
    if (is.null(id)) {
        preddta <- spssdata.GetDataFromSPSS(allvars, missingValueToNA = TRUE, 
            keepUserMissing=FALSE, factorMode = "levels")
    } else {
        # If an id variable was specified, it is retrieved as the row names.
        # These always appear as strings, so it will be converted back to
        # numeric or the string length will be adjusted if/when the new dataset is saved
        # based on dictionary information.
        preddta <- spssdata.GetDataFromSPSS(allvars, row.label=id, missingValueToNA = TRUE, 
            keepUserMissing=FALSE, factorMode = "levels")
    }

    # Need to check here, because predictors were not screened via
    # an existingvarlist type in the syntax
    if (length(preddta) != length(allvars)) {
        reqvars = paste(allvars, sep=" ", collapse="\n")
        stop(gtxtf("One or more of the required variables was not found in the active dataset.\nRequired variables:\n%s", reqvars), call.=FALSE)
    }
    if (!is.null(id)) {
        if (id %in% allvars) {
            stop(gtxtf("The id variable specified duplicates the name of a model variable: %s", id), call.=FALSE)
        }
        if (checkmissing) {  # eliminate cases with any missing data (except in row names)
            preddta = preddta[complete.cases(preddta),]
        }
    }

    if (tbl1values[1] == "unsupervised") {
        stop(gtxt("Predictions cannot be made from an unsupervised forest"), call.=FALSE)
    }

    # for regression trees, a single predicted value column is returned
    # for classification trees, one column is returned for each class
    # wrapping in data frame generates case id
    # any missing data in the input will case the prediction call to fail
    pred = data.frame(predict(res, newdata=preddta, type=predtype)) # will fail if type inconsistent with tree
    npredcols = length(pred)
    newdict = list()
    printlbls = list()
    # copy id variable dictionary properties if id was specified
    # The dictionary properties probably do not match the row.names properties :-(
    # We need to match the originals since otherwise it would be hard to match
    # the prediction dataset with the original
    if (!is.null(id)) {
        newdict[1] = spssdictionary.GetDictionaryFromSPSS(id)
    } else {
        newdict[1]=list(c("caseNumber_", gtxt("Case Number (Not Counting Filtered Cases)"), 0, "F8.0", "nominal"))
        printlbls[1] = gtxt("Case Number")
    }
    for (i in 1:npredcols) {
        if (npredcols == 1) {name = paste(spssdict[["varName", 1]], gtxt("pred"), sep="_", collapse="_")
        } else {
            name = paste(spssdict[["varName", 1]], i, sep="_", collapse="_")
        }
        if (predtype == "response") {
            vartype = spssdict[["varType",1]]
            varlbl = "Predicted Value"
            varfmt = spssdict[["varFormat", 1]]
            varlevel = spssdict[["varMeasurementLevel",1]] 
        } else if (predtype == "prob") {
            vartype = 0
            varlbl = paste(gtxt("Probability"), res$classes[i], sep="_")
            varfmt = "F6.3"
            varlevel = "scale"
    } else {  # votes
            vartype = 0
            varlbl = paste(gtxt("Votes"), res$classes[i], sep="_")
            varfmt = "F6.3"
            varlevel = "scale"
    }
        newdict[i+1] = list(c(name, varlbl, vartype, varfmt, varlevel))
        #printlbls[i+1] = varlbl
        printlbls[i] = varlbl
    }
    if (!is.null(predvalues)) {
        tryCatch({
            dict = spssdictionary.CreateSPSSDictionary(newdict)
            spssdictionary.SetDictionaryToSPSS(predvalues, dict)
            if (!is.null(id)) { # row names will be converted to SPSS dict spec automatically
                spssdata.SetDataToSPSS(predvalues, data.frame(row.names(preddta), pred))
            } else {
                spssdata.SetDataToSPSS(predvalues, data.frame(row.names(pred), pred))
            }
            spssdictionary.EndDataStep()},
            error=function(e) {print(e)
            print(gtxtf("Failed to create predicted values dataset: %s", predvalues))}
        )
    }
    if (printpredictions) {
        if (!is.null(id)) {
            #spsspivottable.Display(data.frame(row.names(preddta), pred), 
            spsspivottable.Display(pred, rowlabels=row.names(preddta), 
                rowdim=id, hiderowdimtitle=FALSE,
                gtxt("Predicted Values or Probabilities"), "RANFORPREDICTIONS",
                isSplit=FALSE, collabels=printlbls)
        } else {
            spsspivottable.Display(pred, gtxt("Predicted Values or Probabilities"), 
            "RANFORPREDICTIONS", isSplit=FALSE, collabels=printlbls)
        }
    }
    rm(preddta)
    rm(pred)
}


# localization initialization
setuplocalization = function() {
    # enable localization		
    domain <- "SPSSINC_RANPRED"
    fpath = Find(file.exists, file.path(.libPaths(), paste(domain, ".R", sep="")))
    bindtextdomain(domain, file.path(dirname(fpath), domain, "lang"))
}

# override for api to account for extra parameter in V19 and beyond
StartProcedure <- function(procname, omsid) {
    if (substr(spsspkg.GetSPSSVersion(),1, 2) >= 19) {
        spsspkg.StartProcedure(procname, omsid)
    }
    else {
        spsspkg.StartProcedure(omsid)
    }
}
gtxt <- function(...) {
    return(gettext(...,domain="SPSSINC_RANPRED"))
}

gtxtf <- function(...) {
    return(gettextf(...,domain="SPSSINC_RANPRED"))
}

Run = function(args) {
    #Execute the SPSSINC RANPRED command

    cmdname=args[[1]]
    args = args[[2]]
    oobj = spsspkg.Syntax(list(
        spsspkg.Template("FOREST", subc="",  ktype="literal", var="workspace"),
        spsspkg.Template("PREDTYPE", subc="",  ktype="str", var="predtype",
            vallist=list("response", "probabilities", "votes")),
        spsspkg.Template("ID", subc="", ktype="varname", var="id"),
        spsspkg.Template("PREDICTIONS", subc="PRINT",  ktype="bool", var="printpredictions"),
        spsspkg.Template("PROGRAMFILE", subc="SAVE", ktype="literal", var="programfile"),
        spsspkg.Template("PREDVALUES", subc="SAVE", ktype="literal", var="predvalues"),

        spsspkg.Template("CHECKMISSING", subc="", ktype="bool", var="checkmissing"),
        spsspkg.Template("HELP", subc="", ktype="bool")
    ))

    # A HELP subcommand overrides all else
    if ("HELP" %in% attr(args,"names")) {
        ###writeLines(helptext)
        helper(cmdname)
    }
    else {
        res <- spsspkg.processcmd(oobj, args, "ranpred")
    }
}

helper = function(cmdname) {
    # find the html help file and display in the default browser
    # cmdname may have blanks that need to be converted to _ to match the file
    
    fn = gsub(" ", "_", cmdname, fixed=TRUE)
    thefile = Find(file.exists, file.path(.libPaths(), fn, "markdown.html"))
    if (is.null(thefile)) {
        print("Help file not found")
    } else {
        browseURL(paste("file://", thefile, sep=""))
    }
}
if (exists("spsspkg.helper")) {
assign("helper", spsspkg.helper)
}