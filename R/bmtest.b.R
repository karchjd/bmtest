
# This file is a generated template, your changes will not be overwritten

bmtestClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "bmtestClass",
    inherit = bmtestBase,
    private = list( .run=function() {

        groupVarName <- self$options$group
        depVarNames <- self$options$vars
        varNames <- c(groupVarName, depVarNames)

        if (is.null(groupVarName) || length(depVarNames) == 0)
            return()

        data <- jmvcore::select(self$data, varNames)

        for (name in depVarNames)
            data[[name]] <- jmvcore::toNumeric(data[[name]])
        data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))

        ttestTable <- self$results$bmtest

        confInt <- self$options$ciWidth / 100

        if (any(depVarNames == groupVarName))
            jmvcore::reject(.("Grouping variable '{a}' must not also be a dependent variable"),
                            code="a_is_dependent_variable", a=groupVarName)

        # exclude rows with missings in the grouping variable
        data <- data[ ! is.na(data[[groupVarName]]),]

        groupLevels <- base::levels(data[[groupVarName]])

        if (length(groupLevels) != 2)
            jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
                            code="grouping_var_must_have_2_levels", a=groupVarName)

        if (self$options$miss == "listwise") {
            data <- jmvcore::naOmit(data)
            if (dim(data)[1] == 0)
                jmvcore::reject("Grouping variable '{a}' has less than 2 levels after missing values are excluded",
                                code="grouping_var_must_have_2_levels", a=groupVarName)
        }

        ## Hypothesis options checking
        if (self$options$hypothesis == 'oneGreater')
            Ha <- "greater"
        else if (self$options$hypothesis == 'twoGreater')
            Ha <- "less"
        else
            Ha <- "two.sided"

        for (depName in depVarNames) {

            dataTTest <- data.frame(dep=data[[depName]], group=data[[groupVarName]])

            if (self$options$miss == "perAnalysis")
                dataTTest <- jmvcore::naOmit(dataTTest)

            if (is.factor(dataTTest$dep)) {
                res <- createError(.('Variable is not numeric'))
            }
            else if (any(is.infinite(dataTTest$dep))) {
                res <- createError('Variable contains infinite values')
            }
            else {
                if (self$options$hypothesis == 'oneGreater') {
                    HA <- "greater"
                }
                else if (self$options$hypothesis == 'twoGreater') {
                    HA <- "less"
                }
                else {
                    HA <- "two.sided"
                }


                if(self$options$asym){
                    res <- try(suppressWarnings(
                        brunnermunzel::brunnermunzel.test(dep ~ group, data=dataTTest,
                                                          alternative=HA,
                                                          alpha=1-confInt)
                    ), silent=TRUE)
                    statistic <- res$statistic
                    parameter <- res$parameter
                    p_value <- res$p.value
                    mm <- res$estimate
                    cil <- res$conf.int[1]
                    ciu <- res$conf.int[2]
                    if(HA == "greater" || HA == "two.sided"){
                        mm <- 1-mm
                        cil_orig <- cil
                        cil <- 1-ciu
                        ciu <- 1-cil_orig
                    }



                    if ( ! jmvcore::isError(res)) {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[asym]"=statistic,
                            "df[asym]"=parameter,
                            "p[asym]"=p_value,
                            "relEff[asym]"=mm,
                            "cil[asym]"=cil,
                            "ciu[asym]"=ciu
                        ))

                    } else {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[asym]"=NaN,
                            "df[asym]"='',
                            "p[asym]"='',
                            "cil[asym]"='',
                            "ciu[asym]"=''
                        ))

                        message <- jmvcore::extractErrorMessage(res)
                        if (message == 'grouping factor must have exactly 2 levels')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'not enough observations')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'cannot compute confidence interval when all observations are tied')
                            message <- .('All observations are tied')

                        ttestTable$addFootnote(rowKey=depName, 'stat', message)
                    }
                }

                if(self$options$randomPerm){
                    res <- try(suppressWarnings(
                        nparcomp::npar.t.test(dep ~ group, data=dataTTest,
                                              alternative=HA,
                                              conf.level=confInt,
                                              method = "permu",
                                              info = FALSE)
                    ), silent=TRUE)

                    res_sel <- as.numeric(res$Analysis["id", ])
                    names(res_sel) <- colnames(res$Analysis)
                    statistic <- res_sel["Statistic"]
                    parameter <- ""
                    p_value <- res_sel["p.value"]
                    mm <- res_sel["Estimator"]
                    cil <- res_sel["Lower"]
                    ciu <- res_sel["Upper"]
                    if(HA == "greater" || HA == "two.sided"){
                        mm <- 1-mm
                        cil_orig <- cil
                        cil <- 1-ciu
                        ciu <- 1-cil_orig
                    }



                    if ( ! jmvcore::isError(res)) {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[randomPerm]"=statistic,
                            "df[randomPerm]"=parameter,
                            "p[randomPerm]"=p_value,
                            "relEff[randomPerm]"=mm,
                            "cil[randomPerm]"=cil,
                            "ciu[randomPerm]"=ciu
                        ))

                    } else {

                        ttestTable$setRow(rowKey=depName, list(
                            "stat[randomPerm]"=NaN,
                            "df[randomPerm]"='',
                            "p[randomPerm]"='',
                            "cil[randomPerm]"='',
                            "ciu[randomPerm]"=''
                        ))

                        message <- jmvcore::extractErrorMessage(res)
                        if (message == 'grouping factor must have exactly 2 levels')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'not enough observations')
                            message <- .('One or both groups do not contain enough observations')
                        else if (message == 'cannot compute confidence interval when all observations are tied')
                            message <- .('All observations are tied')

                        ttestTable$addFootnote(rowKey=depName, 'stat', message)
                    }
                }
            }

        }}  ,
        .init=function() {

            hypothesis <- self$options$hypothesis
            groupName <- self$options$group

            groups <- NULL
            if ( ! is.null(groupName))
                groups <- base::levels(self$data[[groupName]])
            if (length(groups) != 2)
                groups <- c('Group 1', 'Group 2')

            table <- self$results$bmtest

            ciTitleString <- '{ciWidth}% Confidence Interval'

            ciTitle <- jmvcore::format(ciTitleString, ciWidth=self$options$ciWidth)
            table$getColumn('ciu[asym]')$setSuperTitle(ciTitle)
            table$getColumn('cil[asym]')$setSuperTitle(ciTitle)
            table$getColumn('ciu[randomPerm]')$setSuperTitle(ciTitle)
            table$getColumn('cil[randomPerm]')$setSuperTitle(ciTitle)


            table$getColumn("relEff[asym]")$setTitle(jmvcore::format("P({} > {}) + \u00BDP({} = {})", groups[1], groups[2], groups[1], groups[2]))
            table$getColumn("relEff[randomPerm]")$setTitle(jmvcore::format("P({} > {}) + \u00BDP({} = {})", groups[1], groups[2], groups[1], groups[2]))

            if (hypothesis == 'oneGreater')
                table$setNote("hyp", jmvcore::format("H\u2090 P({} > {}) + \u00BDP({} = {}) > \u00BD", groups[1], groups[2], groups[1], groups[2]))
            else if (hypothesis == 'twoGreater')
                table$setNote("hyp", jmvcore::format("H\u2090 P({} < {}) + \u00BDP({} = {}) > \u00BD", groups[1], groups[2], groups[1], groups[2]))
            else
                table$setNote("hyp", jmvcore::format("H\u2090 P({} > {}) + \u00BDP({} = {}) \u2260 \u00BD", groups[1], groups[2], groups[1], groups[2]))
        }
    )
)


