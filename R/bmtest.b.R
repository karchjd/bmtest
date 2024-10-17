bmtestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "bmtestClass",
        inherit = bmtestBase,
        private = list(
            .init = function() {
                if (is.null(self$options$group)) {
                    return()
                }

                groups <- base::levels(self$data[[self$options$group]])
                if (length(groups) != 2) {
                    groups <- c("Group 1", "Group 2")
                }

                # Initialize results table
                table <- self$results$bmtest
                ciTitleString <- "{ciWidth}% Confidence Interval"
                ciTitle <- jmvcore::format(ciTitleString, ciWidth = self$options$ciWidth)
                ci_columns <- c("ciu[asym]", "cil[asym]", "ciu[randomPerm]", "cil[randomPerm]")

                for (col in ci_columns) {
                    table$getColumn(col)$setSuperTitle(ciTitle)
                }

                relEff_columns <- c("relEff[asym]", "relEff[randomPerm]", "relEff[fullPerm]")
                relEff_title <- jmvcore::format("P\u0302({} < {}) + \u00BDP\u0302({} = {})", groups[1], groups[2], groups[1], groups[2])

                for (col in relEff_columns) {
                    table$getColumn(col)$setTitle(relEff_title)
                }

                hypothesis_note <- switch(self$options$hypothesis,
                                          oneGreater = jmvcore::format("H\u2090 P\u0302({} < {}) + \u00BDP\u0302({} = {}) < \u00BD", groups[1], groups[2], groups[1], groups[2]),
                                          twoGreater = jmvcore::format("H\u2090 P\u0302({} < {}) + \u00BDP\u0302({} = {}) > \u00BD", groups[1], groups[2], groups[1], groups[2]),
                                          jmvcore::format("H\u2090 P\u0302({} < {}) + \u00BDP\u0302({} = {}) \u2260 \u00BD", groups[1], groups[2], groups[1], groups[2])
                )
                table$setNote("hyp", hypothesis_note)
            },
            .run = function() {
                # only do something if grouping variable and dependent variable(s) are specified
                if (is.null(self$options$group) || length(self$options$vars) == 0) {
                    return()
                }

                # functions and constants

                ## give warnings for some combinations of input paramters
                check_parameters <- function(parameters) {
                    if (parameters$ci && !parameters$relEff) {
                        warning("relEff parameter must also be TRUE to get confidence intervals")
                    }

                    if (parameters$ci && parameters$fullPerm) {
                        warning("Confidence intervals not supported for full permutation approach.")
                    }
                }

                ## preprocess input to data.frame
                preprocess <- function(groupVarName, depVarnames) {
                    if (is.null(groupVarName) || length(depVarNames) == 0) {
                        return()
                    }

                    data <- jmvcore::select(self$data, varNames)

                    for (name in depVarNames) {
                        data[[name]] <- jmvcore::toNumeric(data[[name]])
                    }
                    data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))

                    if (any(depVarNames == groupVarName)) {
                        jmvcore::reject(("Grouping variable '{a}' must not also be a dependent variable"),
                                        code = "a_is_dependent_variable", a = groupVarName
                        )
                    }

                    # exclude rows with missings in the grouping variable
                    data <- data[!is.na(data[[groupVarName]]), ]

                    groupLevels <- base::levels(data[[groupVarName]])

                    if (length(groupLevels) != 2) {
                        jmvcore::reject("Grouping variable '{a}' must have exactly 2 levels",
                                        code = "grouping_var_must_have_2_levels", a = groupVarName
                        )
                    }

                    if (self$options$miss == "listwise") {
                        data <- jmvcore::naOmit(data)
                        if (dim(data)[1] == 0) {
                            jmvcore::reject("Grouping variable '{a}' has less than 2 levels after missing values are excluded",
                                            code = "grouping_var_must_have_2_levels", a = groupVarName
                            )
                        }
                    }
                    return(data)
                }

                ## check whether data converted to data frame are valid
                check_data <- function(dataTTest, check_n = FALSE) {
                    if (is.factor(dataTTest$dep)) {
                        return(jmvcore::createError(("Variable is not numeric")))
                    }
                    if (any(is.infinite(dataTTest$dep))) {
                        return(jmvcore::createError("Variable contains infinite values"))
                    }
                    ns <- table(dataTTest$group)
                    if (any(ns == 1)) {
                        return(jmvcore::createError("Not enough observations. Each group must at least contain 2 observations"))
                    }

                    levels_groups <- levels(dataTTest$group)
                    g1 <- dataTTest$dep[dataTTest$group == levels_groups[1]]
                    g2 <- dataTTest$dep[dataTTest$group == levels_groups[2]]
                    if (length(unique(g1)) == 1 || length(unique(g2)) == 1) {
                        return(jmvcore::createError("Not enough unique observations. Each group must at least contain 2 unique observations"))
                    }

                    if (check_n) {
                        total_perm <- choose(sum(ns), ns[1])
                        if (total_perm >= 40116600L) {
                            return(jmvcore::createError("Number of needed permutations too large to be computationally feasible. Use one of the other two options."))
                        }
                    }
                    return(1)
                }

                ## check whether results indicate an error
                check_error_res <- function(res) {
                    if (is.nan(res$parameter)) {
                        return(jmvcore::createError("Results cannot be trusted. Likely caused by an estimated relative effect of very close to 1 or 0.
                                              Try one of the other test types."))
                    } else {
                        return(res)
                    }
                }

                ## function to run a any version (type) of the BM test
                run_analysis <- function(type, dataTTest) {
                    ## set run_test (running the test), extract_res (extracting results from res)
                    ## functions and suffix, full_name, check_n variable to values appropriate
                    ## for the selected type
                    if (type == "asym") {
                        run_test <- function(dataTTest, HA, confInt) {
                            brunnermunzel::brunnermunzel.test(dep ~ group,
                                                              data = dataTTest,
                                                              alternative = HA,
                                                              alpha = 1 - confInt
                            )
                        }
                        extract_res <- function(res) {
                            if (is.nan(res$parameter)) {
                                res$p.value <- NaN
                            }
                            res
                        }
                        suffix <- "[asym]"
                        full_name <- "Asymptotic"
                        check_n <- FALSE
                    } else if (type == "randomPerm") {
                        run_test <- function(dataTTest, HA, confInt) {
                            if(self$options$setSeed){
                                set.seed(self$options$seed)
                            }
                            R.utils::withTimeout(nparcomp::npar.t.test(dep ~ group,
                                                                       data = dataTTest,
                                                                       alternative = HA,
                                                                       conf.level = confInt,
                                                                       method = "permu",
                                                                       info = FALSE,
                                                                       nperm = self$options$n_perm
                            ), timeout = self$options$etl)
                        }
                        extract_res <- function(res) {
                            res_sel <- as.numeric(res$Analysis["id", ])
                            names(res_sel) <- colnames(res$Analysis)
                            list(
                                statistic = res_sel["Statistic"],
                                p.value = res_sel["p.value"], estimate = res_sel["Estimator"],
                                conf.int = c(res_sel["Lower"], res_sel["Upper"])
                            )
                        }
                        suffix <- "[randomPerm]"
                        full_name <- "Random Permutation"
                        check_n <- FALSE
                    } else if (type == "fullPerm") {
                        run_test <- function(dataTTest, HA, confInt) {
                            res <- brunnermunzel::brunnermunzel.permutation.test(dep ~ group,
                                                                                 data = dataTTest,
                                                                                 alternative = HA,
                                                                                 alpha = 1 - confInt, force = TRUE
                            )
                            res_tmp <- brunnermunzel::brunnermunzel.test(dep ~ group,
                                                                         data = dataTTest,
                                                                         alternative = HA,
                                                                         alpha = 1 - confInt
                            )
                            res$statistic <- res_tmp$statistic
                            res
                        }
                        suffix <- "[fullPerm]"
                        full_name <- "Full Permutation"
                        extract_res <- function(res) {
                            res
                        }
                        check_n <- TRUE
                    }

                    # perform the computations
                    is_jamovi <- !private$.dataProvided

                    ## check the data is valid
                    res <- check_data(dataTTest, check_n)
                    ## run test
                    if (!jmvcore::isError(res)) {
                        res <- try(suppressWarnings(
                            run_test(dataTTest, HA, confInt)
                        ), silent = TRUE)
                    }

                    ## extract results
                    res_names <- paste0(
                        c("stat", "df", "p", "relEff", "cil", "ciu"),
                        suffix
                    )
                    if (!jmvcore::isError(res)) {
                        res <- extract_res(res)
                        if (type == "asym") {
                            res <- check_error_res(res)
                        }
                    }
                    if (!jmvcore::isError(res)) {
                        results <- list(
                            res$statistic, res$parameter, res$p.value,
                            res$estimate, res$conf.int[1], res$conf.int[2]
                        )

                        results_list <- setNames(results, res_names)
                        ttestTable$setRow(rowKey = depName, results_list)
                    } else {
                        results <- as.list(c(NaN, "", "", "", "", ""))
                        results_list <- setNames(results, res_names)
                        ttestTable$setRow(rowKey = depName, results_list)

                        message <- jmvcore::extractErrorMessage(res)
                        if (is_jamovi) {
                            ttestTable$addFootnote(rowKey = depName, paste0("stat", suffix), message)
                        } else {
                            format_string <- paste0("For Analysis, Dependent Variable: ", depName, ", Test Type: ", full_name, ": {}")
                            warning(jmvcore::format(format_string, message))
                        }
                    }
                }
                # actual computations (calling the defined functions)
                is_jamovi <- !private$.dataProvided

                ## if used in R give warnings if invalid combinations of input
                ## parameters are used
                if (!is_jamovi) {
                    check_parameters(self$options)
                }

                ## extract data frame from inputs
                groupVarName <- self$options$group
                depVarNames <- self$options$vars
                varNames <- c(groupVarName, depVarNames)
                data <- preprocess(groupVarName, depVarNames)

                ## initialze results table
                ttestTable <- self$results$bmtest

                ## define confidence level  and alternative hypothesis
                ## from input
                confInt <- self$options$ciWidth / 100
                if (self$options$hypothesis == "oneGreater") {
                    HA <- "greater"
                } else if (self$options$hypothesis == "twoGreater") {
                    HA <- "less"
                } else {
                    HA <- "two.sided"
                }

                ## run selected tests for each dependent variable
                for (depName in depVarNames) {
                    ## create data.frame containing only seletected dependent variable
                    ## and grouping variable
                    dataTTest <- data.frame(dep = data[[depName]], group = data[[groupVarName]])

                    ## remove missing data if perAnalysis
                    ## if missing is listwise the preprocess function
                    ## takes care of the missing value, as it needs all dependent variable
                    if (self$options$miss == "perAnalysis") {
                        dataTTest <- jmvcore::naOmit(dataTTest)
                    }

                    ## run each of the test version if selected
                    if (self$options$asym) {
                        run_analysis("asym", dataTTest)
                    }

                    if (self$options$randomPerm) {
                        run_analysis("randomPerm", dataTTest)
                    }

                    if (self$options$fullPerm) {
                        run_analysis("fullPerm", dataTTest)
                    }
                }
            },
            ## in case user uses formula interface in R
            .formula = function() {
                jmvcore:::composeFormula(self$options$vars, self$options$group)
            }
        )
    )
}
