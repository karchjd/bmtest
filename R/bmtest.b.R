# This file is a generated template, your changes will not be overwritten

bmtestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "bmtestClass",
        inherit = bmtestBase,
        private = list(
            .run = function() {
                ## private functions and constants
                check_parameters <- function(parameters){
                    if(parameters$ci && !parameters$relEff){
                        warning("relEff parameter must also be TRUE to get confidence intervals")
                    }

                    if(parameters$ci && parameters$fullPerm){
                        warning("Confidence intervals not supported for full permutation approach.")
                    }
                }

                revert <- function(estimate, cil = NULL, ciu = NULL, HA) {
                    if (HA == "greater" || HA == "two.sided") {
                        estimate <- 1 - estimate
                        if (!is.null(cil)) {
                            cil_orig <- cil
                            cil <- 1 - ciu
                            ciu <- 1 - cil_orig
                        }else{
                            cil <- ciu <- ""
                        }
                    }
                    return(list(estimate = estimate, cil = cil, ciu = ciu))
                }

                check_error <- function(dataTTest, check_n = FALSE) {
                    if (is.factor(dataTTest$dep)) {
                        return(jmvcore::createError(("Variable is not numeric")))
                    }
                    if (any(is.infinite(dataTTest$dep))) {
                        return(jmvcore::createError("Variable contains infinite values"))
                    }
                    ns <- table(dataTTest$group)
                    if(any(ns == 1)){
                        return(jmvcore::createError("Not enough observations. Each group must at least contain 2 observations"))
                    }

                    levels_groups <- levels(dataTTest$group)
                    g1 <- dataTTest$dep[dataTTest$group == levels_groups[1]]
                    g2 <- dataTTest$dep[dataTTest$group == levels_groups[2]]
                    if (length(unique(g1)) == 1 || length(unique(g2)) == 1){
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

                preprocess <- function(groupVarName, depVarnames){
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

                check_error_res <- function(res){
                    if(is.nan(res$parameter)){
                        return(jmvcore::createError("Results cannot be trusted. Likely caused by an estimated relative effect of very close to 1 or 0.
                                              Try one of the other test types."))
                    }else{
                        return(res)
                    }
                }

                run_analysis <- function(type , dataTTest){
                    if(type == "asym"){
                        run_test <- function(dataTTest, HA, confInt){
                            brunnermunzel::brunnermunzel.test(dep ~ group,
                                                              data = dataTTest,
                                                              alternative = HA,
                                                              alpha = 1 - confInt
                            )
                        }
                        extract_res <- function(res){
                            if(is.nan(res$parameter)){
                                res$p.value <-NaN
                            }
                            res
                        }
                        suffix <- "[asym]"
                        full_name <- "Asymptotic"
                        check_n <- FALSE
                    }else if(type == "randomPerm"){
                        run_test <- function(dataTTest, HA, confInt){
                            nparcomp::npar.t.test(dep ~ group,
                                                  data = dataTTest,
                                                  alternative = HA,
                                                  conf.level = confInt,
                                                  method = "permu",
                                                  info = FALSE, nperm = self$options$n_perm)
                        }
                        extract_res <- function(res){
                            res_sel <- as.numeric(res$Analysis["id", ])
                            names(res_sel) <- colnames(res$Analysis)
                            list(statistic = res_sel["Statistic"], parameter = "",
                                 p.value = res_sel["p.value"], estimate = res_sel["Estimator"],
                                 conf.int = c(res_sel["Lower"], res_sel["Upper"]))
                        }
                        suffix <- "[randomPerm]"
                        full_name <- "Random Permutations"
                        check_n <- FALSE

                    } else if (type == "fullPerm"){
                        run_test <- function(dataTTest, HA, confInt){
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
                        full_name <- "All Permutations"
                        extract_res <- function(res){
                            res$parameter = ""
                            res
                        }
                        check_n <- TRUE
                    }
                    is_jamovi <- !private$.dataProvided

                    res <- check_error(dataTTest, check_n)
                    if (!jmvcore::isError(res)) {
                        res <- try(suppressWarnings(
                            run_test(dataTTest, HA, confInt)
                        ), silent = TRUE)
                    }

                    res_names <- paste0(c("stat", "df", "p", "relEff", "cil", "ciu"),
                                        suffix)
                    if (!jmvcore::isError(res)) {
                        res <- extract_res(res)
                        res  <- check_error_res(res)
                    }
                    if (!jmvcore::isError(res)) {
                        ests <- revert(res$estimate, res$conf.int[1], res$conf.int[2], HA)
                        results <- list(res$statistic, res$parameter, res$p.value,
                                             ests$estimate, ests$cil, ests$ciu)

                        results_list <- setNames(results, res_names)
                        ttestTable$setRow(rowKey = depName, results_list)
                    } else {
                        results <- as.list(c(NaN, "", "", "", "", ""))
                        results_list <- setNames(results, res_names)
                        ttestTable$setRow(rowKey = depName, results_list)

                        message <- jmvcore::extractErrorMessage(res)
                        if(is_jamovi){
                            ttestTable$addFootnote(rowKey = depName, paste0("stat",suffix), message)
                        }else{
                            format_string <- paste0("For Analysis, Dependent Variable: ", depName, ", Test Type: ", full_name, ": {}")
                            warning(jmvcore::format(format_string, message))
                        }
                    }
                }
                is_jamovi <- !private$.dataProvided

                ## preprocessing and initializing
                if(!is_jamovi){
                    check_parameters(self$options)
                }
                groupVarName <- self$options$group
                depVarNames <- self$options$vars
                varNames <- c(groupVarName, depVarNames)



                data <- preprocess(groupVarName, depVarNames)


                ttestTable <- self$results$bmtest
                confInt <- self$options$ciWidth / 100
                if (self$options$hypothesis == "oneGreater") {
                    HA <- "greater"
                } else if (self$options$hypothesis == "twoGreater") {
                    HA <- "less"
                } else {
                    HA <- "two.sided"
                }

                # actual analysis
                for (depName in depVarNames) {
                    dataTTest <- data.frame(dep = data[[depName]], group = data[[groupVarName]])

                    if (self$options$miss == "perAnalysis") {
                        dataTTest <- jmvcore::naOmit(dataTTest)
                    }

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
            .init = function() {
                hypothesis <- self$options$hypothesis
                groupName <- self$options$group

                groups <- NULL
                if (!is.null(groupName)) {
                    groups <- base::levels(self$data[[groupName]])
                }
                if (length(groups) != 2) {
                    groups <- c("Group 1", "Group 2")
                }

                table <- self$results$bmtest

                ciTitleString <- "{ciWidth}% Confidence Interval"
                ciTitle <- jmvcore::format(ciTitleString, ciWidth = self$options$ciWidth)
                table$getColumn("ciu[asym]")$setSuperTitle(ciTitle)
                table$getColumn("cil[asym]")$setSuperTitle(ciTitle)
                table$getColumn("ciu[randomPerm]")$setSuperTitle(ciTitle)
                table$getColumn("cil[randomPerm]")$setSuperTitle(ciTitle)
                table$getColumn("relEff[asym]")$setTitle(jmvcore::format("P({} > {}) + \u00BDP({} = {})", groups[1], groups[2], groups[1], groups[2]))
                table$getColumn("relEff[randomPerm]")$setTitle(jmvcore::format("P({} > {}) + \u00BDP({} = {})", groups[1], groups[2], groups[1], groups[2]))
                table$getColumn("relEff[fullPerm]")$setTitle(jmvcore::format("P({} > {}) + \u00BDP({} = {})", groups[1], groups[2], groups[1], groups[2]))

                if (hypothesis == "oneGreater") {
                    table$setNote("hyp", jmvcore::format("H\u2090 P({} > {}) + \u00BDP({} = {}) > \u00BD", groups[1], groups[2], groups[1], groups[2]))
                } else if (hypothesis == "twoGreater") {
                    table$setNote("hyp", jmvcore::format("H\u2090 P({} < {}) + \u00BDP({} = {}) > \u00BD", groups[1], groups[2], groups[1], groups[2]))
                } else {
                    table$setNote("hyp", jmvcore::format("H\u2090 P({} > {}) + \u00BDP({} = {}) \u2260 \u00BD", groups[1], groups[2], groups[1], groups[2]))
                }
            },

            .formula=function() {
                jmvcore:::composeFormula(self$options$vars, self$options$group)
            }
        )
    )
}
