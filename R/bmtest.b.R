# This file is a generated template, your changes will not be overwritten

bmtestClass <- if (requireNamespace("jmvcore", quietly = TRUE)) {
    R6::R6Class(
        "bmtestClass",
        inherit = bmtestBase,
        private = list(
            .run = function() {
                ## private functions
                revert <- function(estimate, cil = NULL, ciu = NULL, HA) {
                    if (HA == "greater" || HA == "two.sided") {
                        estimate <- 1 - estimate
                        if (!is.null(cil)) {
                            cil_orig <- cil
                            cil <- 1 - ciu
                            ciu <- 1 - cil_orig
                        }
                    }
                    return(list(estimate = estimate, cil = cil, ciu = ciu))
                }

                check_error <- function(dataTTest, check_n = FALSE) {
                    res <- 1
                    if (is.factor(dataTTest$dep)) {
                        res <- jmvcore::createError(("Variable is not numeric"))
                    } else if (any(is.infinite(dataTTest$dep))) {
                        res <- jmvcore::createError("Variable contains infinite values")
                    } else if (check_n) {
                        ns <- table(dataTTest$group)
                        total_perm <- choose(sum(ns), ns[1])
                        if (total_perm >= 40116600L) {
                            res <- jmvcore::createError("Number of needed permutations too large to be computationally feasible. Use one of the other two options.")
                        }
                    }
                    return(res)
                }

                ## function defintion
                groupVarName <- self$options$group
                depVarNames <- self$options$vars
                varNames <- c(groupVarName, depVarNames)

                if (is.null(groupVarName) || length(depVarNames) == 0) {
                    return()
                }

                data <- jmvcore::select(self$data, varNames)

                for (name in depVarNames) {
                    data[[name]] <- jmvcore::toNumeric(data[[name]])
                }
                data[[groupVarName]] <- droplevels(as.factor(data[[groupVarName]]))

                ttestTable <- self$results$bmtest
                confInt <- self$options$ciWidth / 100

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

                ## Hypothesis options checking
                if (self$options$hypothesis == "oneGreater") {
                    HA <- "greater"
                } else if (self$options$hypothesis == "twoGreater") {
                    HA <- "less"
                } else {
                    HA <- "two.sided"
                }

                for (depName in depVarNames) {
                    dataTTest <- data.frame(dep = data[[depName]], group = data[[groupVarName]])

                    if (self$options$miss == "perAnalysis") {
                        dataTTest <- jmvcore::naOmit(dataTTest)
                    }

                    if (self$options$asym) {
                        res_asym <- check_error(dataTTest)
                        if (!jmvcore::isError(res_asym)) {
                            res_asym <- try(suppressWarnings(
                                brunnermunzel::brunnermunzel.test(dep ~ group,
                                                                  data = dataTTest,
                                                                  alternative = HA,
                                                                  alpha = 1 - confInt
                                )
                            ), silent = TRUE)
                        }

                        if (!jmvcore::isError(res_asym)) {
                            if(is.nan(res_asym$parameter)){
                                res_asym$p.value <-NaN
                            }

                            ests_asym <- revert(res_asym$estimate, res_asym$conf.int[1], res_asym$conf.int[2], HA)
                            ttestTable$setRow(rowKey = depName, list(
                                "stat[asym]" = res_asym$statistic,
                                "df[asym]" = res_asym$parameter,
                                "p[asym]" = res_asym$p.value,
                                "relEff[asym]" = ests_asym$estimate,
                                "cil[asym]" = ests_asym$cil,
                                "ciu[asym]" = ests_asym$ciu
                            ))
                        } else {
                            ttestTable$setRow(rowKey = depName, list(
                                "stat[asym]" = NaN,
                                "df[asym]" = "",
                                "p[asym]" = "",
                                "cil[asym]" = "",
                                "ciu[asym]" = ""
                            ))

                            message <- jmvcore::extractErrorMessage(res_asym)
                            if (message == "not enough observations") {
                                message <- ("One or both groups do not contain enough observations")
                            }

                            ttestTable$addFootnote(rowKey = depName, "stat[asym]", message)
                        }
                    }

                    if (self$options$randomPerm) {
                        res_random <- check_error(dataTTest)
                        if (!jmvcore::isError(res_random)) {
                            res_random <- try(suppressWarnings(
                                nparcomp::npar.t.test(dep ~ group,
                                                      data = dataTTest,
                                                      alternative = HA,
                                                      conf.level = confInt,
                                                      method = "permu",
                                                      info = FALSE, nperm = self$options$n_perm
                                )
                            ), silent = TRUE)
                        }

                        if (!jmvcore::isError(res_random)) {
                            res_sel <- as.numeric(res_random$Analysis["id", ])
                            names(res_sel) <- colnames(res_random$Analysis)
                            ests_random <- revert(res_sel["Estimator"], res_sel["Lower"], res_sel["Upper"], HA)

                            ttestTable$setRow(rowKey = depName, list(
                                "stat[randomPerm]" = res_sel["Statistic"],
                                "df[randomPerm]" = "",
                                "p[randomPerm]" = res_sel["p.value"],
                                "relEff[randomPerm]" = ests_random$estimate,
                                "cil[randomPerm]" = ests_random$cil,
                                "ciu[randomPerm]" = ests_random$ciu
                            ))
                        } else {
                            ttestTable$setRow(rowKey = depName, list(
                                "stat[randomPerm]" = NaN,
                                "df[randomPerm]" = "",
                                "p[randomPerm]" = "",
                                "cil[randomPerm]" = "",
                                "ciu[randomPerm]" = ""
                            ))

                            message <- jmvcore::extractErrorMessage(res_random)
                            if (message == "grouping factor must have exactly 2 levels") {
                                message <- ("One or both groups do not contain enough observations")
                            } else if (message == "not enough observations") {
                                message <- ("One or both groups do not contain enough observations")
                            } else if (message == "cannot compute confidence interval when all observations are tied") {
                                message <- ("All observations are tied")
                            }

                            ttestTable$addFootnote(rowKey = depName, "stat[randomPerm]", message)
                        }
                    }

                    if (self$options$fullPerm) {
                        res_full <- check_error(dataTTest, check_n = TRUE)
                        if (!jmvcore::isError(res_full)) {
                            res_full <- try(suppressWarnings(
                                brunnermunzel::brunnermunzel.permutation.test(dep ~ group,
                                                                              data = dataTTest,
                                                                              alternative = HA,
                                                                              alpha = 1 - confInt, force = TRUE
                                )
                            ), silent = TRUE)
                        }


                        if (!jmvcore::isError(res_full)) {
                            ests_full <- revert(res_full$estimate, HA = HA)
                            ttestTable$setRow(rowKey = depName, list(
                                "stat[fullPerm]" = res_full$statistic,
                                "df[fullPerm]" = res_full$parameter,
                                "p[fullPerm]" = res_full$p.value,
                                "relEff[fullPerm]" = ests_full$estimate
                            ))
                        } else {
                            ttestTable$setRow(rowKey = depName, list(
                                "stat[fullPerm]" = NaN,
                                "df[fullPerm]" = "",
                                "p[fullPerm]" = "",
                                "cil[fullPerm]" = "",
                                "ciu[fullPerm]" = ""
                            ))

                            message <- jmvcore::extractErrorMessage(res_full)
                            if (message == "grouping factor must have exactly 2 levels") {
                                message <- ("One or both groups do not contain enough observations")
                            } else if (message == "not enough observations") {
                                message <- ("One or both groups do not contain enough observations")
                            } else if (message == "cannot compute confidence interval when all observations are tied") {
                                message <- ("All observations are tied")
                            } else {
                                message <- (message)
                            }

                            ttestTable$addFootnote(rowKey = depName, "stat[fullPerm]", message)
                        }
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
            }
        )
    )
}
