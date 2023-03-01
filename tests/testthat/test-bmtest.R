testthat::test_that('All computations give correct results', {
    df <- data.frame(
        `dep 1` = rep(c(0,1), 10),
        `dep 2` = rep(c(0,1), 10) + rep(c(0,1), each = 10),
        `group 1` = rep(letters[1:2], each = 10),
        check.names = FALSE
    )

    set.seed(123)
    res <- bmtest(
        df,
        vars = c("dep 1", "dep 2"),
        group = "group 1",
        relEff = TRUE,
        ci = TRUE,
        ciWidth = 95,
        fullPerm = TRUE,
        randomPerm = TRUE,
        asym = TRUE,
        hypothesis = "different",

    )

    # Test main t-test table
    res_table <- res$bmtest$asDF
    expected_table <- data.frame(
        var = c("dep 1", "dep 2"),
        `test[asym]` = c("t-Approximation", "t-Approximation"),
        `stat[asym]` = c(0.000000, 6.363961),
        `df[asym]` = c(18, 18),
        `p[asym]` = c(1.000000e+00, 5.394655e-06),
        `relEff[asym]` = c(0.500, 0.125),
        `cil[asym]` = c(0.252403963, 0.001201982),
        `ciu[asym]` = c(0.747596, 0.248798),
        `test[randomPerm]` = c("Random Permutations", "Random Permutations"),
        `stat[randomPerm]` = c(0.000, 6.364),
        `df[randomPerm]` = c(NA, NA),
        `p[randomPerm]` = c(0.654, 0.002),
        `relEff[randomPerm]` = c(0.500, 0.125),
        `cil[randomPerm]` = c(0.282, 0.000),
        `ciu[randomPerm]` = c(0.718, 0.250),
        `test[fullPerm]` = c("All Permutations", "All Permutations"),
        `stat[fullPerm]` = c(NA, NA),
        `df[fullPerm]` = c(NA, NA),
        `p[fullPerm]` = c(1.000000000, 0.002727922),
        `relEff[fullPerm]` = c(0.500, 0.125),
        `cil[fullPerm]` = c(NA, NA),
        `ciu[fullPerm]` = c(NA, NA),
        check.names = FALSE
    )
    expected_table <- expected_table[,names(res_table)]
    rownames(expected_table) <- c("\"dep 1\"", "\"dep 2\"")
    testthat::expect_equal(res_table, expected_table, tolerance = 0.0001, ignore_attr = TRUE)
})

testthat::test_that('Error is thrown if grouping var has more than 2 levels', {
    df <- data.frame(
        dep = c(1, 7, 4),
        group = c("a", "b", "c"),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        bmtest(df, vars = "dep", group = "group"),
        "Grouping variable 'group' must have exactly 2 levels",
        fixed=TRUE
    )
})

testthat::test_that('Error is thrown if dependent variable is not numeric', {
    df <- data.frame(
        group = c(1, 7, 4),
        dep = c("a", "b", "c"),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        bmtest(df, vars = "dep", group = "group"),
        "Argument 'vars' requires a numeric variable ('dep' is not valid)",
        fixed=TRUE
    )
})


testthat::test_that('Error is thrown if dependent variable is not numeric', {
    df <- data.frame(
        group = c(1, 7, 4),
        dep =  c(1, 7, 4),
        stringsAsFactors = TRUE
    )

    testthat::expect_error(
        bmtest(df, vars = "dep", group = "group"),
        "Argument 'vars' requires a numeric variable ('dep' is not valid)",
        fixed=TRUE
    )
})

testthat::test_that('Error is thrown if numer full permutation too high', {
    df <- data.frame(
        dep = rnorm(100),
        group = rep(letters[1:2], each = 50),
        check.names = FALSE
    )

    testthat::expect_error(
        bmtest(df, vars = "dep", group = "group", fullPerm = TRUE),
        "Argument 'vars' requires a numeric variable ('dep' is not valid)",
        fixed=TRUE
    )

    res <- jmv::ttestIS(df, vars = "dep", group = "group")
})


