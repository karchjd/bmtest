testthat::test_that('All computations give correct results', {
    df <- data.frame(
        y1 = rep(c(0,1), 10),
        y2 = rep(c(0,1), 10) + rep(c(0,1), each = 10),
        g = rep(letters[1:2], each = 10),
        check.names = FALSE
    )

    set.seed(123)

    testthat::expect_warning(res <- bmtest(
        df,
        vars = c("y1", "y2"),
        group = "g",
        relEff = TRUE,
        ci = TRUE,
        ciWidth = 95,
        fullPerm = TRUE,
        randomPerm = TRUE,
        asym = TRUE,
        hypothesis = "different",

    ), "Confidence intervals not supported for full permutation approach.")

    # Test main t-test table
    res_table <- res$bmtest$asDF
    expected_table <- data.frame(
        var = c("y1", "y2"),
        `test[asym]` = c("Asymptotic", "Asymptotic"),
        `stat[asym]` = c(0, 6.363961),
        `df[asym]` = c(18, 18),
        `p[asym]` = c(1, 5.394655e-06),
        `relEff[asym]` = c(0.500, 0.875),
        `cil[asym]` = c(0.252403963, 0.751202),
        `ciu[asym]` = c(0.747596, 0.998798),
        `test[randomPerm]` = c("Random Permutation", "Random Permutation"),
        `stat[randomPerm]` = c(0, 6.364),
        `df[randomPerm]` = c(NA, NA),
        `p[randomPerm]` = c(0.654, 0.002),
        `relEff[randomPerm]` = c(0.5, 0.875),
        `cil[randomPerm]` = c(0.282, .75),
        `ciu[randomPerm]` = c(0.718, 1),
        `test[fullPerm]` = c("Full Permutation", "Full Permutation"),
        `stat[fullPerm]` = c(0, 6.363961),
        `df[fullPerm]` = c(NA, NA),
        `p[fullPerm]` = c(1, 0.002727922),
        `relEff[fullPerm]` = c(0.500, 0.875),
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


testthat::test_that('Warning is thrown if number full permutation too high', {
    df <- data.frame(
        dep = rnorm(100),
        group = rep(letters[1:2], each = 50),
        check.names = FALSE
    )

    testthat::expect_warning(
        bmtest(df, vars = "dep", group = "group", fullPerm = TRUE), "For Analysis, Dependent Variable: dep, Test Type: Full Permutation: Number of needed permutations too large to be computationally feasible. Use one of the other two options.")

    res <- jmv::ttestIS(df, vars = "dep", group = "group")
})

testthat::test_that('Warning is thrown if ci true but not relEff', {
    df <- data.frame(
        y1 = rep(c(0,1), 10),
        g = rep(letters[1:2], each = 10),
        check.names = FALSE
    )
    testthat::expect_warning(
        bmtest(df, vars = "y1", group = "g", ci = TRUE), "relEff parameter must also be TRUE to get confidence intervals")
})

testthat::test_that('Warning is thrown if ci true but only fullPerm', {
    df <- data.frame(
        y1 = rep(c(0,1), 10),
        g = rep(letters[1:2], each = 10),
        check.names = FALSE
    )
    testthat::expect_warning(
        bmtest(df, vars = "y1", group = "g", fullPerm = TRUE, asym = FALSE, ci = TRUE, relEff = TRUE), "Confidence intervals not supported for full permutation approach.")
})


testthat::test_that('Warning is thrown if relative effect 1 or 0', {
    df <- data.frame(
        y1 = rep(c(0,1), each= 10) + rnorm(20) * 0.001,
        g = rep(letters[1:2], each = 10),
        check.names = FALSE
    )
    testthat::expect_warning(
        bmtest(df, vars = "y1", group = "g", ci = TRUE, relEff = TRUE), "For Analysis, Dependent Variable: y1, Test Type: Asymptotic: Results cannot be trusted. Likely caused by an estimated relative effect of very close to 1 or 0.*")
})

testthat::test_that('Formula Interface', {
    df <- data.frame(
        y1 = rep(c(0,1), 10),
        g = rep(letters[1:2], each = 10)
    )

    set.seed(123)
    testthat::expect_warning(res <- bmtest(data = df, formula = y1 ~g, randomPerm = TRUE, fullPerm = TRUE, relEff = TRUE, ci = TRUE))
    res_table <- res$bmtest$asDF
    expected_table <- data.frame(
        var = "y1",
        `test[asym]` = "Asymptotic",
        `stat[asym]` = 0,
        `df[asym]` = 18,
        `p[asym]` = 1,
        `relEff[asym]` = 0.5,
        `cil[asym]` = 0.252403963,
        `ciu[asym]` = 0.747596,
        `test[randomPerm]` = "Random Permutation",
        `stat[randomPerm]` = 0,
        `p[randomPerm]` = 0.654,
        `relEff[randomPerm]` = 0.5,
        `cil[randomPerm]` = 0.282,
        `ciu[randomPerm]` = 0.718,
        `test[fullPerm]` = "Full Permutation",
        `stat[fullPerm]` = 0,
        `p[fullPerm]` = 1,
        `relEff[fullPerm]` = 0.5,
        check.names = FALSE
    )
    testthat::expect_equal(res_table, expected_table, tolerance = 0.0001, ignore_attr = TRUE)
})

