testthat::test_that('All computations give correct results', {
    df <- data.frame(
        `dep 1` = rep(c(0,1), 50),
        `dep 2` = rep(c(0,1), 50) + rep(c(0,1), each = 50),
        `group 1` = rep(letters[1:2], each = 50 ),
        check.names = FALSE
    )

    res <- bmtest(
        df,
        vars = c("dep 1", "dep 2"),
        group = "group 1",
        relEff = TRUE,
        ci = TRUE,
        ciWidth = 95,
        computation = "randomPerm"
    )

    # Test main t-test table
    res_table <- res$bmtest$asDF
    testthat::expect_equal(c('dep 1', 'dep 2'), res_table$var)
    testthat::expect_equal(c(0, -14.84924), res_table$stat, tolerance = 1e-3)
    testthat::expect_equal(c(0.5, 0.875), res_table$relEff, tolerance = 1e-3)
    testthat::expect_equal(c(98, 98), res_table$df)
    testthat::expect_equal(c(1, 7.959306e-27), res_table$p, tolerance = 1e-3)
    testthat::expect_equal(c(0.3997693, 0.8248846), res_table$cil, tolerance = 1e-3)
    testthat::expect_equal(c(0.6002307, 0.9251154), res_table$ciu, tolerance = 1e-3)
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


testthat::test_that('Error is thrown if grouping var is also dependent variable', {
    df <- data.frame(
        dep = c(1, 7),
        group = c(1, 2))

    testthat::expect_error(
        bmtest(df, vars = "dep", group = "group"),
        "Grouping variable 'group' must have exactly 2 levels",
        fixed=TRUE
    )
})
