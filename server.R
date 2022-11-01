library(shiny)
library(shinythemes)
library(ggplot2)
library(ggnewscale)
library(shinyFeedback)
options(scipen = 999)

server <- function(input, output) {
  plot_colors <- c("orange", "black", "red")
  max_obs <- 100000
  
  # Initializing some reactive vectors.
  betas <- reactiveValues(vals = NULL)
  data_generated <- reactiveVal(FALSE)
  coefs <- reactiveValues(vals = NULL,
                          lower = NULL,
                          upper = NULL,
                          se = NULL)
  error_coefs <- reactiveValues(vals = NULL,
                                lower = NULL,
                                upper = NULL,
                                se = NULL)

  # Creating a reactive that indicates whether inputs are valid.
  valid <- reactive({
    input$n >= 2 & (input$n %% 1) == 0 & input$n <= max_obs & input$sd_res >= 0
  })

  # Throw warnings if inputs aren't valid.
  observe({
    feedbackWarning("sd_res", input$sd_res < 0, "Negative sd not allowed.")
    feedbackWarning("error_sd_x", input$error_sd_x < 0, "Negative sd not allowed.")
    feedbackWarning("error_sd_y", input$error_sd_y < 0, "Negative sd not allowed.")
    
    # For the number of observations there are two cases that we wish to
    # treat separately. This is done with the following trick.
    hideFeedback("n")
    case1 <- input$n > max_obs
    case2 <- (input$n < 2) | ((input$n %% 1) != 0)
    feedback_text <- ""
    if (case1) {
      feedback_text <- paste0("Too many observations (maximum allowed is ", max_obs, ").")
    }
    else if (case2) {
      feedback_text <- paste0("Number of observations needs to be an integer >= 2.")
    }
    feedbackWarning("n", case1 | case2, feedback_text)
  })

  # Simulate data upon pressing the generate button if inputs are valid.
  dat <- eventReactive(input$generate, {
    if (valid()) {
      x <- runif(input$n, 0, 10)
      y <- input$beta0 + input$beta1 * x + rnorm(input$n, 0, input$sd_res)
      data.frame(x, y)
    }
    else {
      NULL
    }
  })

  # If data has succesfully been generated, then we store 
  # the true parameters and fit a linear regression model.
  observeEvent(input$generate, {
    betas$vals <- c(input$beta0, input$beta1)
    if (!is.null(dat())) {
      model1 <- lm(y ~ x, dat())
      coefs$vals <- model1$coef
      coefs$lower <- confint(model1)[, 1]
      coefs$upper <- confint(model1)[, 2]
      coefs$se <- summary(model1)$coef[, 2]
    }
    else {
      coefs$vals <- NULL
      coefs$lower <- NULL
      coefs$upper <- NULL
      coefs$se <- NULL
    }
  })

  # If data has succesfully been generated and measurement error inputs
  # are valid, then a data set with measurement error is created.
  error_dat <- reactive({
    if (input$error_sd_x >= 0 & input$error_sd_y >= 0 & !is.null(dat())) {
      new_dat <- dat()
      which_x <- input$xrange[1] <= new_dat$x & new_dat$x <= input$xrange[2]
      which_y <- input$yrange[1] <= new_dat$y & new_dat$y <= input$yrange[2]
      new_dat$x[which_y] <- new_dat$x[which_y] + rnorm(sum(which_y), input$error_mean_x, max(input$error_sd_x, 0))
      new_dat$y[which_x] <- new_dat$y[which_x] + rnorm(sum(which_x), input$error_mean_y, max(input$error_sd_y, 0))
      new_dat
    }
    else {
      NULL
    }
  })

  # If measurement error data is succesfully created, we
  # fit a linear regression model to those.
  observe({
    if (!is.null(error_dat())) {
      model2 <- lm(y ~ x, error_dat())
      error_coefs$vals <- model2$coef
      error_coefs$lower <- confint(model2)[, 1]
      error_coefs$upper <- confint(model2)[, 2]
      error_coefs$se <- summary(model2)$coef[, 2]
    }
    else {
      error_coefs$vals <- NULL
      error_coefs$lower <- NULL
      error_coefs$upper <- NULL
      error_coefs$se <- NULL
    }
  })

  # Make various plots depending on the user-specified preferences.
  make_plot <- function(dat, error_dat, betas, coefs, error_coefs, points, merror, connect, ptrans, ltrans) {
    
    # Return NULL if no valid data is available.
    if (is.null(dat)) {
      return(NULL)
    }
    
    # If user has specified that measurement error should be added but no valid
    # measurement error data is available (i.e. invalid inputs), then we simply 
    # return the plot corresponding to setting measurement error to "off".
    if (merror & is.null(error_dat)) {
      return(make_plot(dat = dat,
                       error_dat = NULL,
                       betas = betas,
                       coefs = coefs,
                       error_coefs = NULL,
                       points = points,
                       merror = FALSE,
                       connect = FALSE,
                       ptrans = ptrans,
                       ltrans = ltrans))
    }
    
    # This is the base plot.
    base_plot <- ggplot() +
      theme_minimal() +
      labs(x = "", y = "") +
      geom_abline(aes(color = "True line",
                      linetype = "True line",
                      slope = betas$vals[2],
                      intercept = betas$vals[1]),
                  size = 1) +
      geom_abline(aes(color = "Linear fit (without error)",
                      linetype = "Linear fit (without error)",
                      slope = coefs$vals[2],
                      intercept = coefs$vals[1]),
                  size = 1) +
      scale_color_manual(name = "",
                         values = c("True line" = "orange",
                                    "Linear fit (without error)" = "black")) +
      scale_linetype_manual(name = "",
                            values = c("True line" = 1,
                                       "Linear fit (without error)" = 2)) +
      geom_point(data = dat,
                 aes(x = x, y = y),
                 alpha = 0) +
      theme(legend.position = "bottom",
            text = element_text(size = 20))
    
    # If points and measurement error is off, return base plot.
    if (!points & !merror) {
      res_plot <- base_plot
    }
    
    # If measurement error is on, then add a line based on this data.
    # We add the points to graph but make them invisible in order for
    # the axes to be identical when toggling points on and off.
    else if (!points & merror) {
      res_plot <- base_plot +
        geom_abline(aes(color = "Linear fit (with error)",
                        linetype = "Linear fit (with error)",
                        slope = error_coefs$vals[2],
                        intercept = error_coefs$vals[1]),
                    size = 1) +
        scale_color_manual(name = "",
                           values = c("True line" = "orange",
                                      "Linear fit (without error)" = "black",
                                      "Linear fit (with error)" = "red")) +
        scale_linetype_manual(name = "",
                              values = c("True line" = 1,
                                         "Linear fit (without error)" = 2,
                                         "Linear fit (with error)" = 2)) +
        geom_point(data = dat,
                   aes(x = x, y = y),
                   alpha = 0) +
        geom_point(data = error_dat,
                   aes(x = x, y = y),
                   alpha = 0)
    }
    
    # If points are on but measurement error is off, return base plot
    # with scatter plot of original data added to it.
    else if (points & !merror) {
      res_plot <- base_plot +
        new_scale_color() +
        geom_point(data = dat,
                   aes(x = x,
                       y = y,
                       shape = "Observations (without error)",
                       size = "Observations (without error)",
                       color = "Observations (without error)"),
                   alpha = ptrans) +
        scale_size_manual(name = "",
                          values = c("Observations (without error)" = 1.5)) +
        scale_shape_manual(name = "",
                           values = c("Observations (without error)" = 16)) +
        scale_color_manual(name = "",
                           values = c("Observations (without error)" = "black")) +
        theme(legend.position = "bottom",
              text = element_text(size = 20)) +
        guides(size = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))
    }
    
    # If both points and measurement error are on, return a plot with
    # scatter plots and lines based on both data sets added to it.
    else if (points & merror) {
      res_plot <- base_plot +
        geom_abline(aes(color = "Linear fit (with error)",
                        linetype = "Linear fit (with error)",
                        slope = error_coefs$vals[2],
                        intercept = error_coefs$vals[1]),
                    size = 1) +
        scale_color_manual(name = "",
                           values = c("True line" = "orange",
                                      "Linear fit (without error)" = "black",
                                      "Linear fit (with error)" = "red")) +
        scale_linetype_manual(name = "",
                              values = c("True line" = 1,
                                         "Linear fit (without error)" = 2,
                                         "Linear fit (with error)" = 2)) +
        new_scale_color() +
        geom_point(data = dat,
                   aes(x = x,
                       y = y,
                       shape = "Observations (without error)",
                       size = "Observations (without error)",
                       color = "Observations (without error)"),
                   alpha = ptrans) +
        geom_point(data = error_dat,
                   aes(x = x,
                       y = y,
                       shape = "Observations (with error)",
                       size = "Observations (with error)",
                       color = "Observations (with error)"),
                   alpha = ptrans) +
        scale_size_manual(name = "",
                          values = c("Observations (without error)" = 1.5,
                                     "Observations (with error)" = 1.5)) +
        scale_shape_manual(name = "",
                           values = c("Observations (without error)" = 16,
                                      "Observations (with error)" = 4)) +
        scale_color_manual(name = "",
                           values = c("Observations (without error)" = "black",
                                      "Observations (with error)" = "red")) +
        theme(legend.position = "bottom",
              text = element_text(size = 20)) +
        guides(size = guide_legend(order = 1),
               color = guide_legend(order = 1),
               shape = guide_legend(order = 1))
      
      # If connect is on, then we add a line segment between the original
      # points and the points with measurement error.
      if (connect) {
        comb_dat <- cbind(dat, error_dat)
        colnames(comb_dat) <- c("x", "y", "x_err", "y_err")
        res_plot <- res_plot +
          geom_segment(dat = comb_dat,
                       aes(x = x,
                           y = y,
                           xend = x_err,
                           yend = y_err),
                       alpha = ltrans)
      }
    }
    return(res_plot)
  }

  # A reactive plot is made.
  plot <- reactive({
    make_plot(dat = dat(),
              error_dat = error_dat(),
              betas = betas,
              coefs = coefs,
              error_coefs = error_coefs,
              points = (input$toggle_points == "Show"),
              merror = (input$toggle_error == "On"),
              connect = (input$toggle_connect == "On"),
              ptrans = input$transparency,
              ltrans = input$transparency_seg)
  })

  # We update the slider range as this range depends on the variation 
  # in the simulated data.
  observe({
    if (!is.null(dat())) {
      y_upper <- ceiling(max(dat()$y))
      y_lower <- floor(min(dat()$y))
      x_upper <- ceiling(max(dat()$x))
      x_lower <- floor(min(dat()$x))
    }
    else {
      y_upper <- 10
      y_lower <- -10
      x_upper <- 10
      x_lower <- -10
    }
    updateSliderInput(
      inputId = "yrange",
      min = y_lower,
      max = y_upper,
      value = c(y_lower, y_upper)
    )
    updateSliderInput(
      inputId = "xrange",
      min = x_lower,
      max = x_upper,
      value = c(x_lower, x_upper)
    )
  })

  # Allow the user to reset measurement error.
  observeEvent(input$reset_error_x, {
    updateNumericInput(inputId = "error_mean_x",
                       value = 0)
    updateNumericInput(inputId = "error_sd_x",
                       value = 0)
  })

  observeEvent(input$reset_error_y, {
    updateNumericInput(inputId = "error_mean_y",
                       value = 0)
    updateNumericInput(inputId = "error_sd_y",
                       value = 0)
  })

  # Output the plot.
  output$main_plot <- renderPlot({
    plot()
  })
  
  # Output estimates based on the original data.
  output$estimates_all <- renderTable({
    if (!is.null(dat())) {
      res <- cbind(round(coefs$vals, 2), round(coefs$se, 2), round(coefs$lower, 2), round(coefs$upper, 2))
      res <- cbind(c("Intercept", "Slope"), res)
      colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
      res
    }
    else {
      NULL
    }
  })
  
  # Output estimates based on the measurement error data.
  output$estimates_error <- renderTable({
    if (!is.null(error_dat()) & (input$toggle_error == "On")) {
      res <- cbind(round(error_coefs$vals, 2), round(error_coefs$se, 2), round(error_coefs$lower, 2), round(error_coefs$upper, 2))
      res <- cbind(c("Intercept", "Slope"), res)
      colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
      res
    } 
    else {
      NULL
    }
  })
}