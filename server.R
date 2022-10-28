library(shiny)
library(shinythemes)
library(ggplot2)
library(ggnewscale)
library(shinyFeedback)
options(scipen=999)

server <- function(input, output) {
  plot_colors <<- c("orange", "black", "red")
  max_obs <<- 100000
  betas <- reactiveValues(vals=NULL)
  data_generated <- reactiveVal(FALSE)
  coefs <- reactiveValues(vals=NULL, lower=NULL, upper=NULL, se=NULL)
  error_coefs <- reactiveValues(vals=NULL, lower=NULL, upper=NULL, se=NULL)
  
  valid_obs <- reactive({
    input$n <= max_obs
  })
  
  valid_sdres <- reactive({
    input$sd_res >= 0
  })
  
  valid_error_sd_x <- reactive({
    input$error_sd_x>=0
  })
  
  valid_error_sd_y <- reactive({
    input$error_sd_y>=0
  })
  
  observe({
    shinyFeedback::feedbackWarning("n", !valid_obs(), paste0("Number of observations set to ", max_obs))
    shinyFeedback::feedbackWarning("sd_res", !valid_sdres(), "Negative sd not allowed (set to 0).")
    shinyFeedback::feedbackWarning("error_sd_x", !valid_error_sd_x(), "Negative sd not allowed (set to 0).")
    shinyFeedback::feedbackWarning("error_sd_y", !valid_error_sd_y(), "Negative sd not allowed (set to 0).")
  })
  
  dat <- eventReactive(input$generate, {
    n <- min(round(input$n), max_obs)
    x <- runif(n, 0, 10)
    y <- input$beta0 + input$beta1*x + rnorm(n, 0, max(input$sd_res,0))
    data.frame(x,y)
  })
  
  observeEvent(input$generate, {
    data_generated(TRUE)
    model1 <- lm(y~x, dat())
    coefs$vals <- model1$coef
    coefs$lower <- confint(model1)[,1]
    coefs$upper <- confint(model1)[,2]
    coefs$se <- summary(model1)$coef[,2]
    betas$vals <- c(input$beta0, input$beta1)
  })
  
  error_dat <- reactive({
    if (data_generated()) {
      req(input$error_mean_x, input$error_mean_y, input$error_sd_x, input$error_sd_y)
      new_dat <- dat()
      which_x <- input$xrange[1] <= new_dat$x & new_dat$x <= input$xrange[2]
      which_y <- input$yrange[1] <= new_dat$y & new_dat$y <= input$yrange[2]
      new_dat$x[which_y] <- new_dat$x[which_y] + rnorm(sum(which_y), input$error_mean_x, max(input$error_sd_x, 0))
      new_dat$y[which_x] <- new_dat$y[which_x] + rnorm(sum(which_x), input$error_mean_y, max(input$error_sd_y, 0))
      new_dat
    }
    else {
      data.frame()
    }
  })
  
  comb_dat <- reactive({
    if (data_generated()) {
      res <- cbind(dat(), error_dat())
      colnames(res) <- c("x", "y", "x_err", "y_err")
      res
    }
    else {
      data.frame()
    }
  })
  
  observe({
    if (data_generated()) {
      model2 <- lm(y~x, error_dat())
      error_coefs$vals <- model2$coef
      error_coefs$lower <- confint(model2)[,1]
      error_coefs$upper <- confint(model2)[,2]
      error_coefs$se <- summary(model2)$coef[,2]
      
    }
  })
  
  plot <- reactive({
    if (data_generated()) {
      if (input$toggle_points) {
        if (input$toggle_error) {
          res <- ggplot() + theme_minimal() + labs(x="", y="") + 
            geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
            geom_abline(aes(color="Linear fit (without error)", linetype="Linear fit (without error)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
            geom_abline(aes(color="Linear fit (with error)", linetype="Linear fit (with error)", slope=error_coefs$vals[2], intercept=error_coefs$vals[1]), size=1) +
            scale_color_manual(name="", values=c("True line"="orange", "Linear fit (without error)"="black", "Linear fit (with error)"="red")) + 
            scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (without error)"=2, "Linear fit (with error)"=2)) +
            new_scale_color() +
            geom_point(data=dat(), aes(x=x, y=y, shape="Observations (without error)", size="Observations (without error)", color="Observations (without error)"), alpha=input$transparency) + 
            geom_point(data=error_dat(), aes(x=x, y=y, shape="Observations (with error)", size="Observations (with error)", color="Observations (with error)"), alpha=input$transparency) +
            scale_size_manual(name="", values=c("Observations (without error)"=1.5, "Observations (with error)"=1.5)) +
            scale_shape_manual(name="", values=c("Observations (without error)"=16, "Observations (with error)"=4)) +
            scale_color_manual(name="", values=c("Observations (without error)"="black", "Observations (with error)"="red")) +
            theme(legend.position="bottom", text=element_text(size=20)) + 
            guides(size=guide_legend(order=1), color=guide_legend(order=1), shape=guide_legend(order=1))
          if (input$toggle_connect) {
            res + geom_segment(dat=comb_dat(), aes(x=x, y=y, xend=x_err, yend=y_err), alpha=input$transparency_seg)
          }
          else {
            res
          }
          
        }
        else { 
          ggplot() + theme_minimal() + labs(x="", y="") + 
            geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
            geom_abline(aes(color="Linear fit (without error)", linetype="Linear fit (without error)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
            scale_color_manual(name="", values=c("True line"="orange", "Linear fit (without error)"="black")) + 
            scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (without error)"=2)) + 
            new_scale_color() + 
            geom_point(data=dat(), aes(x=x, y=y, shape="Observations (without error)", size="Observations (without error)", color="Observations (without error)"), alpha=input$transparency) + 
            scale_size_manual(name="", values=c("Observations (without error)"=1.5)) +
            scale_shape_manual(name="", values=c("Observations (without error)"=16)) +
            scale_color_manual(name="", values=c("Observations (without error)"="black")) +
            theme(legend.position="bottom", text=element_text(size=20)) + 
            guides(size=guide_legend(order=1), color=guide_legend(order=1), shape=guide_legend(order=1))
        }
      }
      
      else {
        if (input$toggle_error) {
          ggplot() + theme_minimal() + labs(x="", y="") + 
            geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
            geom_abline(aes(color="Linear fit (without error)", linetype="Linear fit (without error)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
            geom_abline(aes(color="Linear fit (with error)", linetype="Linear fit (with error)", slope=error_coefs$vals[2], intercept=error_coefs$vals[1]), size=1) +
            scale_color_manual(name="", values=c("True line"="orange", "Linear fit (without error)"="black", "Linear fit (with error)"="red")) + 
            scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (without error)"=2, "Linear fit (with error)"=2)) +
            geom_point(data=dat(), aes(x=x, y=y), alpha=0) +
            geom_point(data=error_dat(), aes(x=x, y=y), alpha=0) + 
            theme(legend.position="bottom", text=element_text(size=20))
        }
        else {
          ggplot() + theme_minimal() + labs(x="", y="") + 
            geom_abline(aes(color="True line", linetype="True line", slope=betas$vals[2], intercept=betas$vals[1]), size=1) + 
            geom_abline(aes(color="Linear fit (without error)", linetype="Linear fit (without error)", slope=coefs$vals[2], intercept=coefs$vals[1]), size=1) +
            scale_color_manual(name="", values=c("True line"="orange", "Linear fit (without error)"="black")) + 
            scale_linetype_manual(name="", values=c("True line"=1, "Linear fit (without error)"=2)) + 
            geom_point(data=dat(), aes(x=x, y=y), alpha=0) +
            theme(legend.position="bottom", text=element_text(size=20))
        }
      }
    } else {
      ggplot()
    }
  })
  
  observe({
    round.choose <- function(x, roundTo, dir=1) {
      if(dir == 1) {  ##ROUND UP
        x + (roundTo - x %% roundTo)
      } else {
        if(dir == 0) {  ##ROUND DOWN
          x - (x %% roundTo)
        }
      }
    }
    y_upper <- round.choose(max(dat()$y), 1, 1)
    y_lower <- round.choose(min(dat()$y), 1, 0)
    x_upper <- round.choose(max(dat()$x), 1, 1)
    x_lower <- round.choose(min(dat()$x), 1, 0)
    updateSliderInput(inputId="yrange", min=y_lower, max=y_upper, value=c(y_lower, y_upper))
    updateSliderInput(inputId="xrange", min=x_lower, max=x_upper, value=c(x_lower, x_upper))
  })
  
  observeEvent(input$reset_error_x, {
    updateNumericInput(inputId="error_mean_x", value=0)
    updateNumericInput(inputId="error_sd_x", value=0)
  })
  
  observeEvent(input$reset_error_y, {
    updateNumericInput(inputId="error_mean_y", value=0)
    updateNumericInput(inputId="error_sd_y", value=0)
  })
  
  output$scatterPlot <- renderPlot({plot()})
  
  output$estimates_all <- renderTable({
    if (data_generated()) {
      res <- cbind(round(coefs$vals,2), round(coefs$se,2), round(coefs$lower,2), round(coefs$upper,2))
      res <- cbind(c("Intercept", "Slope"), res)
      colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
      res
    }
    else {
      NULL
    }
  })
  
  output$estimates_sel <- renderTable({
    if (data_generated()) {
      if (input$toggle_error) {
        res <- cbind(round(error_coefs$vals,2), round(error_coefs$se,2),round(error_coefs$lower,2), round(error_coefs$upper,2))
        res <- cbind(c("Intercept", "Slope"), res)
        colnames(res) <- c(" ", "Estimate", "SE", "Lower", "Upper")
        res
      }
    }
    else {
      NULL
    }
  })
  
}