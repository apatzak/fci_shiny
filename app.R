#### Frayed CI's Shiny web application
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

### conventional CI
ci <- function(n, m, sd, p){
  error <- qt((1 - (1-p)/2), df = (n-1)) * sd / sqrt(n)
  cil <- m - error
  cir <- m + error
  return(c(cil, cir))
}


### frayed CI's
cif <- function(n, m, sd, p){
  rsd <- replicate(100, {   # randomly draw sd's from a normal distribution with parameters of the sample
    sd(rnorm(n = n, mean = m, sd = sd))
  })
  error <- qnorm(1 - (1-p)/2) * rsd/sqrt(n) # compute error based on a normal distribution, using random sd's
  cil <- m - error    # compute the left fence of the CI
  cir <- m + error    # compute the right fence of the CI
  return(c(cilc = min(cil), cill = max(cil), cirl = min(cir), circ = max(cir)))
}



## Coverage
coverage <- function(n, m, sd, p){
  cis <- replicate(100, {
    sample <- rnorm(n, m, sd)
    c(cif(n, mean(sample), sd(sample), p),
      ci(n, mean(sample), sd(sample), p))
  })
  inner <- mean(cis[2,] < m & cis[3,] > m)
  conventional <- mean(cis[5,] < m & cis[6,] > m)
  outer <- mean(cis[1,] < m & cis[4,] > m)
  return(c(inner, conventional, outer))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  # Application title
  titlePanel("Compute Conventional and Frayed Confidence Intervals (CI)"),

  # Sidebar with text input for sample parameters and dropdown for type I error
  sidebarLayout(
    sidebarPanel(
      textInput("n",
                "Sample size:"),
      textInput("m",
                "Sample mean:"),
      textInput("sd",
                "Sample standard deviation:"),
      numericInput("seed",
                   "Seed to replicate random values:",
                   123,
                   min = -10000,
                   max = 10000),
      sliderInput("p",
                  "Confidence level",
                  min = 0.5, max = .995, step = .005, value = .99)
    ),

    # Show FCI's
    mainPanel(
      tableOutput("results"),
      textOutput("help"),
      plotOutput("cisplot")
    )
  ),
  div(HTML("<hr style='margin-top: 2em;'/><h3>Details</h3>
    <h5><i>What is coverage?</i></h5>
    <p>Coverage is the percentage of CI's that contain the true parameter.</p>
    <h5><i>What is a relative span?</i></h5>
    <p>Relative span is the span of a frayed CI divided by the span of a conventional CI.</p>
    <h5><i>What are frayed CI?</i></h5>
    <p>Researchers are urged to report confidence intervals (CI) to qualify estimates of population parameters
	   and effect sizes observed in studies. Many CIs include two estimates: (a) the focal point estimate, 
	   e.g., a mean, correlation or effect size; and (b) a second point estimate used in estimating the focal
	   point estimate’s standard error (variability). Logic commending CIs to qualify the focal point estimate
	   in the first place applies equally to the standard error of the point estimate, but the latter is often
	   overlooked. Quantifying sampling variability at the fences of a conventional CI produces a frayed confidence
	   interval (FCI). A FCI is illustrated for a random sample’s mean. How much fraying matters and interpretive
	   issues attending FCIs are described.</p>
    <h5><i>For more information:</i></h5>
    <p>Winne, P. H., & Patzak, A. (2021). Frayed confidence intervals temper confidence in confidence intervals.
	   Manuscript submitted for publication.</p>")),

  div(HTML("<hr style='margin-top: 2em;'/><p class='small'>© Alexandra Patzak 2020</p>
    <p class='small'>The content published at https://frayedci.shinyapps.io/frayed_CI/ is licensed under a
	<a rel='license' href='http://creativecommons.org/licenses/by/4.0/'>Creative Commons Attribution 4.0 International License</a>."))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  observe({
    p <- as.numeric(input$p)
    n <- as.numeric(input$n)
    m <- as.numeric(input$m)
    sd <- as.numeric(input$sd)
    seed <- as.integer(input$seed)

    if (length(m) == 1 && !is.na(m) &&
        length(sd) == 1 && !is.na(sd) &&
        length(n) == 1 && !is.na(n)) {
      if(isTRUE(n < 501) && isTRUE(n > 1)){
        if(isTRUE(length(seed) == 1) && !is.na(seed)){
          set.seed(seed)
        }

        fci <- cif(n, m, sd, p)
        cci <- ci(n, m, sd, p)
        cov <- coverage(n, m, sd, p)

        output$results <- renderTable({
          data.frame(Lower = c(fci[2], cci[1], fci[1]), Upper = c(fci[3], cci[2], fci[4]), Coverage = cov,
                     `Relative Span` = c((fci[3]-fci[2])/(cci[2]-cci[1]), 1, (fci[4]-fci[1])/(cci[2]-cci[1])),
                     row.names = c("Frayed CI inner fences", "Conventional CI", "Frayed CI outer fences"),
                     check.names = FALSE)
        }, rownames = TRUE)

        output$cisplot <- renderPlot({

          name <- factor(
            c("Frayed CI outer fences", "Frayed CI inner fences", "Conventional CI"),
            levels = rev(c("Frayed CI inner fences", "Conventional CI", "Frayed CI outer fences")))
          left <- c(fci[c(1,2)], cci[1])
          right <- c(fci[c(4, 3)], cci[2])
          data <- data.frame(name, m, left, right)

          ggplot(data,
                 aes(x = name, y = m,
                     ymin = left, ymax = right))+
            geom_errorbar(size = 1)+
            geom_point(size = 3)+
            labs(y = "Mean and range of confidence intervals",
                 x = "")+
            geom_hline(yintercept = m,
                       linetype = "22")+
            coord_flip()+
            theme_gray(base_size = 16)
        })
      }
    }
  })
  output$help <- renderText({
    p <- as.numeric(input$p)
    n <- as.numeric(input$n)
    m <- as.numeric(input$m)
    sd <- as.numeric(input$sd)
    seed <- as.integer(input$seed)
    status <- ''
    if (length(m) == 1 && !is.na(m) &&
        length(sd) == 1 && !is.na(sd) &&
        length(n) == 1 && !is.na(n)) {
      if (isTRUE(n > 500)) {
        status <- "Please enter a sample size smaller than 500."
      }
      if(isTRUE(n < 2)){
        status <- "Please enter a sample size larger than 1."
      }
      if (length(seed) == 0 || isTRUE(is.na(seed))) {
        status <- paste(status,
                        "Frayed confidence intervals are computed by simulating many random samples. if you do not set a seed, values will vary slightly.",
                        sep = "\n")
      }
    } else {
      status <- "Please enter your values."
    }
    status
  })
}


# Run the application
shinyApp(ui = ui, server = server)
