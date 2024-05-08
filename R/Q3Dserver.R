#' Server function for dichotomous Q3 App
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#'
#' @rawNamespace import(shiny, except = dataTableOutput)
#' @importFrom utils read.csv
#'
#' @noRd
Q3Dserver <- function(input, output, session) {

  #my_colors <- c("seashell3", "seashell2", "seashell")
  colpal <- c("#CFCFC2", "#95DA4C", "#3F8058", "#2980B9", "#F67400", "#7F8C8D", "#FDBC4B", "#3DAEE9", "#27AEAE", "#7A7C7D", "#7F8C8D", "#A43340", "#2980B9", "#F67400", "#DA4453", "#0099FF", "#F67400", "#8E44AD", "#27AE60", "#C45B00", "#CFCFC2", "#CFCFC2", "#27AE60", "#27AE60", "#2980B9", "#3DAEE9", "#DA4453", "#F44F4F", "#27AEAE", "#DA4453", "#DA4453")
  my_colors <- colpal[c(12, 28, 1)]
  names(my_colors) <- c("2.5%", "5%", "other")

  betaRea <- reactive({

    validate(
      need(input$beta != "", "Please select CSV file with item parameters")
    )

    beta0 <- input$beta
    if (is.null(beta0)) return(NULL)

    beta1 <- read.csv(beta0$datapath,
                      header = input$headerBeta#,
                      #sep = input$sepBeta,
                      #quote = input$quoteBeta
    )

    if (any(class(beta1) %in% c("matrix", "data.frame", "array"))) {
      if (any(colnames(beta1) == "beta")) {
        beta <- as.numeric(beta1[,"beta"])
      } else if (is.integer(beta1[, 1])) {
        beta <- as.numeric(beta1[, 2])
      } else if (ncol(beta1) == 1) {
        beta <- as.numeric(beta1[,1])
      }
    } else if (any(class(beta1) == "numeric")) {
      beta <- beta1
    } else {
      beta <- NULL
    }

    validate(
      need(!is.null(beta), "Cannot identify item parameters from input file.")
    )

    beta

  })

  thetaRea <- reactive({

    validate(
      need(input$theta != "", "Please select CSV file with person parameters")
    )

    theta0 <- input$theta
    if (is.null(theta0)) return(NULL)

    theta1 <- read.csv(theta0$datapath,
                       header = input$headerTheta#,
                       #sep = input$sepTheta,
                       #quote = input$quoteTheta
    )

    if (any(class(theta1) %in% c("matrix", "data.frame", "array"))) {
      if (any(colnames(theta1) == "theta")) {
        theta <- as.numeric(theta1[,"theta"])
      } else if (is.integer(theta1[, 1])) {
        theta <- as.numeric(theta1[, 2])
      } else if (ncol(theta1) == 1) {
        theta <- as.numeric(theta1[,1])
      }
    } else if (any(class(theta1) == "numeric")) {
      theta <- theta1
    } else {
      theta <- NULL
    }

    validate(
      need(!is.null(theta), "Cannot identify person parameters from input file.")
    )

    theta


  })

  observeEvent(betaRea(), {

    if(input$method == "JML") {
      if(!dplyr::near(mean(betaRea()), 0, tol = .Machine$double.eps^0.5)) {
        showModal(zeromean_confirm)
      }
    }

  })

  observeEvent(input$ok, {

    showNotification(paste("Continue without zero-mean constraint:",
                           "mean = ", signif(mean(betaRea()), digits = 1), collapse = "\n"),
                     duration = NULL)
    removeModal()

  })

  observeEvent(input$cancel, {
    removeModal()
  })

  toListen <- reactive({
    list(thetaRea(),betaRea())
  })

  observeEvent(toListen(), {
    beta <- betaRea()
    theta <- thetaRea()
    if (length(beta) == length(theta)) {
      if (all(beta == theta)) {
        showModal(nobsnitms_confirm)
      }
    }
  })

  observeEvent(input$okn, {
    showNotification("Files of item parameters and person parameters are the same",
                     duration = NULL)
    removeModal()
  })
  observeEvent(input$canceln, {
    removeModal()
  })

  output$tbl1 <- DT::renderDataTable(DT::datatable(data.frame(betaRea()),
                                 options = list(searching = FALSE, scrollX = T),
                                 rownames= FALSE)
  )

  output$tbl2 <- DT::renderDataTable(DT::datatable(data.frame(thetaRea()),
                                 options = list(searching = FALSE, scrollX = T),
                                 rownames= FALSE)
  )

  observeEvent(input$go, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Output")
  })

  #============ Simulate statistics =======================

  selectedData <- eventReactive(input$go, {

      beta <- betaRea()
      theta <- thetaRea()

      method.item <- input$method
      method.person <- input$methodpp
      #if (input.method == "JML") {
      #  jlm.adj <- input$adj
      #}

      B <- input$B
      set.seed(1)

      withProgress(message = "Simulating",
                   max = B,
                   value = 0,
                   expr = {
                     withCallingHandlers(
                       selectedData <- simRASCHq3(beta, theta,
                                                  method.item, method.person,
                                                  B,
                                                  model = "RMD",
                                                  trace.it = 1),
                       message = function(m) {
                         val <- as.numeric(m$message)
                         shiny::setProgress(value=val, detail = paste("Iteration", val))
                         }
                       )
                     }
                   )
  })

  output$plot <- renderPlot({

    z <- selectedData()

    p1 <- plot(x = z,
               type = "Q3", extreme = "min",
               probs = c(0.025, 0.05),
               breaks = c("2.5%", "5%"),
               labels = c("2.5%", "5%", "other"),
               colours = my_colors,
               xtitle = "Minimal Q3",
               title = "") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
               type = "Q3", extreme = "max",
               probs = c(0.95, 0.975),
               breaks = c("5%", "2.5%"),
               labels = c("other", "5%", "2.5%"),
               colours = my_colors,
               xtitle = "Maximal Q3",
               title = "") +
      ggplot2::theme_minimal()

    pp <- ggpubr::ggarrange(plotlist = list(p1, p2),
                            common.legend = TRUE, legend = "bottom")

    toptit <- "Q3 distribution"
    ggpubr::annotate_figure(pp, top = ggpubr::text_grob(toptit))

  })

}
