#' Server function for polytomous App
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#'
#' @rawNamespace import(shiny, except = dataTableOutput)
#' @importFrom utils read.csv
#'
#' @export
RMPserver <- function(input, output, session) {

  idb <- NULL

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

    if (is.integer(beta1[,1])) {
      beta <- apply(beta1[, -1], 2, as.numeric)
    } else {
      beta <- apply(beta1, 2, as.numeric)
    }


    validate(
      need(!is.null(beta), "Cannot identify item parameters from input file.")
    )

    idb <<- showNotification(paste("Number of items: ", ncol(beta)),
                             duration = NULL, type = "warning")

    beta

  })

  thetaRea <- reactive({

    validate(
      need(input$theta != "", "Please select CSV file with person parameters")
    )

    theta0 <- input$theta
    if (is.null(theta0)) return(NULL)

    #theta <- suppressWarnings(as.numeric(data))
    #validate(
    #  need(anyNA(theta), "All values of person parameters can not be converted to numeric")
    #)

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
      beta <- betaRea()
      if(!dplyr::near(mean(beta), 0, tol = .Machine$double.eps^0.5)) {
        showModal(zeromean_confirm)
      }
    }
  })

  observeEvent(input$ok, {
    beta <- betaRea()
    showNotification(paste("Continue without zero-mean constraint:",
                           "mean = ", signif(mean(beta), digits = 1), collapse = "\n"),
                     duration = NULL)
    removeModal()
  })
  observeEvent(input$cancel, {
    removeModal()
  })

  #toListen <- reactive({
  #  list(thetaRea(),betaRea())
  #})

  #observeEvent(toListen(), {
  #  beta <- as.numeric(betaRea()[, 2]) #betaRea()
  #  theta <- as.numeric(thetaRea()[, 2]) #thetaRea()
  #  if (length(beta) == length(theta)) {
  #    if (all(beta == theta)) {
  #      showModal(nobsnitms_confirm)
  #    }
  #  }
  #})

  #observeEvent(input$okn, {
  #  showNotification("Files of item parameters and person parameters are the same",
  #                   duration = NULL)
  #  removeModal()
  #})
  #observeEvent(input$canceln, {
  #  removeModal()
  #})

  #output$table1 <- renderDataTable({
  #  beta <- as.numeric(betaRea()[,"beta"])
  #  theta <- as.numeric(thetaRea()[,"theta"])
  #
  #  data.frame("Item paramters" = beta,
  #             "Person parameters" = theta)
  #})

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
                      selected = "Outfit")
  })


  #============ Simulate statistics =======================

  selectedData <- eventReactive(input$go, {

    removeNotification(idb)

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
                     selectedData <- simRASCHstats(beta, theta,
                                                   method.item, method.person,
                                                   B,
                                                   model = "RMP",
                                                   trace.it = 1),
                     message = function(m) {
                       val <- as.numeric(m$message)
                       shiny::setProgress(value=val, detail = paste("Iteration", val))
                     }
                   )
                 }
    )



  })

  output$plot2 <- renderPlot({

    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")

    z <- selectedData()

    p1 <- plot(x = z,
                          type = "Outfit", extreme = "min",
                          probs = c(0.025, 0.05),
                          breaks = c("2.5%", "5%"),
                          labels = c("2.5%", "5%", "other"),
                          colours = my_colors,
                          xtitle = "Outfit",
                          title = "Minimal outfit distribution") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
                  type = "Outfit", extreme = "max",
                  probs = c(0.95, 0.975),
                  breaks = c("5%", "2.5%"),
                  labels = c("other", "5%", "2.5%"),
                  colours = my_colors,
                  xtitle = "Outfit",
                  title = "Maximal outfit distribution") +
      ggplot2::theme_minimal()

    ggpubr::ggarrange(plotlist = list(p1, p2),
                      common.legend = TRUE, legend = "bottom")

  })

  output$plot3 <- renderPlot({

    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")

    z <- selectedData()

    p1 <- plot(x = z,
                          type = "Infit", extreme = "min",
                          probs = c(0.025, 0.05),
                          breaks = c("2.5%", "5%"),
                          labels = c("2.5%", "5%", "other"),
                          colours = my_colors,
                          xtitle = "Infit",
                          title = "Minimal infit distribution") +
      ggplot2::theme_minimal()


    p2 <- plot(x = z,
                          type = "Infit", extreme = "max",
                          probs = c(0.95, 0.975),
                          breaks = c("5%", "2.5%"),
                          labels = c("other", "5%", "2.5%"),
                          colours = my_colors,
                          xtitle = "Infit",
                          title = "Maximal infit distribution") +
      ggplot2::theme_minimal()

    ggpubr::ggarrange(plotlist = list(p1, p2),
                      common.legend = TRUE, legend = "bottom")

  })

  output$plot4 <- renderPlot({

    my_colors <- c("seashell3", "seashell2", "seashell")
    names(my_colors) <- c("2.5%", "5%", "other")

    z <- selectedData()

    p1 <- plot(x = z,
                          type = "FitResid", extreme = "min",
                          probs = c(0.025, 0.05),
                          breaks = c("2.5%", "5%"),
                          labels = c("2.5%", "5%", "other"),
                          colours = my_colors,
                          xtitle = "FitResidual",
                          title = "Minimal FitResidual distribution") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
                          type = "FitResid",
                          extreme = "max",
                          probs = c(0.95, 0.975),
                          breaks = c("5%", "2.5%"),
                          labels = c("other", "5%", "2.5%"),
                          colours = my_colors,
                          xtitle = "FitResidual",
                          title = "Maximal FitResidual distribution") +
      ggplot2::theme_minimal()

    ggpubr::ggarrange(plotlist = list(p1, p2),
                      common.legend = TRUE, legend = "bottom")

  })

}
