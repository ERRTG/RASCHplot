#' Server function for dichotomous App
#' @param input input set by Shiny.
#' @param output output set by Shiny.
#' @param session session set by Shiny.
#'
#' @rawNamespace import(shiny, except = dataTableOutput)
#' @importFrom utils read.csv
#'
#' @examples
#' library(iarm)
#' data(amts)
#' it.AMTS <- amts[,4:13]
#' it.AMTSc <- it.AMTS[complete.cases(it.AMTS), ]
#' idx <- which(rowSums(it.AMTSc) %in% c(0,ncol(it.AMTSc)))
#' dat <- it.AMTSc[-idx,]
#' object <- eRm::RM(dat)
#' pp <- eRm::person.parameter(object)
#' delta <- -object$betapar
#' theta <- pp$thetapar$NAgroup1
#' shinyApp(RMDui, RMDserver)
#'
#' @noRd
RMDserver <- function(input, output, session) {

  #my_colors <- c("seashell3", "seashell2", "seashell")
  colpal <- c("#CFCFC2", "#95DA4C", "#3F8058", "#2980B9", "#F67400", "#7F8C8D", "#FDBC4B", "#3DAEE9", "#27AEAE", "#7A7C7D", "#7F8C8D", "#A43340", "#2980B9", "#F67400", "#DA4453", "#0099FF", "#F67400", "#8E44AD", "#27AE60", "#C45B00", "#CFCFC2", "#CFCFC2", "#27AE60", "#27AE60", "#2980B9", "#3DAEE9", "#DA4453", "#F44F4F", "#27AEAE", "#DA4453", "#DA4453")
  my_colors <- colpal[c(12, 28, 1)]
  names(my_colors) <- c("2.5%", "5%", "other")

  deltaRea <- reactive({

    validate(
      need(input$delta != "", "Please select CSV file with item parameters")
    )

    delta0 <- input$delta
    if (is.null(delta0)) return(NULL)

    delta1 <- read.csv(delta0$datapath,
                      header = input$headerDelta#,
                      #sep = input$sepDelta,
                      #quote = input$quoteDelta
    )

    if (any(class(delta1) %in% c("matrix", "data.frame", "array"))) {
      if (any(colnames(delta1) == "delta")) {
        delta <- as.numeric(delta1[,"delta"])
      } else if (is.integer(delta1[, 1])) {
        delta <- as.numeric(delta1[, 2])
      } else if (ncol(delta1) == 1) {
        delta <- as.numeric(delta1[,1])
      }
    } else if (any(class(delta1) == "numeric")) {
      delta <- delta1
    } else {
      delta <- NULL
    }

    validate(
      need(!is.null(delta), "Cannot identify item parameters from input file.")
    )

    delta

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

  observeEvent(deltaRea(), {

    if(input$method == "JML") {
      if(!dplyr::near(mean(deltaRea()), 0, tol = .Machine$double.eps^0.5)) {
        showModal(zeromean_confirm)
      }
    }

  })

  observeEvent(input$ok, {

    showNotification(paste("Continue without zero-mean constraint:",
                           "mean = ", signif(mean(deltaRea()), digits = 1), collapse = "\n"),
                     duration = NULL)
    removeModal()

  })

  observeEvent(input$cancel, {
    removeModal()
  })

  toListen <- reactive({
    list(thetaRea(),deltaRea())
  })

  observeEvent(toListen(), {
    delta <- deltaRea()
    theta <- thetaRea()
    if (length(delta) == length(theta)) {
      if (all(delta == theta)) {
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

  output$tbl1 <- DT::renderDataTable(DT::datatable(data.frame(deltaRea()),
                                 options = list(searching = FALSE, scrollX = T),
                                 rownames= FALSE) %>%
                                   DT::formatRound(1, 4)
  )

  output$tbl2 <- DT::renderDataTable(DT::datatable(data.frame(thetaRea()),
                                 options = list(searching = FALSE, scrollX = T),
                                 rownames= FALSE) %>%
                                   DT::formatRound(1, 4)
  )

  observeEvent(input$go, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Outfit")
  })

  #============ Simulate statistics =======================

  selectedData <- eventReactive(input$go, {

      delta <- deltaRea()
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
                       selectedData <- rRMDstats(delta = delta, theta = theta,
                                            method.item = method.item, method.person = method.person,
                                            B = B,
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

    z <- selectedData()

    p1 <- plot(x = z,
               type = "Outfit", extreme = "min",
               probs = c(0.025, 0.05),
               breaks = c("2.5%", "5%"),
               labels = c("2.5%", "5%", "other"),
               colours = my_colors,
               xtitle = "Minimal Outfit",
               title = "") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
               type = "Outfit", extreme = "max",
               probs = c(0.95, 0.975),
               breaks = c("5%", "2.5%"),
               labels = c("other", "5%", "2.5%"),
               colours = my_colors,
               xtitle = "Maximal Outfit",
               title = "") +
      ggplot2::theme_minimal()

    pp <- ggpubr::ggarrange(plotlist = list(p1, p2),
                            common.legend = TRUE, legend = "bottom")

    toptit <- "Outfit distribution"
    ggpubr::annotate_figure(pp, top = ggpubr::text_grob(toptit))

  })

  output$plot3 <- renderPlot({

    z <- selectedData()

    p1 <- plot(x = z,
               type = "Infit", extreme = "min",
               probs = c(0.025, 0.05),
               breaks = c("2.5%", "5%"),
               labels = c("2.5%", "5%", "other"),
               colours = my_colors,
               xtitle = "Minimal Infit",
               title = "") +
      ggplot2::theme_minimal()


    p2 <- plot(x = z,
               type = "Infit", extreme = "max",
               probs = c(0.95, 0.975),
               breaks = c("5%", "2.5%"),
               labels = c("other", "5%", "2.5%"),
               colours = my_colors,
               xtitle = "Maximal Infit",
               title = "") +
      ggplot2::theme_minimal()

    pp <- ggpubr::ggarrange(plotlist = list(p1, p2),
                            common.legend = TRUE, legend = "bottom")

    toptit <- "Infit distribution"
    ggpubr::annotate_figure(pp, top = ggpubr::text_grob(toptit))

  })

  output$plot4 <- renderPlot({

    z <- selectedData()

    p1 <- plot(x = z,
               type = "FitResid", extreme = "min",
               probs = c(0.025, 0.05),
               breaks = c("2.5%", "5%"),
               labels = c("2.5%", "5%", "other"),
               colours = my_colors,
               xtitle = "Minimal fit residual",
               title = "") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
               type = "FitResid",
               extreme = "max",
               probs = c(0.95, 0.975),
               breaks = c("5%", "2.5%"),
               labels = c("other", "5%", "2.5%"),
               colours = my_colors,
               xtitle = "Maximal fit residual",
               title = "") +
      ggplot2::theme_minimal()

    pp <- ggpubr::ggarrange(plotlist = list(p1, p2),
                            common.legend = TRUE, legend = "bottom")

    toptit <- "Fit residual distribution"
    ggpubr::annotate_figure(pp, top = ggpubr::text_grob(toptit))

  })

  output$plot5 <- renderPlot({

    z <- selectedData()

    p1 <- plot(x = z,
               type = "tOutfit", extreme = "min",
               probs = c(0.025, 0.05),
               breaks = c("2.5%", "5%"),
               labels = c("2.5%", "5%", "other"),
               colours = my_colors,
               xtitle = "Minimal t-Outfit",
               title = "") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
               type = "tOutfit",
               extreme = "max",
               probs = c(0.95, 0.975),
               breaks = c("5%", "2.5%"),
               labels = c("other", "5%", "2.5%"),
               colours = my_colors,
               xtitle = "Maximal t-Outfit",
               title = "") +
      ggplot2::theme_minimal()

    pp <- ggpubr::ggarrange(plotlist = list(p1, p2),
                      common.legend = TRUE, legend = "bottom")

    toptit <- "Wilson-Hilferty cube-root transformed Outfit (t-Outfit) distribution"
    ggpubr::annotate_figure(pp, top = ggpubr::text_grob(toptit))

  })

  output$plot6 <- renderPlot({

    z <- selectedData()

    p1 <- plot(x = z,
               type = "tInfit", extreme = "min",
               probs = c(0.025, 0.05),
               breaks = c("2.5%", "5%"),
               labels = c("2.5%", "5%", "other"),
               colours = my_colors,
               xtitle = "Minimal t-Infit",
               title = "") +
      ggplot2::theme_minimal()

    p2 <- plot(x = z,
               type = "tInfit",
               extreme = "max",
               probs = c(0.95, 0.975),
               breaks = c("5%", "2.5%"),
               labels = c("other", "5%", "2.5%"),
               colours = my_colors,
               xtitle = "Maximal t-Infit",
               title = "") +
      ggplot2::theme_minimal()

    pp <- ggpubr::ggarrange(plotlist = list(p1, p2),
                            common.legend = TRUE, legend = "bottom")

    toptit <- "Wilson-Hilferty cube-root transformed Infit (t-Infit) distribution"
    ggpubr::annotate_figure(pp, top = ggpubr::text_grob(toptit))

  })

}
