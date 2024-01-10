#' overall_data_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overall_data_viz_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        fluidRow(
          column(6,
                 "Key Performance Indicators (KPIs) Summary."),
          column(3,
                 dateRangeInput(ns("date"), "Date:", start = min(get_raw_data()$Day), end = max(get_raw_data()$Day))),
          column(3,
                 actionButton(ns("inputs_back"), "Reset Date!"))
        )
      ),
      bslib::card_body(
        bslib::layout_column_wrap(

          width = "250px",
          bslib::value_box(
            title = "Number of Calves",
            value = textOutput(ns("calves")),
            showcase = bsicons::bs_icon("clipboard2-data")
          ),
          bslib::value_box(
            title = "Average Milk Consumption",
            value = textOutput(ns("milk_cons")),
            showcase = bsicons::bs_icon("graph-up")
          ),
          bslib::value_box(
            title = "Average Drinking Speed",
            value = textOutput(ns("drink_speed")),
            showcase = bsicons::bs_icon("graph-up")
          ),
          bslib::value_box(
            title = "Visits With Consumption",
            value = textOutput(ns("vis_with_cons")),
            showcase = bsicons::bs_icon("bucket")
          ),
          bslib::value_box(
            title = "Visits With Credit Without Consumption",
            value = textOutput(ns("vis_with_credit_without_cons")),
            showcase = bsicons::bs_icon("bucket")
          ),
          bslib::value_box(
            title = "Visits Without Credit",
            value = textOutput(ns("vis_without_credit")),
            showcase = bsicons::bs_icon("bucket")
          )
        )
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        fluidRow(
          column(6,
                 "Visualization by Feeder Over Time and Overall Summarization."),
          column(6,
                selectInput(ns("var_selection"), "Variable:", choices = c("No. Calves",
                                                                  "Milk Cons. (l)",
                                                                  "Drink. Speed",
                                                                  "Vis. With Cons.",
                                                                  "Vis. With Credit Without Cons.",
                                                                  "Vis. Without Credit"),
                    selected = "Drink. Speed"))
      )),
      bslib::card_body(
        min_height = 350,
        bslib::layout_column_wrap(
          width = 1/2,
          shinycustomloader::withLoader(plotly::plotlyOutput(ns("plot")), type = "html", loader = "loader1"),
          shinycustomloader::withLoader(DT::dataTableOutput(ns("sum_table")), type = "html", loader = "loader1")
        )
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Plot and Table with Calf Level Data."
      ),
      bslib::card_body(
        fluidRow(
          column(3,
                 shinyWidgets::pickerInput(ns("feeder_number"), "Feeder:", choices = unique(get_raw_data()$FeederNumber), options = list(container = "body"))),
          column(3,
                 shinyWidgets::pickerInput(ns("calf"), "Calf Number:", choices = unique(get_raw_data()$CalfNumber), selected = "4872", options = list(container = "body", `actions-box` = TRUE), multiple = TRUE)),
          column(3,
                 shinyWidgets::pickerInput(ns("var_y"), "Variable:", choices = names(get_raw_data()[6:23]), selected = "Amount", options = list(container = "body"))))),
      min_height = 450,
      bslib::layout_column_wrap(
        width = 1/2,
        shinycustomloader::withLoader(plotly::plotlyOutput(ns("calf_plot")), type = "html", loader = "loader1"),
        shinycustomloader::withLoader(DT::dataTableOutput(ns("calf_table")), type = "html", loader = "loader1")
      )
    ),

    bslib::card(
      bslib::card_header(
        "Report with all Data Summarized."
      ),
      bslib::card_body(
        fluidRow(
          column(2,
                 offset = 10,
                 downloadButton(ns("downloadreport"), label = "Report.html!")),

          # column(2,
          #        offset = 8,
          #        downloadButton(ns("downloadreport_pdf"), label = "Report.pdf!"))
        )
      )
    )
  )
}

#' overall_data_viz Server Functions
#'
#' @noRd
mod_overall_data_viz_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$inputs_back, {

      updateDateRangeInput(session, inputId = "date", start = min(get_raw_data()$Day), end = max(get_raw_data()$Day))

      shinyWidgets::updatePickerInput(session, inputId = "feeder_number", choices = unique(get_raw_data()$FeederNumber), selected = "1")

    })

    data_filtered <- reactive({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::filter(FeederNumber == input$feeder_number)

    })

    observeEvent(data_filtered(), {

      choices <- unique(data_filtered()$CalfNumber)

      shinyWidgets::updatePickerInput(session, inputId = "calf", choices = choices, selected = choices[1], options = list(container = "body", `actions-box` = TRUE))

    })

    output$calves <- renderText({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::ungroup() |>
        dplyr::summarise(calves = dplyr::n_distinct(CalfNumber)) |>
        dplyr::pull(calves)

    })

    output$milk_cons <- renderText({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::summarise(milk = mean(AverageConsumption, na.rm = TRUE)) |>
        dplyr::pull(milk) |>
        round(2)

    })

    output$drink_speed <- renderText({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::summarise(speed = mean(AverageDrinkingSpeed, na.rm = TRUE)) |>
        dplyr::pull(speed) |>
        round(2)
    })

    output$vis_with_cons <- renderText({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::summarise(visits = mean(VisitsWithConsum, na.rm = TRUE)) |>
        dplyr::pull(visits) |>
        round(2)

    })

    output$vis_with_credit_without_cons <- renderText({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::summarise(visits = mean(VisitsWithCreditWithoutConsum, na.rm = TRUE)) |>
        dplyr::pull(visits) |>
        round(2)

    })

    output$vis_without_credit <- renderText({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::summarise(visits = mean(VisitsWithoutCredit, na.rm = TRUE)) |>
        dplyr::pull(visits) |>
        round(2)

    })

    output$plot <- plotly::renderPlotly({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::select(-LOM, -Iso) |>
        dplyr::group_by(FeederNumber, CalfAge) |>
        dplyr::summarise(
          "No. Calves" = dplyr::n_distinct(CalfNumber),
          "Milk Cons. (l)" = mean(AverageConsumption, na.rm = TRUE),
          "Drink. Speed" = mean(AverageDrinkingSpeed, na.rm = TRUE),
          "Vis. With Cons." = mean(VisitsWithConsum, na.rm = TRUE),
          "Vis. With Credit Without Cons." = mean(VisitsWithCreditWithoutConsum, na.rm = TRUE),
          "Vis. Without Credit" = mean(VisitsWithoutCredit, na.rm = TRUE)) |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), round, 2)
        ) |>
        plotly::plot_ly(x = ~CalfAge,
                        y = ~get(input$var_selection),
                        type = "scatter",
                        mode = "lines+markers",
                        marker = list(size = 8),
                        color = ~FeederNumber,
                        text = ~paste("<b></b><br>",
                                      "No Calves:", `No. Calves`, "</br>",
                                      "Milk Cons.:", `Milk Cons. (l)`, "</br>",
                                      "Drink. Speed:", `Drink. Speed`, "</br>",
                                      "Vis. With Cons.:", `Vis. With Cons.`, "</br>",
                                      "Vis. With Credit Without Cons.:", `Vis. With Credit Without Cons.`, "</br>",
                                      "Vis. Without Credit:", `Vis. Without Credit`, "</br>"
                        ),
                        hoverinfo = "text") |>
        plotly::layout(title = input$var_selection,
                       yaxis = list(title = list(text = input$var_selection)),
                       hoverlabel = list(bgcolor = "white"),
                       legend = list(title = list(text = '<b> Feeder: </b>'))) |>
        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) |>
        plotly::config(displayModeBar = FALSE) |>
        suppressWarnings()

    })

    output$sum_table <- DT::renderDT({

      get_raw_data() |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        dplyr::select(-LOM, -Iso) |>
        dplyr::group_by(FeederNumber) |>
        dplyr::summarise(
          "No. Calves" = dplyr::n_distinct(CalfNumber),
          "Calf Age (days)" = mean(CalfAge, na.rm = TRUE),
          "Milk Cons. (l)" = mean(AverageConsumption, na.rm = TRUE),
          "Drink. Speed" = mean(AverageDrinkingSpeed, na.rm = TRUE),
          "Vis. With Cons." = mean(VisitsWithConsum, na.rm = TRUE),
          "Vis. With Credit Without Cons." = mean(VisitsWithCreditWithoutConsum, na.rm = TRUE),
          "Vis. Without Credit" = mean(VisitsWithoutCredit, na.rm = TRUE)) |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), round, 2)
        ) |>
        tidyr::pivot_longer(cols = -FeederNumber,
                            names_to = "Variables",
                            values_to = "Value") |>
        tidyr::pivot_wider(names_from = FeederNumber,
                           values_from = Value) |>
        DT::datatable(
          options = list(pageLength = 6,
                         autoWidth = TRUE,
                         rownames = FALSE,
                         dom = "t"
        ),
        caption = htmltools::tags$caption(
          style = "caption-side: bottom; text-align: left;",
          " ", htmltools::em(
            paste0("Variables summarized by feeder (1 to 6) and responds to date filter: ",
                   min(input$date), " to ", max(input$date), ".")
          )
        )
      )

    })

    output$calf_plot <- plotly::renderPlotly({

      data_filtered() |>
        dplyr::filter(CalfNumber %in% input$calf) |>
        plotly::plot_ly(x = ~CalfAge,
                        y = ~get(input$var_y),
                        type = "scatter",
                        mode = "lines+markers",
                        marker = list(size = 8),
                        color = ~CalfNumber,
                        text = ~paste("<b></b><br>",
                                      "Calf:", input$calf, "</br>",
                                      input$var_y, ":", get(input$var_y), "</br>",
                                      "Calf Age (days):", CalfAge, "</br>",
                                      "Drink. Speed:", AverageDrinkingSpeed, "</br>",
                                      "Vis. With Cons.:", VisitsWithConsum, "</br>",
                                      "Vis. With Credit Without Cons.:", VisitsWithCreditWithoutConsum, "</br>",
                                      "Vis. Without Credit:", VisitsWithoutCredit, "</br>"
                        ),
                        hoverinfo = "text") |>
        plotly::layout(title = input$var_y,
                       yaxis = list(title = list(text = input$var_y)),
                       hoverlabel = list(bgcolor = "white"),
                       legend = list(title = list(text = '<b> Calf Number: </b>'))) |>
        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE)) |>
        plotly::config(displayModeBar = FALSE) |>
        suppressWarnings()

    })

    output$calf_table <- DT::renderDT({

      data_filtered() |>
        dplyr::select(-LOM, -Iso) |>
        dplyr::filter(CalfNumber %in% input$calf) |>
        dplyr::group_by(FeederNumber, CalfNumber) |>
        dplyr::summarise(
          "No. obs." = dplyr::n(),
          "Calf Age (days)" = mean(CalfAge, na.rm = TRUE),
          "Milk Cons. (l)" = mean(AverageConsumption, na.rm = TRUE),
          "Drink. Speed" = mean(AverageDrinkingSpeed, na.rm = TRUE),
          "Vis. With Cons." = mean(VisitsWithConsum, na.rm = TRUE),
          "Vis. With Credit Without Cons." = mean(VisitsWithCreditWithoutConsum, na.rm = TRUE),
          "Vis. Without Credit" = mean(VisitsWithoutCredit, na.rm = TRUE)) |>
        dplyr::mutate(
          dplyr::across(dplyr::where(is.numeric), round, 2)
        ) |>
        dplyr::ungroup() |>
        tidyr::pivot_longer(cols = -c(FeederNumber, CalfNumber),
                            names_to = "Variables",
                            values_to = "Values") |>
        DT::datatable(
          options = list(pageLength = 7,
                         autoWidth = TRUE,
                         rownames = FALSE
          ),
          caption = htmltools::tags$caption(
            style = "caption-side: bottom; text-align: left;",
            " ", htmltools::em(
              paste0("Variables summarized by calf and respond to date filter: ",
                     min(input$date), " to ", max(input$date), ".")
            )
          )
        )

    })


    # report generation

    all_data_to_report <- get_raw_data()

    output$downloadreport <-

      downloadHandler(
        "GranjaSanJoseReport.html",
        content =
          function(file)
          {

            withProgress(message = "Rendering the report...",
                         detail = "This may take a few seconds...", value = 0.5, {

              path_reportRMD <- system.file("app", "report.Rmd", package = "GranjaSanJoseDashboard")

              path_reportHTML <- system.file("app", "built_report.html", package = "GranjaSanJoseDashboard")

              rmarkdown::render(
                input = path_reportRMD,
                output_file = "built_report.html",

                params = list(
                  all_data = all_data_to_report
                )
              )

              readBin(con = path_reportHTML,
                      what = "raw",
                      n = file.info(path_reportHTML)[ , "size"]) |>

                writeBin(con = file)

            })
          }
      )

    # pdf - TO BE DONE

    output$downloadreport_pdf <-
      downloadHandler(
        "GranjaSanJoseReport.pdf",
        content =
          function(file)
          {
            path_reportRMD <- system.file("app", "report_pdf.Rmd", package = "GranjaSanJoseDashboard")

            path_reportPDF <- system.file("app", "built_report.pdf", package = "GranjaSanJoseDashboard")

            rmarkdown::render(
              input = path_reportRMD,
              output_file = "built_report.pdf",

              params = list(
                all_data = all_data_to_report
              )
            )

            readBin(con = path_reportPDF,
                    what = "raw",
                    n = file.info(path_reportPDF)[ , "size"]) |>

              writeBin(con = file)
          }
      )





  })
}

