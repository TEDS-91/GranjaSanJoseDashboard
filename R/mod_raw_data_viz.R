#' raw_data_viz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_raw_data_viz_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::card(
      full_screen = TRUE,
      height = 230,
      bslib::card_header(
        "Filters"
      ),
      bslib::card_body(
        fluidRow(
          column(3,
                 shinyWidgets::pickerInput(ns("feeder_number"), "Feeder:", choices = unique(get_raw_data()$FeederNumber), options = list(container = "body"))),
          column(3,
                 dateRangeInput(ns("date"), "Date:", start = min(get_raw_data()$Day), end = max(get_raw_data()$Day))),
          column(3,
                 shinyWidgets::pickerInput(ns("calf"), "Calf Number:", choices = unique(get_raw_data()$CalfNumber), selected = "4872", options = list(container = "body", `actions-box` = TRUE), multiple = TRUE)),
          column(3,
                 shinyWidgets::pickerInput(ns("var_y"), "Variable:", choices = names(get_raw_data()[6:23]), selected = "Amount", options = list(container = "body")))
        ),
        column(2,
               offset = 10,
               actionButton(ns("inputs_back"), "Inputs back!"))

      )
    ),


    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Key Performance Indicators (KPIs) summary."
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
        "Visualization by Calf"
      ),
      bslib::card_body(
        plotly::plotlyOutput(ns("plot")),
      )
    ),

    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        "Table with raw data."
      ),
      bslib::card_body(
        DT::dataTableOutput(ns("table"))
      )
    )

  )
}

#' raw_data_viz Server Functions
#'
#' @noRd
mod_raw_data_viz_server <- function(id){
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

      data_filtered() |>
        dplyr::ungroup() |>
        dplyr::summarise(calves = dplyr::n_distinct(CalfNumber)) |>
        dplyr::pull(calves)

    })

    output$milk_cons <- renderText({

      data_filtered() |>
        dplyr::summarise(milk = mean(AverageConsumption, na.rm = TRUE)) |>
        dplyr::pull(milk) |>
        round(2)

    })

    output$drink_speed <- renderText({

      data_filtered() |>
        dplyr::summarise(speed = mean(AverageDrinkingSpeed, na.rm = TRUE)) |>
        dplyr::pull(speed) |>
        round(2)
    })

    output$vis_with_cons <- renderText({

      data_filtered() |>
        dplyr::summarise(visits = mean(VisitsWithConsum, na.rm = TRUE)) |>
        dplyr::pull(visits) |>
        round(2)

    })

    output$vis_with_credit_without_cons <- renderText({

      data_filtered() |>
        dplyr::summarise(visits = mean(VisitsWithCreditWithoutConsum, na.rm = TRUE)) |>
        dplyr::pull(visits) |>
        round(2)

    })

    output$vis_without_credit <- renderText({

      data_filtered() |>
        dplyr::summarise(visits = mean(VisitsWithoutCredit, na.rm = TRUE)) |>
        dplyr::pull(visits) |>
        round(2)

    })

    output$plot <- plotly::renderPlotly({

      data_filtered() |>
        dplyr::filter(CalfNumber %in% input$calf) |>
        plotly::plot_ly(x = ~CalfAge,
                        y = ~get(input$var_y),
                        type = "scatter",
                        mode = "lines+markers",
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
        plotly::config(displayModeBar = FALSE) |>
        suppressWarnings()

    })

    output$table <- DT::renderDT({

      data_filtered() |>
        dplyr::select(-LOM, -Iso) |>
        dplyr::filter(Day >= input$date[1] & Day <= input$date[2]) |>
        #dplyr::filter(CalfNumber %in% input$calf) |>
        dplyr::filter(FeederNumber == input$feeder_number) |>
        DT::datatable()
    })

  })
}

