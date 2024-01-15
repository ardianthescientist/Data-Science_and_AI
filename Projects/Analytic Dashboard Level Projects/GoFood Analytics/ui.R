dashboardPage(
  skin="black",
  dashboardHeader(
    titleWidth = 300,
    title = tags$span(style = "font-size: 20px;", "GoFood Pontianak Analytics")  # Apply custom CSS for font size
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName="overview", icon=icon("chart-simple")),
      menuItem("Merchant", tabName="merchant", icon=icon("store")),
      menuItem("Customer", tabName="customer", icon=icon("users")),
      menuItem("Data", tabName="data", icon=icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      #----Overview----
      tabItem(
        tabName = "overview",
        fluidRow(
          infoBox(
            "TOTAL ORDERS:",
            width = 3,
            value = paste(nrow(gofood), "orders"),
            icon = icon("bone"),
            color = "green"
          ),
          infoBox(
            "TOTAL UNIQUE MERCHANTS:",
            width = 3,
            value = paste(length(unique(gofood$merchant_name)), "merchants"),
            icon = icon("store"),
            color = "green"
          ),
          infoBox(
            "TOTAL UNIQUE CUSTOMERS:",
            width = 3,
            value = paste(length(unique(gofood$customer_name)), "customers"),
            icon = icon("users"),
            color = "green"
          ),
          infoBox(
            "TOTAL CATEGORIES:",
            width = 3,
            value = paste(length(unique(gofood$merchant_category)), "categories"),
            icon = icon("tags"),
            color = "green"
          ),
        ),
        fluidRow(
          box(
            width = 4,
            plotlyOutput("plot_gofood_category_rank")
          ),
          box(
            width = 8,
            plotlyOutput("plot_gofood_top15_merchant")
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("plot_gofood_order_by_hour")
          )
        )
      ),
      
      #----Merchant----
      tabItem(
        tabName = "merchant",
        fluidRow(
          infoBox(
            "TOTAL ORDERS:",
            width = 3,
            value = paste(nrow(gofood), "orders"),
            icon = icon("bone"),
            color = "green"
          ),
          infoBox(
            "TOTAL UNIQUE MERCHANTS:",
            width = 3,
            value = paste(length(unique(gofood$merchant_name)), "merchants"),
            icon = icon("store"),
            color = "green"
          ),
          infoBox(
            "TOTAL UNIQUE CUSTOMERS:",
            width = 3,
            value = paste(length(unique(gofood$customer_name)), "customers"),
            icon = icon("users"),
            color = "green"
          ),
          infoBox(
            "TOTAL CATEGORIES:",
            width = 3,
            value = paste(length(unique(gofood$merchant_category)), "categories"),
            icon = icon("tags"),
            color = "green"
          )
        ),
        fluidRow(
          box(
            width = 12,
            selectInput("selected_merchant_category", "Pilih kategori merchant:", c("All", as.character(unique(gofood$merchant_category)))),
            height = "90px"
          )
        ),
        fluidRow(
          box(
            width = 12,
            uiOutput("dynamic_slider_input"),
            height = "100px"
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput("plot_gofood_top_n_merchant_total_order_by_category")
          ),
          box(
            width = 6,
            plotlyOutput("plot_gofood_top_n_merchant_total_transaction_by_category")
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("plot_gofood_merchant_by_kecamatan")
          )
        )
      ),
      
      #----Customer----
      tabItem(
        tabName = "customer",
        fluidRow(
          infoBox(
            "TOTAL ORDERS:",
            width = 3,
            value = paste(nrow(gofood), "orders"),
            icon = icon("bone"),
            color = "green"
          ),
          infoBox(
            "TOTAL UNIQUE MERCHANTS:",
            width = 3,
            value = paste(length(unique(gofood$merchant_name)), "merchants"),
            icon = icon("store"),
            color = "green"
          ),
          infoBox(
            "TOTAL UNIQUE CUSTOMERS:",
            width = 3,
            value = paste(length(unique(gofood$customer_name)), "customers"),
            icon = icon("users"),
            color = "green"
          ),
          infoBox(
            "TOTAL CATEGORIES:",
            width = 3,
            value = paste(length(unique(gofood$merchant_category)), "categories"),
            icon = icon("tags"),
            color = "green"
          )
        ),
        fluidRow(
          box(
            width = 12,
            selectInput("selected_customer_category", "Pilih kategori merchant:", c("All", as.character(unique(gofood$merchant_category)))),
            height = "100px"
          )
        ),
        fluidRow(
          box(
            plotlyOutput("plot_gofood_category_order_by_hour")
          ),
          box(
            plotlyOutput("plot_gofood_customer_by_age")
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("plot_gofood_customer_by_kecamatan")
          )
        )
      ),
      
      #----data----
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            "Dataset GoFood Pontianak September 2018 - Maret 2019",
            width = 12,
            dataTableOutput("table_data_gojek")
          ),
        )
      )
    )
  )
)