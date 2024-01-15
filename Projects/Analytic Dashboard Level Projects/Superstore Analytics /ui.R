dashboardPage(
  skin = "black",
  dashboardHeader(title = "Superstore Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("chart-simple")),
      menuItem("Sales", tabName = "sales", icon = icon("cart-shopping")),
      menuItem("Profit", tabName = "profit", icon = icon("dollar")),
      menuItem("Product", tabName = "product", icon = icon("tag")),
      menuItem("Data", tabName = "data", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      # ----overview----
      tabItem(
        tabName = "overview",
        fluidRow(
          uiOutput("overview_total_order"),
          uiOutput("overview_total_sales"),
          uiOutput("overview_total_profit"),
          uiOutput("overview_total_customer")
        ),
        fluidRow(
          box(
            width = 12,
            selectInput("overview_select_year", "Tahun:", choices = unique(ecom$Order.Year))
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("overview_order_monthly_trend_by_year")
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput("overview_sales_monthly_trend_by_year")
          ),
          box(
            width = 6,
            plotlyOutput("overview_profit_monthly_trend_by_year")
          )
        ),
        fluidRow(
          box(
            width = 3,
            plotlyOutput("overview_segment_distribution")
          ),
          box(
            width = 3,
            plotlyOutput("overview_category_distribution")
          ),
          box(
            width = 3,
            plotlyOutput("overview_ship_mode_distribution")
          ),
          box(
            width = 3,
            plotlyOutput("overview_region_distribution")
          )
        )
      ),
      
      # ----sales----
      tabItem(
        tabName = "sales",
        fluidRow(
          uiOutput("sales_total_order"),
          uiOutput("sales_total_sales"),
          uiOutput("sales_total_profit"),
          uiOutput("sales_total_customer")
        ),
        fluidRow(
          box(
            width = 12,
            selectInput("sales_select_year", "Tahun:", choices = unique(ecom$Order.Year))
          )
        ),
        fluidRow(
          box(
            title = "Sales Trend",
            width = 12,
            plotlyOutput("sales_monthly_trend_by_year")
          )
        ),
        fluidRow(
          box(
            width = 4,
            plotlyOutput("sales_category_trend")
          ),
          box(
            width = 4,
            plotlyOutput("sales_segment_trend")
          ),
          box(
            width = 4,
            plotlyOutput("sales_region_trend")
          )
        ),
        fluidRow(
          box(
            width = 4,
            plotlyOutput("sales_category_distribution")
          ),
          box(
            width = 4,
            plotlyOutput("sales_segment_distribution")
          ),
          box(
            width = 4,
            plotlyOutput("sales_region_distribution")
          )
        )
      ),
      
      # ----profit----
      tabItem(
        tabName = "profit",
        fluidRow(
          uiOutput("profit_total_order"),
          uiOutput("profit_total_sales"),
          uiOutput("profit_total_profit"),
          uiOutput("profit_total_customer")
        ),
        fluidRow(
          box(
            width = 12,
            selectInput("profit_select_year", "Tahun:", choices = unique(ecom$Order.Year))
          )
        ),
        fluidRow(
          box(
            width = 12,
            plotlyOutput("profit_monthly_trend_by_year")
          )
        ),
        fluidRow(
          box(
            width = 4,
            plotlyOutput("profit_category_trend")
          ),
          box(
            width = 4,
            plotlyOutput("profit_segment_trend")
          ),
          box(
            width = 4,
            plotlyOutput("profit_region_trend")
          )
        ),
        fluidRow(
          box(
            width = 4,
            plotlyOutput("profit_category_distribution")
          ),
          box(
            width = 4,
            plotlyOutput("profit_segment_distribution")
          ),
          box(
            width = 4,
            plotlyOutput("profit_region_distribution")
          )
        )
      ),
      
      # ----product----
      tabItem(
        tabName = "product",
        fluidRow(
          uiOutput("product_total_order"),
          uiOutput("product_total_sales"),
          uiOutput("product_total_profit"),
          uiOutput("product_total_customer")
        ),
        fluidRow(
          box(
            width = 12,
            selectInput("product_select_year", "Tahun:", choices = unique(ecom$Order.Year)),
            selectInput("product_select_segment", "Segment:", choices = c("All", as.character(unique(ecom$Segment))))
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotlyOutput("product_top10_sales")
          ),
          box(
            width = 6,
            plotlyOutput("product_top10_profit")
          )
        )
      ),
      # ----data----
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            title = "Superstore Dataset",
            width = 12,
            dataTableOutput("data_table_superstore")
          )
        )
      )
    )
  )
)
