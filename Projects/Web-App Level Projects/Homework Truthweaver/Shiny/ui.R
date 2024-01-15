dashboardPage(
  skin = "black",
  dashboardHeader(title = "Homework Truthweaver"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home")
    )
  ),
  dashboardBody(
    tabItems(
      # ----Home----
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            fileInput("homeworks", "Choose files", multiple = T, accept = ".pdf"),
            sliderInput("red_flag_threshold", "Set red flag threshold:", min = 0, max = 100, value = 50, width = "20%", post = "%") %>% column(width = 12, align = "center"),
            tags$hr() %>% column(width = 12),
            actionButton("conjure", "Conjure", width = "100") %>% column(width = 12, align = "center")
          ),
          
          
        )
      )
    )
  )
)
