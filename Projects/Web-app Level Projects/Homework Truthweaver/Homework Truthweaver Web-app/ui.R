dashboardPage(
  skin = "black",
  dashboardHeader(title = "🧝🏻‍♀️ Homework Truthweaver", titleWidth = 275),
  dashboardSidebar(
    sidebarMenu(
      menuItem("🔮 Home", tabName = "home"),
      menuItem("📜 Paper", tabName = "paper"),
      menuItem("🧝🏻 About Ian", tabName = "about_ian")
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
            actionLink("sample_documents", "Need a sample documents to try?") %>% div(style = "float: left; width: 50%;"),
            actionLink("how_to_use", "How to use?") %>% div(style = "text-align: right; margin-top: auto;"),
            fileInput("homeworks", "Choose files:", multiple = T, accept = ".pdf", ),
            sliderInput("red_flag_threshold", "Set red flag threshold:", min = 0, max = 100, value = 50, width = "20%", post = "%") %>% column(width = 12, align = "center"),
            tags$hr() %>% column(width = 12),
            actionButton("conjure", "🪄 Conjure", width = "100") %>% column(width = 12, align = "center")
          ),
          
          # ----Info boxes---
          uiOutput("info_boxes") %>% fluidRow(),
          
          # ----Analytics---
          uiOutput("analytics"),
          
          # ----Pair Analysis---
          uiOutput("pair_analysis")
          
        )
      ),
      # ----Paper----
      tabItem(
        tabName = "paper",
        box(
          width = 12,
          tags$iframe(src = "paper.html", width = "100%", height = 915)
        )
      ),
      # ----About Ian----
      tabItem(
        tabName = "about_ian",
        box(
          width = 12,
          column(
            width = 12,
            h2("Ardian the Truthweaver") %>% column(width = 12, align = "center"),
          ),
          column(
            tags$hr(),
            width = 6,
            tags$img(src = "foto-ian.jpg", width = "100%")
          ),
          column(
            tags$hr(),
            width = 6,
            p("Ardian is a 20 year old, currently in his fourth semester as a computer science student at Universitas Indonesia. In middle school, He was a music producer, producing music for international artist and some Indonesian artists. During high school, He established and managed a local business, sourcing various products from suppliers in China. He then ran an online print on demand business that partnered with a company in Surabaya. He continued running his businesses until the end of his second semester at university. Due to certain circumstances, Ardian decided to discontinue his business and moved to Depok. He doesn’t like to simply rest, attend classes, eat, and exercise, so He went through various selections and considerations to determine the best career path that aligned with his academic major. After months of researching potential career paths, He chose Data Science, and that's how you ended up here. He currently works in project basis as He is still studying in University, but a full-time offer won't immediately scare him away. So, if you are looking for a Data Scientist to enhance your business's productivity and efficiency, ", a("He's waiting for you.", href = "mailto:thegreatardian@gmail.com"), style = "text-align: justify;")
          )
          
        )
      )
    )
  )
)
