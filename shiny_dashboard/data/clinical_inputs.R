fluidRow(
  # Age at diagnosis
  column(4,
         numericInput("ageInput",
                      h3("Patient Age"),
                      value = 65)),
  # Type of surgery
  column(
    4,
    selectInput(
      "surgerytypeInput",
      h3("Type of surgery"),
      choices = list("BREAST CONSERVING" = "BREAST CONSERVING",
                     "MASTECTOMY" = "MASTECTOMY"),
      selected = "MASTECTOMY"
    )
  ),
  # cellularity
  column(
    4,
    selectInput(
      "cellularityInput",
      h3("Cellularity"),
      choices = list(
        "Low" = "Low",
        "Moderate" = "Moderate",
        "High" = "High"
      ),
      selected = "Moderate"
    )
  ),
  # chemotherapy
  column(
    4,
    selectInput(
      "chemotherapyInput",
      h3("Chemotherapy"),
      choices = list("yes" = "yes",
                     "no" = "no"),
      selected = "yes"
    )
  ),
  # pam50
  column(
    4,
    selectInput(
      "pam50Input",
      h3("PAM50 Test"),
      choices = list(
        "Basal" = "Basal",
        "claudin-low" = "claudin-low",
        "Her2" = "Her2",
        "LumA" = "LumA",
        "LumB" = "LumB",
        "NC" = "NC",
        "Normal" = "Normal"
      ),
      selected = "Basal"
    )
  ),
  # ER Status
  column(
    4,
    selectInput(
      "erInput",
      h3("ER Status"),
      choices = list("Negative" = "Negative",
                     "Positive" = "Positive"),
      selected = "Negative"
    )
  ),
  # Neoplasm Histologic Grade
  column(
    4,
    selectInput(
      "neoplasmInput",
      h3("Neoplasm Histologic Grade"),
      choices = list("1" = 1,
                     "2" = 2,
                     "3" = 3),
      selected = 2
    )
  ),
  # HER2 Status
  column(
    4,
    selectInput(
      "her2Input",
      h3("HER2"),
      choices = list("Negative" = "Negative",
                     "Positive" = "Positive"),
      selected = "Negative"
    )
  ),
  # Histologic Subtype
  column(
    4,
    selectInput(
      "histologicsubtypeInput",
      h3("Histologic Subtype"),
      choices = list(
        "Ductal/NST" = "Ductal/NST",
        "Lobular" = "Lobular",
        "Medullary" = "Medullary",
        "Mixed" = "Mixed",
        "Mucinous" = "Mucinous",
        "Tubular/ cribriform" = "Tubular/ cribriform"
      ),
      selected = "Mixed"
    )
  ),
  # Hormone Therapy
  column(
    4,
    selectInput(
      "hormoneInput",
      h3("Hormone Therapy"),
      choices = list("no" = "no",
                     "yes" = "yes"),
      selected = "yes"
    )
  ),
  # Inferred Menopausal State
  column(
    4,
    selectInput(
      "menopausalstateInput",
      h3("Inferred Menopausal State"),
      choices = list("Pre" = "Pre",
                     "Post" = "Post"),
      selected = "Post"
    )
  ),
  # Primary Tumor Laterality
  column(
    4,
    selectInput(
      "lateralityInput",
      h3("Primary Tumor Laterality"),
      choices = list("Left" = "Left",
                     "Right" = "Right"),
      selected = "Left"
    )
  ),
  # Lymphnodes
  column(4,
         numericInput(
           "lymphnodesInput",
           h3("Lymphnodes"),
           value = 0
         )),
  # Mutation Count
  column(4,
         numericInput(
           "mutationcountInput",
           h3("Mutation Count"),
           value = 0
         )),
  # Nottingham Prognostic Index
  column(4,
         numericInput(
           "nottinghamInput",
           h3("Nottingham Index"),
           value = 0
         )),
  # PR Status
  column(
    4,
    selectInput(
      "prstatusInput",
      h3("PR Status"),
      choices = list("Positive" = "Positive",
                     "Negative" = "Negative"),
      selected = "Positive"
    )
  ),
  # Radio Therapy
  column(
    4,
    selectInput(
      "radiotherapyInput",
      h3("Radio Therapy"),
      choices = list("no" = "no",
                     "yes" = "yes"),
      selected = "yes"
    )
  ),
  # Tumor Size
  column(4,
         sliderInput(
           "tumorsizeInput",
           h3("Tumor Size"),
           min = 0,
           max = 200,
           value = 50
         )),
), 