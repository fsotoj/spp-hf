sppAboutModuleUI <- function(id,
                             title = "Subnational Politics Project",
                             max_width = 900,
                             open_sections = c("about")) {
  ns <- NS(id)
  root_id <- ns("root")
  
  # helper: TRUE if section should start open
  open_attr <- function(key, open_sections) if (key %in% open_sections) "open" else NULL
  
  # 1) Data list for the team members (Translated from React SppPeople.js)
  people_data <- list(
    list(name = "Agustina Giraudy", role = "Principal Investigator", linkedin = "https://www.linkedin.com/in/agustina-giraudy-72a3b81a9/", site = "https://agustinagiraudy.com/", org = "American University<br/>Tecnológico de Monterrey", img = "docs/agustina.webp", color = "#FFA92A"),
    list(name = "Francisco Urdinez", role = "Collaborator", linkedin = "https://www.linkedin.com/in/francisco-urdinez-a8061813/", site = "https://www.furdinez.com/", org = "Universidad Católica de Chile", img = "docs/francisco.webp", color = "#722464"),
    list(name = "Guadalupe González", role = "Collaborator", linkedin = "https://www.linkedin.com/in/guadag12/", site = "https://guadagonzalez.com/", org = "University of Maryland", img = "docs/guadalupe.webp", color = "#722464"),
    list(name = "Felipe Soto Jorquera", role = "Collaborator", linkedin = "https://www.linkedin.com/in/felipesotojorquera/", site = NULL, org = "Hertie School, Berlin", img = "docs/felipe.webp", color = "#722464"),
    list(name = "Magdalena Nieto", role = "Collaborator", linkedin = "https://www.linkedin.com/in/magdalenanieto/", site = NULL, org = "Universidad de Buenos Aires", img = "docs/magdalena.webp", color = "#722464"),
    list(name = "Sergio Huertas Hernández", role = "Research Assistant", linkedin = "https://www.linkedin.com/in/sergio-huertas-hern%C3%A1ndez/", site = "https://serhuertas.github.io/", org = "Universidad Católica de Chile", img = "docs/sergio.webp", color = "#444447")
  )

  style_css <- paste0(
    "/* Palette via CSS variables from styles.css (fallbacks included) */\n",
    "#", root_id, " { background:#fff; color:var(--gray, #4D4D4D); font-family: Helvetica, Arial, sans-serif; padding:24px 16px 24px; }\n",
    "#", root_id, " .spp-container { max-width:", max_width, "px; margin:0 auto; }\n",
    "#", root_id, " .kicker { color:var(--orange, #FFA92A); text-transform:uppercase; letter-spacing:0.12em; font-size:24px; margin:0 0 12px 0; }\n",
    
    "/* Collapsible card built on <details> */\n",
    "#", root_id, " details.card { background:#fff; border:1px solid #e6e6e6; border-radius:12px; margin-bottom:18px; overflow:visible; transition: transform .12s ease, box-shadow .12s ease, border-color .12s ease; }\n",
    "#", root_id, " details.card:hover { transform: translateY(-2px); box-shadow: 0 6px 14px rgba(0,0,0,0.18); border-color: rgba(255,169,42,0.35); }\n",
    "#", root_id, " details.card[open] { box-shadow:0 6px 14px rgba(0,0,0,0.12); border-color: rgba(255,169,42,0.45); }\n",
    "#", root_id, " summary { list-style:none; cursor:pointer; padding:14px 16px; display:flex; align-items:center; gap:10px; }\n",
    "#", root_id, " summary::-webkit-details-marker { display:none; }\n",
    "#", root_id, " .chev { margin-left:auto; transition: transform .15s ease; }\n",
    "#", root_id, " details[open] .chev { transform: rotate(180deg); }\n",
    "#", root_id, " .card-title { font-weight:800; font-size:18px; color:var(--purple, #722464); }\n",
    "#", root_id, " .card-body { padding: 0 16px 16px 16px; color:var(--gray, #4D4D4D); }\n",
    "#", root_id, " .spp-text { color:var(--gray, #4D4D4D); line-height:1.6; text-align:justify; }\n",
    "#", root_id, " .spp-hr { border:0; height:1px; background:linear-gradient(90deg, rgba(255,169,42,0), rgba(255,169,42,0.8), rgba(255,169,42,0)); margin:14px 0; }\n",
    "#", root_id, " .ref { color:var(--gray, #4D4D4D); font-size:12px; }\n",
    "#", root_id, " .spp-link { color:var(--magenta, #E5007D); text-decoration:none; }\n",
    "#", root_id, " .spp-link:hover { text-decoration:underline; }\n",
    
    "/* SPP People Grid */\n",
    "#", root_id, " .spp-people-grid { display: grid; grid-template-columns: repeat(5, 1fr); gap: 12px; justify-items: center; padding: 10px 0; width: 100%; }\n",
    "#", root_id, " .spp-property-card { height: 19em; width: 100%; max-width: 14em; display: flex; flex-direction: column; position: relative; transition: all 0.4s cubic-bezier(0.645, 0.045, 0.355, 1); border-radius: 16px; overflow: hidden; background: #fff; box-shadow: 15px 15px 27px #e1e1e3, -15px -15px 27px #ffffff; }\n",
    "#", root_id, " .spp-property-image { height: 10em; width: 100%; position: absolute; top: 0; transition: all 0.4s cubic-bezier(0.645, 0.045, 0.355, 1); background-size: cover; background-position: center top; background-repeat: no-repeat; z-index: 2; }\n",    "#", root_id, " .spp-image-overlay { position: absolute; inset: 0; background: linear-gradient(to top, rgba(0,0,0,0.85) 0%, rgba(0,0,0,0) 60%); opacity: 0; transition: opacity 0.4s ease; }\n",
    "#", root_id, " .spp-image-footer { position: absolute; bottom: 0.8em; left: 0.8em; right: 0.8em; display: flex; flex-direction: column; gap: 8px; z-index: 3; }\n",
    "#", root_id, " .spp-image-name { opacity: 0; transform: translateY(5px); transition: all 0.4s ease 0.1s; color: white; font-weight: 800; font-size: 0.95rem; line-height: 1.1; }\n",
    "#", root_id, " .spp-property-description { background-color: #FAFAFC; height: 9em; width: 100%; position: absolute; bottom: 0; transition: all 0.4s cubic-bezier(0.645, 0.045, 0.355, 1); padding: 1.2em 0.6em; text-align: center; display: flex; flex-direction: column; align-items: center; gap: 6px; z-index: 1; }\n",
    "#", root_id, " .spp-role-badge { font-size: 8px; text-transform: uppercase; font-weight: 800; color: white; padding: 3px 8px; border-radius: 4px; }\n",
    "#", root_id, " .spp-person-name { margin: 0; font-size: 0.9rem; font-weight: 700; color: #1a1a1b; }\n",
    "#", root_id, " .spp-person-org { font-size: 10px; color: #666; line-height: 1.3; }\n",
    "#", root_id, " .spp-property-social-icons { display: flex; flex-direction: row; gap: 6px; align-items: center; }\n",
    "#", root_id, " .spp-social-btn { width: 24px; height: 24px; background-color: #000; color: white; display: flex; align-items: center; justify-content: center; border-radius: 5px; text-decoration: none; font-size: 11px; transition: all 0.4s cubic-bezier(0.645, 0.045, 0.355, 1); overflow: hidden; white-space: nowrap; }\n",
    "#", root_id, " .spp-btn-text { max-width: 0; opacity: 0; margin-left: 0; font-weight: 700; font-size: 10px; transition: all 0.4s ease; }\n",
    
    "/* Hover Transitions */\n",
    "#", root_id, " .spp-property-card:hover .spp-property-description { height: 0em; opacity: 0; }\n",
    "#", root_id, " .spp-property-card:hover .spp-property-image { height: 19em; }\n",
    "#", root_id, " .spp-property-card:hover .spp-image-overlay { opacity: 1; }\n",
    "#", root_id, " .spp-property-card:hover .spp-image-name { opacity: 1; transform: translateY(0); }\n",
    "#", root_id, " .spp-property-card:hover .spp-social-btn { width: 78px; background-color: white; color: black; padding: 0 6px; }\n",
    "#", root_id, " .spp-property-card:hover .spp-btn-text { max-width: 50px; opacity: 1; margin-left: 5px; }\n",
    "#", root_id, " .spp-social-btn.li:hover { background-color: #0A66C2 !important; color: white !important; }\n",
    "#", root_id, " .spp-social-btn.web:hover { background-color: #FFA92A !important; color: white !important; }\n",
    
    "@media (max-width: 1150px) { #", root_id, " .spp-people-grid { grid-template-columns: repeat(3, 1fr); } }\n",
    "@media (max-width: 768px) { #", root_id, " .spp-people-grid { grid-template-columns: repeat(2, 1fr); } }\n",
    "@media (max-width: 500px) { #", root_id, " .spp-people-grid { grid-template-columns: 1fr; } }\n",

    "/* Figures */\n",
    "#", root_id, " .figure { margin:10px 0 12px 0; text-align:center; }\n",
    "#", root_id, " .figure img { max-width:100%; height:auto; border-radius:12px; border:1px solid #eee; }\n",
    "#", root_id, " .caption { font-size:0.92rem; color:var(--gray, #4D4D4D); margin-top:6px; }\n",
    "#", root_id, " ol.vars { margin:10px 0 0 18px; }\n",
    "#", root_id, " ol.vars li { margin:6px 0; }\n"
  )
  
  tagList(
    singleton(tags$style(HTML(style_css))),
    tags$div(
      id = root_id,
      tags$div(
        class = "spp-container",
        if (!is.null(title) && nzchar(title)) tags$p(class = "kicker", title),
        
        # 1) About SPP -----------------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("about", open_sections),
          tags$summary(
            tags$span(class = "card-title", "About SPP"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(
              class = "spp-text",
              tags$strong("The Subnational Politics Project (SPP)"),
              "is a collaborative initiative dedicated to compiling, generating, and disseminating systematic, transparent, and publicly accessible data on subnational political institutions, processes, and electoral outcomes across Latin America."
            ),
            tags$p(
              class = "spp-text",
              "The SPP’s central goal is to build a comprehensive and standardized data infrastructure that enables both detailed within-country analysis and robust cross-national comparisons of subnational political dynamics."
            ),
            tags$p(
              class = "spp-text",
              "By providing consistent, high-quality, and spatially disaggregated longitudinal data, the SPP seeks to advance scholarly and policy-oriented research on the political foundations and consequences of territorial inequality in Latin America."
            ),
            tags$p(
              class = "spp-text",
              "This data infrastructure will support empirical work on a wide range of topics, including federalism, decentralization, subnational democracy and authoritarianism, party competition, electoral accountability, territorial governance, among others."
            )
          )
        ),
        
        # 2) People Behind SPP (React Integration) -------------------------------
        tags$details(
          class = "card",
          open  = open_attr("people", open_sections),
          tags$summary(
            tags$span(class = "card-title", "People Behind SPP"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$div(
              class = "spp-people-grid",
              # Iterate over the people_data list
              lapply(people_data, function(p) {
                tags$div(
                  class = "spp-property-card",
                  tags$div(
                    class = "spp-property-image",
                    style = paste0("background-image: url('", p$img, "');"),
                    tags$div(class = "spp-image-overlay"),
                    tags$div(
                      class = "spp-image-footer",
                      tags$div(class = "spp-image-name", p$name),
                      tags$div(
                        class = "spp-property-social-icons",
                        if (!is.null(p$linkedin)) {
                          tags$a(href = p$linkedin, target = "_blank", rel = "noreferrer", class = "spp-social-btn li",
                                 tags$i(class = "fab fa-linkedin-in"),
                                 tags$span(class = "spp-btn-text", "LinkedIn"))
                        },
                        if (!is.null(p$site)) {
                          tags$a(href = p$site, target = "_blank", rel = "noreferrer", class = "spp-social-btn web",
                                 tags$i(class = "fas fa-globe"),
                                 tags$span(class = "spp-btn-text", "Website"))
                        }
                      )
                    )
                  ),
                  tags$div(
                    class = "spp-property-description",
                    tags$span(class = "spp-role-badge", style = paste0("background-color: ", p$color, ";"), p$role),
                    tags$h5(class = "spp-person-name", p$name),
                    tags$p(class = "spp-person-org", HTML(p$org))
                  )
                )
              })
            )
          )
        ),
        
        # 3) About Databases -----------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("databases", open_sections),
          tags$summary(
            tags$span(class = "card-title", "About SPP Databases"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(class = "spp-text", tags$strong("Databases’ Structure")),
            tags$p(
              class = "spp-text",
              "The Subnational Politics Project (SPP) is made up of different databases. Each database employs a country–state–year structure, with observations at the subnational unit level for each electoral year. Each observation represents a subnational unit (province/state) in a given year."
            ),
            tags$div(
              class = "figure",
              tags$img(src = "databases_spp.svg", alt = "Figure 1. SPP Databases")
            ),
            tags$hr(class = "spp-hr"),
            
            tags$p(class = "spp-text", tags$strong("Variable Information")),
            tags$p(
              class = "spp-text",
              "The databases in the Subnational Politics Project divide variables into the following types:"
            ),
            tags$ol(
              class = "vars",
              tags$li(tags$b("Identifier Variables:"), " Data identifying country names and codes, state names and codes, region names, and time periods."),
              tags$li(tags$b("Executive Branch Variables:"), " Data on national and subnational executive branches, such as length of term, incumbent party, cumulative years of president/governor in office, etc."),
              tags$li(tags$b("Electoral Variables:"), " Data on subnational executive and subnational legislative elections, including legislatures’ composition."),
              tags$li(tags$b("Indices:"), " Data generated by adding and combining variables, or creating cumulative scales.")
            ),
            tags$div(
              class = "figure",
              tags$img(src = "variables_database.svg", alt = "Figure 2. Variable Types in SPP")
            )
          )
        ),
        
        # 4) References ----------------------------------------------------------
        tags$details(
          class = "card",
          open  = open_attr("refs", open_sections),
          tags$summary(
            tags$span(class = "card-title", "References"),
            icon("chevron-down", class = "chev")
          ),
          tags$div(
            class = "card-body",
            tags$p(class="ref", tags$strong("Suggested Citation for Codebook")),
            tags$p(
              class="ref",
              "Giraudy, Agustina; Gonzalez, Guadalupe Andrea; Urdinez, Francisco, 2025, ",
              "“Codebook: Subnational Politics Project (SPP) (v. 1)”, ",
              tags$a(
                href="https://doi.org/10.17605/OSF.IO/H96FD",
                target="_blank",
                class="spp-link",
                "https://doi.org/10.17605/OSF.IO/H96FD"
              )
            ),
            tags$hr(class="spp-hr"),
            tags$table(
              style = "width:100%; border-collapse:collapse; font-size:12px;",
              tags$thead(
                tags$tr(
                  tags$th("Database Name", style="text-align:left; border-bottom:2px solid #ddd; padding:6px;"),
                  tags$th("Abbreviation", style="text-align:left; border-bottom:2px solid #ddd; padding:6px;"),
                  tags$th("Suggested Citation", style="text-align:left; border-bottom:2px solid #ddd; padding:6px;")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td("Subnational Executive Database", style="padding:6px;"),
                  tags$td("SED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hernández, and Francisco Urdinez. 2025. “Subnational Executive Database (SED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/1D3P3J", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/1D3P3J"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Subnational Executive Elections Database", style="padding:6px;"),
                  tags$td("SEED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hernández, and Francisco Urdinez. 2025. “Subnational Executive Elections Database (SEED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/UPOWMW", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/UPOWMW"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Subnational Legislative Elections Database", style="padding:6px;"),
                  tags$td("SLED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. “Subnational Legislative Elections Database (SLED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/084FXF", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/084FXF"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Subnational Democracy Indices", style="padding:6px;"),
                  tags$td("SDI", style="padding:6px;"),
                  tags$td("Giraudy, Agustina. 2025. “Subnational Democracy Indices (SDI) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/7TNLBW", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/7TNLBW"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("Capital Federal & Tierra del Fuego Legislatures Database", style="padding:6px;"),
                  tags$td("CFTDFLD", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, and Guadalupe Andrea Gonzalez. 2025. “Capital Federal & Tierra Del Fuego Legislatures Database (CFTDFLD) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/AJJLHX", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/AJJLHX"), style="padding:6px;")
                ),
                tags$tr(
                  tags$td("National Executive Database", style="padding:6px;"),
                  tags$td("NED", style="padding:6px;"),
                  tags$td("Giraudy, Agustina, Guadalupe Andrea Gonzalez, Sergio Huertas-Hernández, and Francisco Urdinez. 2025. “National Executive Database (NED) (v. 1).” Subnational Politics Project. ",
                          tags$a(href="https://doi.org/doi:10.7910/DVN/HNKQUH", target="_blank", class="spp-link", "https://doi.org/doi:10.7910/DVN/HNKQUH"), style="padding:6px;")
                )
              )
            )
          )
        )
      )
    )
  )
}

sppAboutModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}