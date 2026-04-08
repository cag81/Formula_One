# ===========================================================================
# F1 Net Zero 2030 — Combined Dashboard (Static + Time-Dynamic)
# ===========================================================================
# Two-tab Shiny app:
#   Tab 1: Static What-If Calculator (instant lever recalculation)
#   Tab 2: Time-Dynamic SD Simulation (Euler integration 2024-2030)
#
# ===========================================================================

library(shiny)

# ═══════════════════════════════════════════════════════════════════════════
# SHARED CONSTANTS (2018 baseline, F1 2025 Sustainability Update)
# ═══════════════════════════════════════════════════════════════════════════
BASELINE_TOTAL      <- 228793
BASELINE_LOGISTICS  <- 67994
BASELINE_BIZTRAVEL  <- 79425
VIP_SHARE           <- 0.15
STAFF_SHARE         <- 0.85
BASELINE_VIP        <- BASELINE_BIZTRAVEL * VIP_SHARE   # 11913.75
BASELINE_STAFF      <- BASELINE_BIZTRAVEL * STAFF_SHARE # 67511.25
BASELINE_EVENTOPS   <- 21210   # Report figure (22,813) minus PU allocation (1,602); rounded to hit 228,793 total
BASELINE_FACILITIES <- 58562
BASELINE_POWERUNIT  <- 1602    # 0.7% of 228,793 (F1 2019 Strategy, p.9)
BASELINE_CALENDAR   <- 21
SAF_REDUCTION       <- 0.80
RAILSEA_EF          <- 0.10

# ═══════════════════════════════════════════════════════════════════════════
# TIME-DYNAMIC CONSTANTS
# ═══════════════════════════════════════════════════════════════════════════
MAX_RAILSEA   <- 0.60
MAX_SAF       <- 1.00
MAX_RE        <- 1.00
MAX_REMOTE    <- 1.00
MAX_CALENDAR  <- 26
MAX_OFFSET    <- 1.00

INIT_RAILSEA  <- 0.15
INIT_SAF      <- 0.05
INIT_RE       <- 0.80
INIT_REMOTE   <- 0.20
INIT_OFFSET   <- 0.00

PRESSURE_SENSITIVITY   <- 1.0
PRESSURE_AUTONOMOUS    <- 0.30

T_START <- 2024.0
T_END   <- 2030.0
DT      <- 0.25

# ═══════════════════════════════════════════════════════════════════════════
# TIME-DYNAMIC SCENARIOS
# ═══════════════════════════════════════════════════════════════════════════
TD_SCENARIOS <- list(
  Y2018 = list(
    label = "2018 Baseline",
    desc = "2018 baseline trajectory (228,793 tCO\u2082e). Minimal intervention.",
    saf_speed = 1, intermodal_speed = 2, remote_speed = 2,
    re_speed = 2, offset_ramp = 0, calendar_size = 21, regionalisation = 0
  ),
  Y2023 = list(
    label = "2023 Level",
    desc = "2023 policy trajectory (182,801 tCO\u2082e). Early-stage progress.",
    saf_speed = 8, intermodal_speed = 10, remote_speed = 8,
    re_speed = 8, offset_ramp = 2, calendar_size = 22, regionalisation = 20
  ),
  Y2024 = list(
    label = "2024 Level",
    desc = "2024 current trajectory (168,720 tCO\u2082e). Accelerating action.",
    saf_speed = 18, intermodal_speed = 20, remote_speed = 12,
    re_speed = 15, offset_ramp = 6, calendar_size = 24, regionalisation = 22
  ),
  NetZero = list(
    label = "Net Zero",
    desc = "Maximum intervention on every lever. Only path that reaches net zero.",
    saf_speed = 50, intermodal_speed = 5, remote_speed = 20,
    re_speed = 30, offset_ramp = 17, offset_effectiveness = 100,
    calendar_size = 20, regionalisation = 100
  )
)

# ═══════════════════════════════════════════════════════════════════════════
# STATIC SCENARIOS
# ═══════════════════════════════════════════════════════════════════════════
# Report values from F1 2025 Sustainability Update, p.11 (With SAFc)
# Power Unit = 0.7% of year total (per F1 2019 Sustainability Strategy, p.9)
# Subtracted from Event Operations (2019 report defines Event Ops as excluding PU)
ST_SCENARIOS <- list(
  Y2018 = list(
    safRate = 0, railSeaShare = 0, remoteRate = 0,
    renewableShare = 0, offsetRate = 0, calendarSize = 21, regionalisation = 0,
    label = "2018 Baseline",
    desc = "2018 baseline: Pre-intervention reference",
    overrides = list(
      logistics  = 67994,
      biz_travel = 79425, vip = 11914, staff = 67511,
      event_ops  = 21210, power_unit = 1602,
      facilities = 58562
    )
  ),
  Y2023 = list(
    safRate = 2, railSeaShare = 5, remoteRate = 11,
    renewableShare = 36, offsetRate = 0, calendarSize = 22, regionalisation = 20,
    label = "2023 Level",
    desc = "2023 emissions: Early-stage progress",
    overrides = list(
      logistics  = 69713,
      biz_travel = 62912, vip = 9437, staff = 53475,
      event_ops  = 23347, power_unit = 1280,
      facilities = 25549
    )
  ),
  Y2024 = list(
    safRate = 5, railSeaShare = 5, remoteRate = 19,
    renewableShare = 48, offsetRate = 0, calendarSize = 24, regionalisation = 22,
    label = "2024 Level",
    desc = "2024 emissions: Accelerating action",
    overrides = list(
      logistics  = 61555,
      biz_travel = 59841, vip = 8976, staff = 50865,
      event_ops  = 21880, power_unit = 1181,
      facilities = 24263
    )
  ),
  NetZero = list(
    safRate = 100, railSeaShare = 20, remoteRate = 50,
    renewableShare = 100, offsetRate = 100, offsetEffectiveness = 100,
    calendarSize = 20, regionalisation = 100,
    label = "Net Zero",
    desc = "Road to net zero"
  )
)

# ═══════════════════════════════════════════════════════════════════════════
# SHARED FORMAT HELPERS
# ═══════════════════════════════════════════════════════════════════════════
fmt  <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt1 <- function(x) sprintf("%.1f", x)
fmt3 <- function(x) sprintf("%.3f", x)

# ═══════════════════════════════════════════════════════════════════════════
# SHARED THEME COLOURS
# ═══════════════════════════════════════════════════════════════════════════
TH_DARK <- list(
  bg = "#080c12", panel = "#0d1117", panelAlt = "#111820",
  border = "#aaaabb", borderLt = "#aaaabb",
  text = "#ffffff", textDim = "#ffffff", heading = "#ffffff",
  red = "#e8334a", redDim = "#5c1520",
  green = "#36dba0", greenDim = "#0e3d2b",
  blue = "#56b4e9", blueDim = "#152a4a",
  orange = "#e69f00", orangeDim = "#4a3018",
  yellow = "#f0e442", yellowDim = "#4a3c18",
  gray = "#8b95a5", grayDim = "#1e2430",
  purple = "#cc79a7", cyan = "#009e73"
)

TH_LIGHT <- list(
  bg = "#f0f0f0", panel = "#ffffff", panelAlt = "#f7f7f7",
  border = "#555555", borderLt = "#555555",
  text = "#000000", textDim = "#000000", heading = "#000000",
  red = "#c42836", redDim = "#fde8e8",
  green = "#1aab78", greenDim = "#e0f5ec",
  blue = "#2e8cca", blueDim = "#e0eeff",
  orange = "#d08d00", orangeDim = "#fff0d6",
  yellow = "#c4a800", yellowDim = "#fff8d6",
  gray = "#6e7888", grayDim = "#eaeaea",
  purple = "#b0668e", cyan = "#008060"
)

TH <- TH_DARK

# ═══════════════════════════════════════════════════════════════════════════
# SHARED DONUT SVG BUILDER
# ═══════════════════════════════════════════════════════════════════════════
build_donut_svg <- function(data, size = 200, th = TH_DARK) {
  total <- sum(data$value)
  if (total == 0) {
    return(HTML(sprintf(
      '<div style="width:%dpx;height:%dpx;display:flex;align-items:center;justify-content:center;">
         <span style="color:%s;font-size:16px;font-weight:700;">Zero Emissions</span></div>', size, size, th$green)))
  }
  cx <- size / 2; cy <- size / 2; r <- size * 0.38; sw <- size * 0.14
  cum_angle <- -90
  arcs <- ""
  for (i in seq_len(nrow(data))) {
    if (data$value[i] <= 0) next
    angle <- (data$value[i] / total) * 360
    sa <- cum_angle; cum_angle <- cum_angle + angle; ea <- cum_angle
    la <- if (angle > 180) 1 else 0
    rad <- function(a) a * pi / 180
    x1 <- cx + r * cos(rad(sa)); y1 <- cy + r * sin(rad(sa))
    x2 <- cx + r * cos(rad(ea)); y2 <- cy + r * sin(rad(ea))
    arcs <- paste0(arcs, sprintf(
      '<path d="M %.1f %.1f A %.1f %.1f 0 %d 1 %.1f %.1f"
             fill="none" stroke="%s" stroke-width="%.0f" stroke-linecap="butt" opacity="0.9">
        <title>%s: %s tCO2e (%.1f%%)</title></path>',
      x1, y1, r, r, la, x2, y2, data$color[i], sw,
      data$label[i], fmt(data$value[i]), (data$value[i] / total) * 100))
  }
  HTML(sprintf(
    '<svg width="%d" height="%d" viewBox="0 0 %d %d" style="display:block;margin:auto;">%s
     <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="10">GROSS</text>
     <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="18" font-weight="700" font-family="monospace">%s</text>
     <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="9">tCO&#x2082;e</text></svg>',
    size, size, size, size, arcs, cx, cy-8, th$textDim, cx, cy+14, th$heading, fmt(total), cx, cy+30, th$textDim))
}

# ═══════════════════════════════════════════════════════════════════════════
# STATIC MODEL — calculate_emissions
# ═══════════════════════════════════════════════════════════════════════════
calculate_emissions <- function(saf_rate, railsea_share, remote_rate,
                                renewable_share, offset_rate,
                                calendar_size, regionalisation,
                                offset_effectiveness = 70) {
  # Convert percentages to fractions
  SAF <- saf_rate / 100
  RS  <- railsea_share / 100
  RW  <- remote_rate / 100
  RE  <- renewable_share / 100
  CO  <- offset_rate / 100
  eff <- offset_effectiveness / 100
  CR  <- (regionalisation / 100) * 0.62   # max 62% distance reduction (Struijk 2024)

  # 4.1 Calendar Effects
  CM  <- calendar_size / BASELINE_CALENDAR   # Calendar_Multiplier
  DM  <- 1 - CR                             # Distance_Multiplier
  ECF <- CM * DM                            # Effective_Calendar_Factor

  # 4.2 Logistics Emissions
  AFS <- 1 - RS                              # Air_Freight_Share
  AFef <- 1 - (SAF * SAF_REDUCTION)          # Air_Freight_EF
  Lef <- (AFS * AFef) + (RS * RAILSEA_EF)   # Logistics_EF
  logistics <- BASELINE_LOGISTICS * ECF * Lef # LE

  # 4.3 Business Travel Emissions
  VTD <- BASELINE_BIZTRAVEL * VIP_SHARE * ECF             # VIP_Travel_Demand
  STD <- BASELINE_BIZTRAVEL * STAFF_SHARE * ECF * (1 - RW) # Staff_Travel_Demand
  Aef <- 1 - (SAF * SAF_REDUCTION)                        # Aviation_EF
  vip   <- VTD * Aef                                       # VIP_Emissions
  staff <- STD * Aef                                       # Staff_Emissions
  biz_travel <- vip + staff                                # Business_Travel

  # 4.4 Facilities Emissions
  Gef <- 1 - RE                              # Grid_EF
  facilities <- BASELINE_FACILITIES * Gef    # FE (no calendar multiplier)

  # 4.5 Event Operations Emissions
  event_ops <- BASELINE_EVENTOPS * CM * Gef  # OE (calendar only, no distance)

  # 4.6 Power Unit Emissions (0 after 2025 — FIA e-fuel mandate)
  power_unit <- 0

  # 5. Final Output Calculations
  gross <- logistics + biz_travel + event_ops + facilities + power_unit  # GE
  OA    <- gross * CO * eff                                              # Offset_Abatement (effectiveness-adjusted)
  net   <- gross - OA                                                    # NE
  net_best  <- gross - (gross * CO * 1.00)                               # Best-case (100% effectiveness)
  net_worst <- gross - (gross * CO * 0.20)                               # Worst-case (20% effectiveness)
  reduction_pct <- ((BASELINE_TOTAL - net) / BASELINE_TOTAL) * 100      # RFB

  list(
    logistics = logistics, vip = vip, staff = staff, biz_travel = biz_travel,
    facilities = facilities, event_ops = event_ops, power_unit = power_unit,
    gross = gross, net = net, net_best = net_best, net_worst = net_worst,
    reduction_pct = reduction_pct,
    calendar_mult = CM, distance_mult = DM, ecf = ECF,
    aviation_ef = Aef, logistics_ef = Lef, grid_ef = Gef
  )
}

# ═══════════════════════════════════════════════════════════════════════════
# STATIC CLD SVG BUILDER
# ═══════════════════════════════════════════════════════════════════════════
build_cld_svg <- function(inputs, R, th = TH_DARK) {
  W <- 720; H <- 520
  is_nz <- R$net < 1
  total_col     <- if (is_nz) th$green else th$red
  total_col_dim <- if (is_nz) th$greenDim else th$redDim
  flow_w <- function(val) max(0.8, min((val / BASELINE_TOTAL) * 18, 5))

  logP <- c(160, 80);   bizP <- c(160, 250)
  vipP <- c(60, 170);   stfP <- c(260, 170)
  evtP <- c(360, 410);  facP <- c(180, 410)
  puP  <- c(540, 410);  totP <- c(440, 250)

  node_svg <- function(x, y, label, sublabel, value, baseline, col, col_dim, w = 140, h = 52) {
    ratio <- if (baseline > 0) min(value / baseline, 1.5) else 0
    opa   <- 0.5 + ratio * 0.5
    sprintf(
      '<g opacity="%.2f">
        <rect x="%.0f" y="%.0f" width="%.0f" height="%.0f" rx="6"
              fill="%s" stroke="%s" stroke-width="1.5"/>
        <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
              font-size="11" font-weight="600">%s</text>
        <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
              font-size="8.5">%s</text>
        <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
              font-size="11" font-weight="700" font-family="monospace">%s t</text>
      </g>',
      opa, x - w/2, y - h/2, w, h, col_dim, col,
      x, y - 8, col, label,
      x, y + 5, th$textDim, sublabel,
      x, y + 20, th$heading, fmt(value))
  }

  arrow_svg <- function(x1, y1, x2, y2, col, thickness, id) {
    tw <- max(0.8, min(thickness, 5))
    sprintf(
      '<defs><marker id="ah-%s" markerWidth="8" markerHeight="6" refX="7" refY="3" orient="auto">
        <polygon points="0 0, 8 3, 0 6" fill="%s" opacity="0.7"/>
      </marker></defs>
      <line x1="%.0f" y1="%.0f" x2="%.0f" y2="%.0f"
            stroke="%s" stroke-width="%.1f" opacity="0.45"
            stroke-dasharray="6 4" marker-end="url(#ah-%s)"/>',
      id, col, x1, y1, x2, y2, col, tw, id)
  }

  loop_badge <- function(x, y, label, type) {
    col  <- if (type == "R") th$red else th$green
    bg   <- if (type == "R") th$redDim else th$greenDim
    sym  <- if (type == "R") "&#x21BB;" else "&#x21BA;"
    sprintf(
      '<circle cx="%.0f" cy="%.0f" r="16" fill="%s" stroke="%s" stroke-width="1"/>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="10" font-weight="700">%s</text>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="9">%s</text>',
      x, y, bg, col, x, y - 2, col, label, x, y + 9, col, sym)
  }

  policy_ind <- function(x, y, label, value, unit, col) {
    sprintf(
      '<rect x="%.0f" y="%.0f" width="104" height="28" rx="4"
             fill="none" stroke="%s" stroke-width="1" stroke-dasharray="4 3" opacity="0.7"/>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="8.5" opacity="0.8">%s</text>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="10"
             font-weight="700" font-family="monospace">%s%s</text>',
      x - 52, y - 14, col, x, y - 1, col, label, x, y + 10, col, value, unit)
  }

  svg_parts <- c(
    sprintf('<svg width="100%%" height="100%%" viewBox="0 0 %d %d"
             style="font-family: sans-serif; max-height: 100%%;">', W, H),
    sprintf('<defs><pattern id="grid" width="40" height="40" patternUnits="userSpaceOnUse">
       <path d="M 40 0 L 0 0 0 40" fill="none" stroke="%s" stroke-width="0.3"/>
     </pattern></defs>', th$border),
    sprintf('<rect width="%d" height="%d" fill="url(#grid)" opacity="0.4"/>', W, H),
    sprintf('<text x="%.0f" y="24" text-anchor="middle" fill="%s"
              font-size="11" letter-spacing="2">FORMULA ONE &mdash; ROAD TO NET ZERO</text>', W/2, th$textDim),
    arrow_svg(logP[1]+70, logP[2]+20, totP[1]-75, totP[2]-20, th$red, flow_w(R$logistics), "log"),
    arrow_svg(bizP[1]+70, bizP[2],    totP[1]-75, totP[2],    th$orange, flow_w(R$biz_travel), "biz"),
    arrow_svg(evtP[1]+50, evtP[2]-20, totP[1]+10, totP[2]+30, th$yellow, flow_w(R$event_ops), "evt"),
    arrow_svg(facP[1]+50, facP[2]-20, totP[1]-30, totP[2]+30, th$blue, flow_w(R$facilities), "fac"),
    arrow_svg(puP[1]-30,  puP[2]-20,  totP[1]+50, totP[2]+30, th$gray, flow_w(R$power_unit), "pu"),
    arrow_svg(vipP[1]+40, vipP[2]+22, bizP[1]-30, bizP[2]-16, th$orange, flow_w(R$vip), "vip"),
    arrow_svg(stfP[1]-40, stfP[2]+22, bizP[1]+30, bizP[2]-16, th$orange, flow_w(R$staff), "stf"),
    sprintf('<line x1="%.0f" y1="%.0f" x2="%.0f" y2="%.0f"
              stroke="%s" stroke-width="1" stroke-dasharray="3 3" opacity="0.5"/>',
            totP[1]+80, totP[2]-30, totP[1]+80, totP[2]+30, th$orange),
    sprintf('<text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
              font-size="8" opacity="0.7">OFFSETS</text>', totP[1]+80, totP[2]-38, th$orange),
    sprintf('<text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
              font-size="10" font-weight="700" font-family="monospace">&#x2212;%d%%</text>',
            totP[1]+80, totP[2]+45, th$orange, inputs$offsetRate),
    sprintf('<text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
              font-size="7" opacity="0.7">%d%% eff.</text>',
            totP[1]+80, totP[2]+57, th$cyan, inputs$offsetEffectiveness),
    node_svg(logP[1], logP[2], "Logistics", "~30% baseline",
             R$logistics, BASELINE_LOGISTICS, th$red, th$redDim),
    node_svg(vipP[1], vipP[2], "VIP Travel", "drivers / TPs (15%)",
             R$vip, BASELINE_VIP, th$orange, th$orangeDim, w = 110, h = 48),
    node_svg(stfP[1], stfP[2], "Staff Travel", "eng / mech / media (85%)",
             R$staff, BASELINE_STAFF, th$orange, th$orangeDim, w = 120, h = 48),
    node_svg(bizP[1], bizP[2], "Business Travel", "~35% / VIP + Staff",
             R$biz_travel, BASELINE_BIZTRAVEL, th$orange, th$orangeDim, w = 155),
    node_svg(evtP[1], evtP[2], "Event Ops", "~10% baseline",
             R$event_ops, BASELINE_EVENTOPS, th$yellow, th$yellowDim, w = 120),
    node_svg(facP[1], facP[2], "Facilities", "~26% baseline",
             R$facilities, BASELINE_FACILITIES, th$blue, th$blueDim, w = 120),
    node_svg(puP[1], puP[2], "Power Unit", "e-fuel mandate 2026",
             R$power_unit, BASELINE_POWERUNIT, th$gray, th$grayDim, w = 130),
    sprintf(
      '<rect x="%.0f" y="%.0f" width="180" height="76" rx="8"
             fill="%s" stroke="%s" stroke-width="2"/>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="11" font-weight="600">Total Carbon Emissions</text>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="20" font-weight="700" font-family="monospace">%s</text>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="9">tCO&#x2082;e net</text>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s" font-size="10" font-weight="600">%s%% reduction</text>',
      totP[1]-90, totP[2]-38, total_col_dim, total_col,
      totP[1], totP[2]-18, total_col,
      totP[1], totP[2]+5, th$heading, fmt(round(R$net)),
      totP[1], totP[2]+20, th$textDim,
      totP[1], totP[2]+32, total_col, fmt1(R$reduction_pct)),
    loop_badge(280, 50, "R1", "R"),
    loop_badge(590, 130, "B1", "B"),
    loop_badge(590, 190, "B2", "B"),
    loop_badge(590, 250, "B3", "B"),
    sprintf('<text x="620" y="133" text-anchor="start" fill="%s" font-size="7.5" opacity="0.7">INTERMODAL</text>', th$green),
    sprintf('<text x="620" y="193" text-anchor="start" fill="%s" font-size="7.5" opacity="0.7">CLEAN ENERGY</text>', th$green),
    sprintf('<text x="620" y="253" text-anchor="start" fill="%s" font-size="7.5" opacity="0.7">REMOTE OPS</text>', th$green),
    sprintf('<text x="300" y="46"  text-anchor="start" fill="%s" font-size="7.5" opacity="0.7">GROWTH</text>', th$red),
    policy_ind(60,  65,  "SAF",       inputs$safRate, "%", th$blue),
    policy_ind(360, 80,  "Rail/Sea",  inputs$railSeaShare, "%", th$cyan),
    policy_ind(360, 170, "Remote",    inputs$remoteRate, "%", th$purple),
    policy_ind(60,  440, "Renewable", inputs$renewableShare, "%", th$green),
    policy_ind(540, 340, "Calendar",  inputs$calendarSize, "", th$purple),
    policy_ind(360, 340, "Region",    inputs$regionalisation, "%", th$cyan),
    sprintf('<line x1="%.0f" y1="%.0f" x2="590" y2="190"
              stroke="%s" stroke-width="0.8" stroke-dasharray="4 4" opacity="0.3"/>',
            totP[1]+60, totP[2]-30, th$green)
  )
  if (is_nz) {
    svg_parts <- c(svg_parts, sprintf(
      '<rect x="%.0f" y="%.0f" width="200" height="30" rx="15"
             fill="%s" stroke="%s" stroke-width="1.5"/>
       <text x="%.0f" y="%.0f" text-anchor="middle" fill="%s"
             font-size="13" font-weight="700" letter-spacing="1">NET ZERO ACHIEVED</text>',
      W/2 - 100, H - 40, th$greenDim, th$green, W/2, H - 21, th$green))
  }
  svg_parts <- c(svg_parts, '</svg>')
  HTML(paste(svg_parts, collapse = "\n"))
}

# ═══════════════════════════════════════════════════════════════════════════
# TIME-DYNAMIC SD MODEL
# ═══════════════════════════════════════════════════════════════════════════
emissions_pressure <- function(gross_em) {
  raw <- gross_em / BASELINE_TOTAL
  pressure <- raw ^ PRESSURE_SENSITIVITY
  PRESSURE_AUTONOMOUS + (1 - PRESSURE_AUTONOMOUS) * pressure
}

# LE = BL × ECF × Lef
calc_logistics <- function(state, t, regionalisation) {
  CM  <- state[4] / BASELINE_CALENDAR
  DM  <- 1 - regionalisation
  ECF <- CM * DM
  AFS <- 1 - state[1]
  AFef <- 1 - state[2] * SAF_REDUCTION
  Lef <- AFS * AFef + state[1] * RAILSEA_EF
  BASELINE_LOGISTICS * ECF * Lef
}

# VE = VTD × Aef, where VTD = BB × VS × ECF
calc_vip <- function(state, t, regionalisation) {
  CM  <- state[4] / BASELINE_CALENDAR
  DM  <- 1 - regionalisation
  ECF <- CM * DM
  Aef <- 1 - state[2] * SAF_REDUCTION
  BASELINE_BIZTRAVEL * VIP_SHARE * ECF * Aef
}

# SE = STD × Aef, where STD = BB × SS × ECF × (1 - RW)
calc_staff <- function(state, t, regionalisation) {
  CM  <- state[4] / BASELINE_CALENDAR
  DM  <- 1 - regionalisation
  ECF <- CM * DM
  Aef <- 1 - state[2] * SAF_REDUCTION
  BASELINE_BIZTRAVEL * STAFF_SHARE * ECF * (1 - state[6]) * Aef
}

# OE = BO × CM × Gef (calendar only, no distance)
calc_eventops <- function(state, t) {
  CM  <- state[4] / BASELINE_CALENDAR
  Gef <- 1 - state[3]
  BASELINE_EVENTOPS * CM * Gef
}

# FE = BF × Gef (no calendar multiplier)
calc_facilities <- function(state) {
  Gef <- 1 - state[3]
  BASELINE_FACILITIES * Gef
}

# PE = IF year >= 2026 THEN 0 ELSE PU × CM
calc_powerunit <- function(state, t) {
  if (t >= 2026) return(0)
  CM <- state[4] / BASELINE_CALENDAR
  BASELINE_POWERUNIT * CM
}

# GE = LE + BE + OE + FE + PE
calc_gross <- function(state, t, regionalisation) {
  calc_logistics(state, t, regionalisation) +
    calc_vip(state, t, regionalisation) +
    calc_staff(state, t, regionalisation) +
    calc_eventops(state, t) +
    calc_facilities(state) +
    calc_powerunit(state, t)
}

# State: [1]=RailSea, [2]=SAF, [3]=RE, [4]=Calendar, [5]=Offset, [6]=Remote
derivatives <- function(state, t, params, pressure_mult) {
  d <- numeric(6)
  d[1] <- params$intermodal_rate * pressure_mult * max(MAX_RAILSEA - state[1], 0)
  d[2] <- params$saf_rate        * pressure_mult * max(MAX_SAF - state[2], 0)
  d[3] <- params$re_rate         * pressure_mult * max(MAX_RE - state[3], 0)
  d[4] <- 0
  d[5] <- if (state[5] < MAX_OFFSET) params$offset_ramp else 0
  d[6] <- params$remote_rate     * pressure_mult * max(MAX_REMOTE - state[6], 0)
  d
}

run_simulation <- function(saf_speed, intermodal_speed, remote_speed,
                           re_speed, offset_ramp, calendar_size, regionalisation,
                           offset_effectiveness = 70) {
  params <- list(
    saf_rate       = saf_speed / 100,
    intermodal_rate = intermodal_speed / 100,
    remote_rate    = remote_speed / 100,
    re_rate        = re_speed / 100,
    offset_ramp    = offset_ramp / 100,
    regionalisation = (regionalisation / 100) * 0.62   # max 62% distance reduction (Struijk 2024)
  )
  eff <- offset_effectiveness / 100

  state <- c(INIT_RAILSEA, INIT_SAF, INIT_RE, calendar_size, INIT_OFFSET, INIT_REMOTE)
  times <- seq(T_START, T_END, by = DT)
  n <- length(times)

  res <- data.frame(
    time = times, RailSea = numeric(n), SAF = numeric(n), RE = numeric(n),
    Calendar = numeric(n), Offset = numeric(n), Remote = numeric(n),
    Logistics = numeric(n), VIP = numeric(n), Staff = numeric(n),
    BizTravel = numeric(n), EventOps = numeric(n), Facilities = numeric(n),
    PowerUnit = numeric(n), Gross = numeric(n), Net = numeric(n),
    Net_best = numeric(n), Net_worst = numeric(n),
    Reduction = numeric(n), Pressure = numeric(n)
  )

  reg <- params$regionalisation

  for (i in seq_len(n)) {
    t <- times[i]
    res$RailSea[i]  <- state[1]; res$SAF[i] <- state[2]; res$RE[i] <- state[3]
    res$Calendar[i] <- state[4]; res$Offset[i] <- state[5]; res$Remote[i] <- state[6]

    log_e <- calc_logistics(state, t, reg); vip_e <- calc_vip(state, t, reg)
    stf_e <- calc_staff(state, t, reg); biz_e <- vip_e + stf_e
    evt_e <- calc_eventops(state, t); fac_e <- calc_facilities(state)
    pu_e  <- calc_powerunit(state, t)
    gross <- log_e + biz_e + evt_e + fac_e + pu_e
    net   <- max(gross * (1 - state[5] * eff), 0)
    net_best  <- max(gross * (1 - state[5] * 1.00), 0)
    net_worst <- max(gross * (1 - state[5] * 0.20), 0)
    p_mult <- emissions_pressure(gross)
    red   <- (BASELINE_TOTAL - net) / BASELINE_TOTAL * 100

    res$Logistics[i] <- log_e; res$VIP[i] <- vip_e; res$Staff[i] <- stf_e
    res$BizTravel[i] <- biz_e; res$EventOps[i] <- evt_e; res$Facilities[i] <- fac_e
    res$PowerUnit[i] <- pu_e; res$Gross[i] <- gross; res$Net[i] <- net
    res$Net_best[i] <- net_best; res$Net_worst[i] <- net_worst
    res$Reduction[i] <- red; res$Pressure[i] <- p_mult

    d <- derivatives(state, t, params, p_mult)
    state <- state + d * DT
    state[1] <- min(max(state[1], 0), MAX_RAILSEA)
    state[2] <- min(max(state[2], 0), MAX_SAF)
    state[3] <- min(max(state[3], 0), MAX_RE)
    state[4] <- min(max(state[4], 0), MAX_CALENDAR)
    state[5] <- min(max(state[5], 0), MAX_OFFSET)
    state[6] <- min(max(state[6], 0), MAX_REMOTE)
  }
  res
}

# ═══════════════════════════════════════════════════════════════════════════
# UI
# ═══════════════════════════════════════════════════════════════════════════
ui <- fluidPage(
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css2?family=DM+Sans:wght@400;500;600;700&family=DM+Mono:wght@400;500&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      :root {
        --bg: #080c12; --panel: #0d1117; --panel-alt: #111820;
        --border: #1c2433; --border-lt: #253040;
        --text: #c9d1d9; --text-dim: #6b7b8d; --heading: #ffffff;
        --red: #e8334a; --red-dim: #5c1520;
        --green: #36dba0; --green-dim: #0e3d2b;
        --blue: #56b4e9; --blue-dim: #152a4a;
        --orange: #e69f00; --orange-dim: #4a3018;
        --yellow: #f0e442; --yellow-dim: #4a3c18;
        --gray: #8b95a5; --gray-dim: #1e2430;
        --purple: #cc79a7; --cyan: #009e73;
        --slider-track: #1e2430; --irs-minmax: #3d4b5e;
        --handle-shadow: rgba(74,158,255,0.4);
        --bar-track-bg: #1e2430;
        --hero-red-from: #5c1520; --hero-red-to: #1a0a0e;
        --hero-green-from: #0e3d2b; --hero-green-to: #0a2a1e;
        --desc-active: #8b9bb0;
      }
      body.light-theme {
        --bg: #f0f0f0; --panel: #ffffff; --panel-alt: #f7f7f7;
        --border: #d8d8d8; --border-lt: #c8c8c8;
        --text: #15151e; --text-dim: #6e7681; --heading: #15151e;
        --red: #c42836; --red-dim: #fde8e8;
        --green: #1aab78; --green-dim: #e0f5ec;
        --blue: #2e8cca; --blue-dim: #e0eeff;
        --orange: #d08d00; --orange-dim: #fff0d6;
        --yellow: #c4a800; --yellow-dim: #fff8d6;
        --gray: #6e7888; --gray-dim: #eaeaea;
        --purple: #b0668e; --cyan: #008060;
        --slider-track: #d8d8d8; --irs-minmax: #999999;
        --handle-shadow: rgba(0,102,204,0.3);
        --bar-track-bg: #e8e8e8;
        --hero-red-from: #fde8e8; --hero-red-to: #fff5f5;
        --hero-green-from: #e0f5ec; --hero-green-to: #f0faf5;
        --desc-active: #555555;
      }

      *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
      html, body { height: 100%; overflow: hidden; }
      body { background: var(--bg); color: var(--text); font-family: 'DM Sans', -apple-system, sans-serif; transition: background 0.3s, color 0.3s; }
      .container-fluid { padding: 0 !important; height: 100vh; display: flex; flex-direction: column; }
      .row { margin: 0 !important; }
      .col-sm-3, .col-sm-6 { padding: 0 !important; }
      .form-group { margin-bottom: 0 !important; }

      .irs { height: 34px !important; }
      .irs-bar, .irs-bar-edge { background: var(--blue) !important; border: none !important; height: 5px !important; top: 28px !important; }
      .irs-line { background: var(--slider-track) !important; border: none !important; height: 5px !important; top: 28px !important; border-radius: 3px !important; }
      .irs-single { background: transparent !important; color: var(--blue) !important; font-family: 'DM Mono', monospace !important; font-size: 13px !important; font-weight: 700 !important; top: -2px !important; padding: 0 !important; }
      .irs-min, .irs-max { background: transparent !important; color: var(--irs-minmax) !important; font-size: 10px !important; visibility: visible !important; }
      .irs-handle { width: 16px !important; height: 16px !important; top: 21px !important; border-radius: 50% !important; background: #fff !important; border: 2px solid var(--blue) !important; box-shadow: 0 0 6px var(--handle-shadow) !important; cursor: pointer !important; }
      .irs-handle i { display: none !important; }
      .irs-grid { display: none !important; }
      .slider-green .irs-bar { background: var(--green) !important; } .slider-green .irs-handle { border-color: var(--green) !important; } .slider-green .irs-single { color: var(--green) !important; }
      .slider-orange .irs-bar { background: var(--orange) !important; } .slider-orange .irs-handle { border-color: var(--orange) !important; } .slider-orange .irs-single { color: var(--orange) !important; }
      .slider-purple .irs-bar { background: var(--purple) !important; } .slider-purple .irs-handle { border-color: var(--purple) !important; } .slider-purple .irs-single { color: var(--purple) !important; }
      .slider-cyan .irs-bar { background: var(--cyan) !important; } .slider-cyan .irs-handle { border-color: var(--cyan) !important; } .slider-cyan .irs-single { color: var(--cyan) !important; }
      .slider-row { margin-bottom: 10px; }
      .slider-label { font-size: 12px; color: var(--text); letter-spacing: 0.3px; margin-bottom: 0; }

      .dash-header { background: var(--panel); border-bottom: 1px solid var(--border); padding: 12px 24px; display: flex; align-items: center; justify-content: space-between; flex-shrink: 0; transition: background 0.3s; }
      .f1-logo { margin-right: 14px; flex-shrink: 0; }
      .dash-title { font-size: 16px; font-weight: 700; color: var(--heading); letter-spacing: 0.5px; }
      .dash-subtitle { font-size: 10px; color: var(--text-dim); margin-top: 2px; }
      .scope-badge { font-size: 10px; color: var(--text-dim); padding: 4px 10px; border: 1px solid var(--border); border-radius: 4px; margin-left: 8px; }

      .tab-toggle { display: flex; gap: 4px; margin-left: 18px; }
      .tab-btn { background: var(--panel-alt); border: 1px solid var(--border); border-radius: 6px; padding: 6px 14px; cursor: pointer; color: var(--text-dim); font-size: 11px; font-weight: 600; font-family: 'DM Sans', sans-serif; transition: all 0.2s; letter-spacing: 0.3px; }
      .tab-btn:hover { border-color: var(--blue); color: var(--text); }
      .tab-btn.active { background: var(--blue-dim); border-color: var(--blue); color: var(--heading); }

      .panel-left { background: var(--panel); border-right: 1px solid var(--border); overflow-y: auto; padding: 16px 18px; height: 100%; transition: background 0.3s; }
      .panel-center { background: var(--bg); height: 100%; display: flex; flex-direction: column; overflow: hidden; transition: background 0.3s; }
      .panel-right { background: var(--panel); border-left: 1px solid var(--border); overflow-y: auto; padding: 16px 18px; height: 100%; transition: background 0.3s; }

      .section-header { font-size: 13px; font-weight: 600; text-transform: uppercase; letter-spacing: 1.5px; margin-bottom: 14px; display: flex; align-items: center; gap: 8px; }
      .section-header .bar { width: 4px; height: 16px; border-radius: 2px; }

      .scenario-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 6px; margin-bottom: 6px; }
      .scenario-btn { background: var(--panel-alt); border: 1px solid var(--border); border-radius: 6px; padding: 7px 8px; cursor: pointer; color: var(--text); font-size: 11px; font-family: 'DM Sans', sans-serif; transition: all 0.2s; text-align: left; line-height: 1.3; }
      .scenario-btn:hover { border-color: var(--blue); color: var(--heading); }
      .scenario-btn.active { background: var(--blue-dim); border-color: var(--blue); color: var(--heading); font-weight: 600; }
      .scenario-btn.active-nz { background: var(--green-dim); border-color: var(--green); color: var(--heading); font-weight: 600; }
      .scenario-btn .btn-label { font-weight: 600; display: block; }
      .scenario-btn .btn-desc { font-size: 9px; color: var(--text-dim); margin-top: 2px; display: block; line-height: 1.3; }
      .scenario-btn.active .btn-desc, .scenario-btn.active-nz .btn-desc { color: var(--desc-active); }
      .reset-btn { width: 100%; padding: 8px 0; margin-top: 8px; background: transparent; border: 1px solid var(--border); border-radius: 6px; color: var(--text-dim); font-size: 11px; cursor: pointer; font-family: 'DM Sans', sans-serif; }
      .reset-btn:hover { border-color: var(--blue); color: var(--text); }

      .info-card { background: var(--panel-alt); border: 1px solid var(--border); border-radius: 8px; padding: 12px 14px; margin-bottom: 12px; }
      .info-card-title { font-size: 12px; color: var(--text); text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; font-weight: 600; }
      .factor-row { display: flex; justify-content: space-between; margin-bottom: 4px; }
      .factor-label { font-size: 12px; color: var(--text-dim); }
      .factor-value { font-size: 13px; color: var(--text); font-family: 'DM Mono', monospace; }

      .net-hero { border-radius: 12px; padding: 20px 16px; text-align: center; margin-bottom: 16px; transition: all 0.4s ease; }
      .net-hero.red { background: linear-gradient(135deg, var(--hero-red-from), var(--hero-red-to)); border: 2px solid var(--red); }
      .net-hero.green { background: linear-gradient(135deg, var(--hero-green-from), var(--hero-green-to)); border: 2px solid var(--green); animation: glow 2s infinite; }
      @keyframes glow { 0%,100% { box-shadow: 0 0 20px rgba(46,201,144,0.2); } 50% { box-shadow: 0 0 40px rgba(46,201,144,0.5); } }
      @keyframes pulse { 0%,100% { opacity:0.7; } 50% { opacity:1; } }
      .net-value { font-size: 34px; font-weight: 700; font-family: 'DM Mono', monospace; line-height: 1.1; }
      .nz-badge { margin-top: 12px; padding: 6px 16px; display: inline-block; background: var(--green); color: #000; font-weight: 700; font-size: 13px; border-radius: 20px; letter-spacing: 1px; animation: pulse 2s infinite; }

      .metric-row { display: flex; gap: 8px; margin-bottom: 12px; }
      .metric-card { background: var(--panel-alt); border: 1px solid var(--border); border-radius: 8px; padding: 8px 10px; text-align: center; flex: 1; }
      .metric-label { font-size: 12px; color: var(--text-dim); letter-spacing: 0.5px; text-transform: uppercase; margin-bottom: 4px; }
      .metric-value { font-size: 14px; font-weight: 700; font-family: 'DM Mono', monospace; }
      .metric-unit { font-size: 9px; color: var(--text-dim); margin-top: 2px; }

      .legend-row { display: flex; align-items: center; gap: 8px; margin-bottom: 5px; }
      .legend-dot { width: 12px; height: 12px; border-radius: 2px; flex-shrink: 0; }
      .legend-label { font-size: 13px; color: var(--text); flex: 1; }
      .legend-val { font-size: 13px; color: var(--heading); font-family: 'DM Mono', monospace; min-width: 50px; text-align: right; }
      .legend-pct { font-size: 12px; color: var(--text-dim); min-width: 32px; text-align: right; }

      .bar-label { display: flex; justify-content: space-between; margin-bottom: 3px; }
      .bar-label span:first-child { font-size: 13px; color: var(--text-dim); }
      .bar-label span:last-child { font-size: 13px; color: var(--text); font-family: 'DM Mono', monospace; }
      .bar-track { display: flex; height: 22px; border-radius: 4px; overflow: hidden; background: var(--bar-track-bg); margin-bottom: 10px; }
      .bar-seg { height: 100%; transition: width 0.3s ease; }

      .loop-legend { border-top: 1px solid var(--border); padding: 8px 16px; display: flex; gap: 16px; flex-wrap: wrap; justify-content: center; background: var(--panel); flex-shrink: 0; }
      .loop-item { display: flex; align-items: center; gap: 6px; }
      .loop-tag { font-size: 9px; font-weight: 700; padding: 2px 6px; border-radius: 3px; letter-spacing: 0.5px; }
      .loop-desc { font-size: 9px; color: var(--text-dim); }

      .sources { font-size: 9px; color: var(--text-dim); line-height: 1.6; border-top: 1px solid var(--border); padding-top: 12px; }
      .sources-title { font-weight: 600; color: var(--text); font-size: 12px; margin-bottom: 4px; }
      .chart-title { font-size: 13px; color: var(--text); text-transform: uppercase; letter-spacing: 1px; padding: 10px 16px 4px; font-weight: 600; }

      ::-webkit-scrollbar { width: 6px; } ::-webkit-scrollbar-track { background: var(--bg); } ::-webkit-scrollbar-thumb { background: var(--border-lt); border-radius: 3px; }
      button { font-family: 'DM Sans', sans-serif; } button:hover { filter: brightness(1.15); }

      .tab-body { display: none; flex: 1; overflow: hidden; }
      .tab-body.active { display: flex; }

      .theme-toggle { background: var(--panel-alt); border: 1px solid var(--border); border-radius: 6px; padding: 5px 10px; cursor: pointer; color: var(--text-dim); font-size: 16px; margin-left: 8px; line-height: 1; transition: all 0.2s; display: flex; align-items: center; }
      .theme-toggle:hover { border-color: var(--blue); color: var(--text); }
    "))
  ),

  # HEADER with Tab Toggle
  div(class = "dash-header",
    div(style = "display:flex;align-items:center;",
      HTML('<div class="f1-logo" style="background:#e8334a;color:#fff;font-weight:800;font-size:18px;padding:4px 10px;border-radius:4px;letter-spacing:1px;font-family:\'DM Sans\',sans-serif;">F1</div>'),
      div(
        div(class = "dash-title", "Net Zero 2030 \u2014 Systems Simulation Dashboard"),
      ),
      div(class = "tab-toggle",
        actionButton("tab_static", "Static What-If", class = "tab-btn active"),
        actionButton("tab_dynamic", "Time-Dynamic Sim", class = "tab-btn")
      )
    ),
    div(style = "display:flex;align-items:center;",
      span(class = "scope-badge", "Scope 1 + 2 + 3"),
      span(class = "scope-badge", "Target: 0 tCO\u2082e by 2030"),
      tags$button(id = "theme_toggle", class = "theme-toggle", title = "Toggle light/dark theme",
        HTML("&#x2600;"))
    )
  ),

  # ═══════════════════════════════════════════════════════════════════════
  # TAB 1: STATIC WHAT-IF
  # ═══════════════════════════════════════════════════════════════════════
  div(id = "static_tab", class = "tab-body active", style = "display:flex; flex:1; overflow:hidden;",

    # LEFT — Static Controls
    div(style = "width:300px; min-width:280px; flex-shrink:0;",
      div(class = "panel-left",
        div(class = "section-header", style = "color:var(--blue);",
            span(class = "bar", style = "background:var(--blue);"), "Policy Levers"),
        div(style = "margin-bottom:14px;",
          div(class = "info-card-title", "Preset Scenarios"),
          uiOutput("st_scenario_buttons")
        ),
        div(class = "slider-row",
          div(class = "slider-label", "SAF Adoption Rate"),
          sliderInput("st_safRate", NULL, min = 0, max = 100, value = 0, step = 1, post = "%", width = "100%")),
        div(class = "slider-row slider-cyan",
          div(class = "slider-label", "Rail / Sea Share"),
          sliderInput("st_railSeaShare", NULL, min = 0, max = 60, value = 0, step = 1, post = "%", width = "100%")),
        div(class = "slider-row slider-purple",
          div(class = "slider-label", "Remote Work Rate"),
          sliderInput("st_remoteRate", NULL, min = 0, max = 100, value = 0, step = 1, post = "%", width = "100%")),
        div(class = "slider-row slider-green",
          div(class = "slider-label", "Renewable Electricity"),
          sliderInput("st_renewableShare", NULL, min = 0, max = 100, value = 0, step = 1, post = "%", width = "100%")),
        div(class = "slider-row slider-orange",
          div(class = "slider-label", "Carbon Offset Rate"),
          sliderInput("st_offsetRate", NULL, min = 0, max = 100, value = 0, step = 1, post = "%", width = "100%")),
        div(class = "slider-row slider-orange",
          div(class = "slider-label", "Offset Effectiveness"),
          sliderInput("st_offsetEffectiveness", NULL, min = 20, max = 100, value = 70, step = 5, post = "%", width = "100%")),
        div(class = "slider-row slider-purple",
          div(class = "slider-label", "Calendar Size"),
          sliderInput("st_calendarSize", NULL, min = 18, max = 26, value = 21, step = 1, post = " races", width = "100%")),
        div(class = "slider-row slider-cyan",
          div(class = "slider-label", "Calendar Regionalisation"),
          sliderInput("st_regionalisation", NULL, min = 0, max = 100, value = 0, step = 1, post = "%", width = "100%")),
        actionButton("st_reset_btn", "Reset to 2018 Baseline", class = "reset-btn"),
        div(class = "info-card", style = "margin-top:14px;",
          div(class = "info-card-title", "Computed Factors"),
          uiOutput("st_factors_ui")
        )
      )
    ),

    # CENTER — CLD
    div(style = "flex:1; min-width:400px; display:flex; flex-direction:column; overflow:hidden;",
      div(style = "flex:1; padding:8px 12px; overflow:hidden;",
        uiOutput("st_cld_svg")
      ),
      div(class = "loop-legend",
        div(class = "loop-item",
          span(class = "loop-tag", style = "color:var(--red);background:var(--red-dim);", "R1 GROWTH"),
          span(class = "loop-desc", "Revenue \u2192 Calendar \u2192 Emissions \u2191")),
        div(class = "loop-item",
          span(class = "loop-tag", style = "color:var(--green);background:var(--green-dim);", "B1 INTERMODAL"),
          span(class = "loop-desc", "Pressure \u2192 Rail/Sea \u2191 \u2192 Em \u2193")),
        div(class = "loop-item",
          span(class = "loop-tag", style = "color:var(--green);background:var(--green-dim);", "B2 CLEAN ENERGY"),
          span(class = "loop-desc", "Pressure \u2192 SAF+RE \u2191 \u2192 Em \u2193")),
        div(class = "loop-item",
          span(class = "loop-tag", style = "color:var(--green);background:var(--green-dim);", "B3 REMOTE OPS"),
          span(class = "loop-desc", "Pressure \u2192 Digital \u2191 \u2192 Em \u2193"))
      )
    ),

    # RIGHT — Static Results
    div(style = "width:310px; min-width:280px; flex-shrink:0;",
      div(class = "panel-right",
        div(class = "section-header", style = "color:var(--green);",
            span(class = "bar", style = "background:var(--green);"), "Results Dashboard"),
        uiOutput("st_results_ui")
      )
    )
  ),

  # ═══════════════════════════════════════════════════════════════════════
  # TAB 2: TIME-DYNAMIC SD
  # ═══════════════════════════════════════════════════════════════════════
  div(id = "dynamic_tab", class = "tab-body", style = "flex:1; overflow:hidden;",

    # LEFT — Dynamic Controls
    div(style = "width:300px; min-width:280px; flex-shrink:0;",
      div(class = "panel-left",
        div(class = "section-header", style = "color:var(--blue);",
            span(class = "bar", style = "background:var(--blue);"), "Policy Levers"),
        div(style = "margin-bottom:14px;",
          div(class = "info-card-title", "Preset Scenarios"),
          uiOutput("td_scenario_buttons")
        ),
        tags$div(class = "info-card-title", style = "margin-bottom:6px;margin-top:4px;", "Adoption Speeds (gap-closing rate per year)"),
        div(class = "slider-row", div(class = "slider-label", "SAF Scale-up Speed"),
            sliderInput("td_saf_speed", NULL, min = 0, max = 50, value = 1, step = 1, post = "%/yr", width = "100%")),
        div(class = "slider-row slider-cyan", div(class = "slider-label", "Intermodal Shift Speed"),
            sliderInput("td_intermodal_speed", NULL, min = 0, max = 50, value = 2, step = 1, post = "%/yr", width = "100%")),
        div(class = "slider-row slider-purple", div(class = "slider-label", "Remote Work Growth"),
            sliderInput("td_remote_speed", NULL, min = 0, max = 30, value = 2, step = 1, post = "%/yr", width = "100%")),
        div(class = "slider-row slider-green", div(class = "slider-label", "Clean Grid Growth"),
            sliderInput("td_re_speed", NULL, min = 0, max = 30, value = 2, step = 1, post = "%/yr", width = "100%")),
        div(class = "slider-row slider-orange", div(class = "slider-label", "Offset Ramp-up"),
            sliderInput("td_offset_ramp", NULL, min = 0, max = 25, value = 0, step = 1, post = "%/yr", width = "100%")),
        div(class = "slider-row slider-orange", div(class = "slider-label", "Offset Effectiveness"),
            sliderInput("td_offset_effectiveness", NULL, min = 20, max = 100, value = 70, step = 5, post = "%", width = "100%")),
        tags$div(class = "info-card-title", style = "margin-bottom:6px;margin-top:10px;", "Calendar Policy"),
        div(class = "slider-row slider-purple", div(class = "slider-label", "Calendar Size"),
            sliderInput("td_calendar_size", NULL, min = 18, max = 26, value = 21, step = 1, post = " races", width = "100%")),
        div(class = "slider-row slider-cyan", div(class = "slider-label", "Calendar Regionalisation"),
            sliderInput("td_regionalisation", NULL, min = 0, max = 100, value = 0, step = 1, post = "%", width = "100%")),
        actionButton("td_reset_btn", "Reset to 2018 Baseline", class = "reset-btn"),
        div(class = "info-card", style = "margin-top:14px;",
          div(class = "info-card-title", "2030 Stock Values (simulated)"),
          uiOutput("td_stocks_2030_ui")
        )
      )
    ),

    # CENTER — Charts
    div(style = "flex:1; min-width:400px;", class = "panel-center",
      div(style = "display:flex; align-items:center; gap:16px;",
        div(class = "chart-title", style = "margin-bottom:0;", "Emissions Trajectory (2024 \u2013 2030)"),
        div(style = "font-size:0.85em; opacity:0.7; display:flex; align-items:center; gap:4px; white-space:nowrap;",
          checkboxInput("td_show_category_breakdown", NULL, value = FALSE, width = "auto"),
          tags$span("Show category breakdown")
        )
      ),
      div(style = "flex:1; padding: 0 16px 8px; min-height:0;",
        plotOutput("td_emissions_plot", height = "100%")
      ),
      div(class = "chart-title", "Stock Evolution (2024 \u2013 2030)"),
      div(style = "flex:1; padding: 0 16px 8px; min-height:0;",
        plotOutput("td_stocks_plot", height = "100%")
      )
    ),

    # RIGHT — Dynamic Results
    div(style = "width:300px; min-width:270px; flex-shrink:0;",
      div(class = "panel-right",
        div(class = "section-header", style = "color:var(--green);",
            span(class = "bar", style = "background:var(--green);"), "2030 Results"),
        uiOutput("td_results_ui")
      )
    )
  )
)

# ═══════════════════════════════════════════════════════════════════════════
# SERVER
# ═══════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ─── THEME ───
  current_th <- reactive({
    theme <- input$app_theme
    if (!is.null(theme) && theme == "light") TH_LIGHT else TH_DARK
  })

  # ─── TAB SWITCHING ───
  active_tab <- reactiveVal("static")

  observeEvent(input$tab_static, {
    active_tab("static")
    session$sendCustomMessage("switchTab", "static")
  })
  observeEvent(input$tab_dynamic, {
    active_tab("dynamic")
    session$sendCustomMessage("switchTab", "dynamic")
  })

  # ═══════════════════════════════════════════════════════════════════════
  # STATIC DASHBOARD SERVER
  # ═══════════════════════════════════════════════════════════════════════
  st_active_scenario <- reactiveVal("Y2018")
  st_sc_counter <- reactiveVal(0)

  output$st_scenario_buttons <- renderUI({
    st_sc_counter()
    cur <- st_active_scenario()
    sc_info <- list(
      list(id = "st_sc_y2018", key = "Y2018",     label = "2018 Baseline", desc = "Pre-intervention"),
      list(id = "st_sc_y2023", key = "Y2023",      label = "2023 Level",    desc = "Early-stage progress"),
      list(id = "st_sc_y2024", key = "Y2024",      label = "2024 Level",    desc = "Accelerated action"),
      list(id = "st_sc_nz",    key = "NetZero",    label = "Net Zero", desc = "Max-intervention")
    )
    div(class = "scenario-grid", lapply(sc_info, function(s) {
      cls <- if (s$key == cur) { if (s$key == "NetZero") "scenario-btn active-nz" else "scenario-btn active" } else "scenario-btn"
      actionButton(s$id, label = HTML(paste0("<span class='btn-label'>", s$label, "</span><span class='btn-desc'>", s$desc, "</span>")), class = cls)
    }))
  })

  st_apply_scenario <- function(name) {
    sc <- ST_SCENARIOS[[name]]
    updateSliderInput(session, "st_safRate",         value = sc$safRate)
    updateSliderInput(session, "st_railSeaShare",    value = sc$railSeaShare)
    updateSliderInput(session, "st_remoteRate",      value = sc$remoteRate)
    updateSliderInput(session, "st_renewableShare",  value = sc$renewableShare)
    updateSliderInput(session, "st_offsetRate",      value = sc$offsetRate)
    updateSliderInput(session, "st_calendarSize",    value = sc$calendarSize)
    updateSliderInput(session, "st_regionalisation", value = sc$regionalisation)
    if (!is.null(sc$offsetEffectiveness))
      updateSliderInput(session, "st_offsetEffectiveness", value = sc$offsetEffectiveness)
    st_active_scenario(name)
    st_sc_counter(st_sc_counter() + 1)
  }

  observeEvent(input$st_sc_y2018, st_apply_scenario("Y2018"))
  observeEvent(input$st_sc_y2023, st_apply_scenario("Y2023"))
  observeEvent(input$st_sc_y2024, st_apply_scenario("Y2024"))
  observeEvent(input$st_sc_nz,    st_apply_scenario("NetZero"))
  observeEvent(input$st_reset_btn, st_apply_scenario("Y2018"))

  observe({
    input$st_safRate; input$st_railSeaShare; input$st_remoteRate
    input$st_renewableShare; input$st_offsetRate; input$st_calendarSize; input$st_regionalisation
    isolate({
      cur <- st_active_scenario()
      if (!is.null(cur) && cur != "") {
        s <- ST_SCENARIOS[[cur]]
        if (!is.null(s)) {
          if (input$st_safRate != s$safRate || input$st_railSeaShare != s$railSeaShare ||
              input$st_remoteRate != s$remoteRate || input$st_renewableShare != s$renewableShare ||
              input$st_offsetRate != s$offsetRate || input$st_calendarSize != s$calendarSize ||
              input$st_regionalisation != s$regionalisation) {
            st_active_scenario("")
            st_sc_counter(st_sc_counter() + 1)
          }
        }
      }
    })
  })

  st_R <- reactive({
    # Compute model values from levers
    R <- calculate_emissions(
      saf_rate             = input$st_safRate,
      railsea_share        = input$st_railSeaShare,
      remote_rate          = input$st_remoteRate,
      renewable_share      = input$st_renewableShare,
      offset_rate          = input$st_offsetRate,
      calendar_size        = input$st_calendarSize,
      regionalisation      = input$st_regionalisation,
      offset_effectiveness = input$st_offsetEffectiveness
    )
    # Override with F1 report values when a preset scenario is active
    cur <- st_active_scenario()
    sc  <- if (!is.null(cur) && cur != "") ST_SCENARIOS[[cur]] else NULL
    if (!is.null(sc) && !is.null(sc$overrides)) {
      ov <- sc$overrides
      R$logistics  <- ov$logistics
      R$biz_travel <- ov$biz_travel
      R$vip        <- ov$vip
      R$staff      <- ov$staff
      R$event_ops  <- ov$event_ops
      R$power_unit <- ov$power_unit
      R$facilities <- ov$facilities
      R$gross      <- ov$logistics + ov$biz_travel + ov$event_ops + ov$facilities + ov$power_unit
      CO  <- input$st_offsetRate / 100
      eff <- input$st_offsetEffectiveness / 100
      R$net        <- R$gross - (R$gross * CO * eff)
      R$net_best   <- R$gross - (R$gross * CO * 1.00)
      R$net_worst  <- R$gross - (R$gross * CO * 0.20)
      R$reduction_pct <- ((BASELINE_TOTAL - R$net) / BASELINE_TOTAL) * 100
    }
    R
  })

  st_inputs_list <- reactive({
    list(
      safRate = input$st_safRate, railSeaShare = input$st_railSeaShare,
      remoteRate = input$st_remoteRate, renewableShare = input$st_renewableShare,
      offsetRate = input$st_offsetRate, offsetEffectiveness = input$st_offsetEffectiveness,
      calendarSize = input$st_calendarSize,
      regionalisation = input$st_regionalisation
    )
  })

  output$st_cld_svg <- renderUI({ build_cld_svg(st_inputs_list(), st_R(), current_th()) })

  output$st_factors_ui <- renderUI({
    r <- st_R()
    facts <- list(
      c("Calendar Multiplier (CM)", fmt3(r$calendar_mult)),
      c("Distance Multiplier (DM)", fmt3(r$distance_mult)),
      c("Effective Calendar (ECF)", fmt3(r$ecf)),
      c("Aviation EF (Aef)",        fmt3(r$aviation_ef)),
      c("Logistics EF (Lef)",       fmt3(r$logistics_ef)),
      c("Grid EF (Gef)",            fmt3(r$grid_ef))
    )
    tagList(lapply(facts, function(f) {
      div(class = "factor-row",
        span(class = "factor-label", f[1]),
        span(class = "factor-value", f[2]))
    }))
  })

  output$st_results_ui <- renderUI({
    r <- st_R()
    th <- current_th()
    is_nz <- r$net < 1
    net_col <- if (is_nz) th$green else th$red
    hero_class <- if (is_nz) "net-hero green" else "net-hero red"
    red_col <- if (r$reduction_pct >= 100) th$green else if (r$reduction_pct >= 50) th$blue else th$orange

    chart_data <- data.frame(
      label = c("Logistics", "Business Travel", "Event Operations", "Facilities", "Power Unit"),
      value = c(r$logistics, r$biz_travel, r$event_ops, r$facilities, r$power_unit),
      color = c(th$red, th$orange, th$yellow, th$blue, th$gray),
      stringsAsFactors = FALSE)

    base_segs <- data.frame(
      label = c("Logistics", "Biz Travel", "Event Ops", "Facilities", "Power Unit"),
      value = c(BASELINE_LOGISTICS, BASELINE_BIZTRAVEL, BASELINE_EVENTOPS, BASELINE_FACILITIES, BASELINE_POWERUNIT),
      color = c(th$red, th$orange, th$yellow, th$blue, th$gray), stringsAsFactors = FALSE)
    cur_segs <- data.frame(
      label = c("Logistics", "Biz Travel", "Event Ops", "Facilities", "Power Unit"),
      value = c(r$logistics, r$biz_travel, r$event_ops, r$facilities, r$power_unit),
      color = c(th$red, th$orange, th$yellow, th$blue, th$gray), stringsAsFactors = FALSE)

    bar_html <- function(label_text, segs, max_val) {
      total <- sum(segs$value)
      seg_html <- paste(sapply(seq_len(nrow(segs)), function(i) {
        if (segs$value[i] <= 0) return("")
        w <- (segs$value[i] / max_val) * 100
        sprintf('<div class="bar-seg" style="width:%.1f%%;background:%s;" title="%s: %s tCO2e"></div>',
                w, segs$color[i], segs$label[i], fmt(segs$value[i]))
      }), collapse = "")
      sprintf('<div class="bar-label"><span>%s</span><span>%s</span></div><div class="bar-track">%s</div>',
        label_text, fmt(total), seg_html)
    }

    legend_html <- paste(sapply(seq_len(nrow(chart_data)), function(i) {
      pct <- if (r$gross > 0) (chart_data$value[i] / r$gross) * 100 else 0
      pct_str <- if (pct > 0.05) paste0(fmt1(pct), "%") else "<0.1%"
      sprintf('<div class="legend-row"><div class="legend-dot" style="background:%s;"></div>
        <span class="legend-label">%s</span><span class="legend-val">%s</span>
        <span class="legend-pct">%s</span></div>',
        chart_data$color[i], chart_data$label[i], fmt(chart_data$value[i]), pct_str)
    }), collapse = "")

    tagList(
      div(class = hero_class,
        div(style = paste0("font-size:12px;color:", th$textDim, ";text-transform:uppercase;letter-spacing:1.5px;margin-bottom:4px;"), "Net Emissions"),
        div(class = "net-value", style = paste0("color:", net_col, ";"), if (is_nz) "0" else fmt(round(r$net))),
        div(style = paste0("font-size:12px;color:", th$textDim, ";margin-top:4px;"), HTML("tCO&#x2082;e")),
        div(style = paste0("font-size:14px;font-weight:600;margin-top:8px;color:", red_col, ";"),
            paste0(fmt1(min(r$reduction_pct, 100)), "% reduction from baseline")),
        if (is_nz) div(class = "nz-badge", "NET ZERO ACHIEVED") else NULL
      ),
      div(class = "metric-row",
        div(class = "metric-card",
          div(class = "metric-label", "Gross"), div(class = "metric-value", style = paste0("color:", th$text, ";"), fmt(round(r$gross))),
          div(class = "metric-unit", HTML("tCO&#x2082;e"))),
        div(class = "metric-card",
          div(class = "metric-label", "Offset"), div(class = "metric-value", style = paste0("color:", th$orange, ";"),
              paste0(input$st_offsetRate, "% @ ", input$st_offsetEffectiveness, "% eff.")),
          div(class = "metric-unit", paste0("\u2212", fmt(round(r$gross - r$net)), " t")),
          if (input$st_offsetRate > 0) div(style = paste0("font-size:9px;color:", th$cyan, ";margin-top:2px;"),
              paste0("Range: ", fmt(round(r$net_best)), "\u2013", fmt(round(r$net_worst)), " net")) else NULL),
        div(class = "metric-card",
          div(class = "metric-label", "Gap"),
          div(class = "metric-value", style = paste0("color:", if (is_nz) th$green else th$red, ";"), if (is_nz) "0" else fmt(round(r$net))),
          div(class = "metric-unit", "to net zero"))
      ),
      div(class = "info-card", style = "padding:12px 0;text-align:center;", build_donut_svg(chart_data, 200, th)),
      div(class = "info-card",
        div(class = "info-card-title", "Emission Breakdown"),
        HTML(legend_html),
        div(style = paste0("border-top:1px solid ", th$border, ";margin-top:8px;padding-top:8px;display:flex;justify-content:space-between;"),
          span(style = paste0("font-size:13px;font-weight:600;color:", th$text, ";"), "Gross Total"),
          span(style = paste0("font-size:13px;font-weight:700;color:", th$heading, ";font-family:'DM Mono',monospace;"),
               HTML(paste0(fmt(round(r$gross)), " tCO&#x2082;e"))))
      ),
      div(class = "info-card",
        div(class = "info-card-title", "Baseline vs Current (Gross)"),
        HTML(bar_html("2018 Baseline", base_segs, BASELINE_TOTAL)),
        HTML(bar_html("Current Scenario", cur_segs, BASELINE_TOTAL))
      ),
      div(class = "info-card",
        div(class = "info-card-title", "Business Travel Split"),
        div(class = "metric-row", style = "margin-bottom:0;",
          div(class = "metric-card",
            div(class = "metric-label", "VIP (15%)"), div(class = "metric-value", style = paste0("color:", th$orange, ";"), fmt(round(r$vip))),
            div(class = "metric-unit", HTML("tCO&#x2082;e"))),
          div(class = "metric-card",
            div(class = "metric-label", "Staff (85%)"), div(class = "metric-value", style = paste0("color:", th$orange, ";"), fmt(round(r$staff))),
            div(class = "metric-unit", HTML("tCO&#x2082;e")))
        )
      ),
      )
  })

  # ═══════════════════════════════════════════════════════════════════════
  # TIME-DYNAMIC DASHBOARD SERVER
  # ═══════════════════════════════════════════════════════════════════════
  td_active_scenario <- reactiveVal("Y2018")
  td_sc_counter <- reactiveVal(0)

  output$td_scenario_buttons <- renderUI({
    td_sc_counter()
    cur <- td_active_scenario()
    sc_info <- list(
      list(id = "td_sc_y2018", key = "Y2018",     label = "2018 Baseline", desc = "Pre-intervention"),
      list(id = "td_sc_y2023", key = "Y2023",      label = "2023 Level",    desc = "Early-stage progress"),
      list(id = "td_sc_y2024", key = "Y2024",      label = "2024 Level",    desc = "Accelerated action"),
      list(id = "td_sc_nz",    key = "NetZero",    label = "Net Zero", desc = "Max-intervention")
    )
    div(class = "scenario-grid", lapply(sc_info, function(s) {
      cls <- if (s$key == cur) { if (s$key == "NetZero") "scenario-btn active-nz" else "scenario-btn active" } else "scenario-btn"
      actionButton(s$id, label = HTML(paste0("<span class='btn-label'>", s$label, "</span><span class='btn-desc'>", s$desc, "</span>")), class = cls)
    }))
  })

  td_apply_scenario <- function(name) {
    sc <- TD_SCENARIOS[[name]]
    updateSliderInput(session, "td_saf_speed",        value = sc$saf_speed)
    updateSliderInput(session, "td_intermodal_speed",  value = sc$intermodal_speed)
    updateSliderInput(session, "td_remote_speed",      value = sc$remote_speed)
    updateSliderInput(session, "td_re_speed",          value = sc$re_speed)
    updateSliderInput(session, "td_offset_ramp",       value = sc$offset_ramp)
    updateSliderInput(session, "td_calendar_size",     value = sc$calendar_size)
    updateSliderInput(session, "td_regionalisation",   value = sc$regionalisation)
    if (!is.null(sc$offset_effectiveness))
      updateSliderInput(session, "td_offset_effectiveness", value = sc$offset_effectiveness)
    td_active_scenario(name)
    td_sc_counter(td_sc_counter() + 1)
  }

  observeEvent(input$td_sc_y2018, td_apply_scenario("Y2018"))
  observeEvent(input$td_sc_y2023, td_apply_scenario("Y2023"))
  observeEvent(input$td_sc_y2024, td_apply_scenario("Y2024"))
  observeEvent(input$td_sc_nz,    td_apply_scenario("NetZero"))
  observeEvent(input$td_reset_btn, td_apply_scenario("Y2018"))

  observe({
    input$td_saf_speed; input$td_intermodal_speed; input$td_remote_speed
    input$td_re_speed; input$td_offset_ramp; input$td_calendar_size; input$td_regionalisation
    isolate({
      cur <- td_active_scenario()
      if (!is.null(cur) && cur != "") {
        s <- TD_SCENARIOS[[cur]]
        if (!is.null(s)) {
          if (input$td_saf_speed != s$saf_speed || input$td_intermodal_speed != s$intermodal_speed ||
              input$td_remote_speed != s$remote_speed || input$td_re_speed != s$re_speed ||
              input$td_offset_ramp != s$offset_ramp || input$td_calendar_size != s$calendar_size ||
              input$td_regionalisation != s$regionalisation) {
            td_active_scenario("")
            td_sc_counter(td_sc_counter() + 1)
          }
        }
      }
    })
  })

  td_sim <- reactive({
    run_simulation(
      saf_speed            = input$td_saf_speed,
      intermodal_speed     = input$td_intermodal_speed,
      remote_speed         = input$td_remote_speed,
      re_speed             = input$td_re_speed,
      offset_ramp          = input$td_offset_ramp,
      calendar_size        = input$td_calendar_size,
      regionalisation      = input$td_regionalisation,
      offset_effectiveness = input$td_offset_effectiveness
    )
  })

  # ─── EMISSIONS TRAJECTORY PLOT ───
  output$td_emissions_plot <- renderPlot({
    d <- td_sim()
    th <- current_th()
    par(bg = th$bg, fg = th$text, col.axis = th$textDim, col.lab = th$text,
        col.main = th$heading, family = "mono", mar = c(3.5, 5, 1, 1), cex.axis = 1.1, cex.lab = 1.15)
    y_max <- max(d$Gross, BASELINE_TOTAL) * 1.05
    plot(d$time, d$Gross, type = "n", xlim = c(2024, 2030), ylim = c(0, y_max),
         xlab = "", ylab = "", axes = FALSE)
    y_ticks <- pretty(c(0, y_max), n = 5)
    abline(h = y_ticks, col = th$border, lwd = 0.5)
    abline(v = 2024:2030, col = th$border, lwd = 0.5)
    abline(h = BASELINE_TOTAL, col = th$textDim, lty = 3, lwd = 1.5)
    text(2024.1, BASELINE_TOTAL, "2018 baseline", col = th$textDim, cex = 0.95, adj = c(0, -0.5))
    abline(h = 0, col = th$green, lty = 2, lwd = 1.5)
    polygon(c(d$time, rev(d$time)), c(d$Gross, rev(d$Net)),
            col = adjustcolor(th$orange, alpha.f = 0.25), border = NA)
    if (isTRUE(input$td_show_category_breakdown)) {
      cat_cols <- c(th$red, th$orange, th$yellow, th$blue, th$gray)
      cats <- cbind(d$Logistics, d$BizTravel, d$EventOps, d$Facilities, d$PowerUnit)
      cum <- rep(0, nrow(d))
      for (j in seq_len(5)) {
        prev <- cum; cum <- cum + cats[, j]
        polygon(c(d$time, rev(d$time)), c(cum, rev(prev)),
                col = adjustcolor(cat_cols[j], alpha.f = 0.30), border = NA)
      }
    }
    lines(d$time, d$Gross, col = th$text, lwd = 3)
    net_col <- if (tail(d$Net, 1) < 1) th$green else th$red
    # Offset uncertainty band (best-case to worst-case)
    has_offset <- any(d$Net_best != d$Net_worst)
    if (has_offset) {
      polygon(c(d$time, rev(d$time)), c(d$Net_best, rev(d$Net_worst)),
              col = adjustcolor(th$cyan, alpha.f = 0.25), border = NA)
      lines(d$time, d$Net_best,  col = th$cyan, lwd = 1.5, lty = 3)
      lines(d$time, d$Net_worst, col = th$cyan, lwd = 1.5, lty = 3)
    }
    lines(d$time, d$Net, col = net_col, lwd = 3)
    axis(1, at = 2024:2030, labels = 2024:2030, col = th$border, col.ticks = th$borderLt)
    axis(2, at = y_ticks, labels = format(y_ticks, big.mark = ","), col = th$border, col.ticks = th$borderLt, las = 1)
    mtext("tCO\u2082e", side = 2, line = 3.8, col = th$textDim, cex = 1.0)
    leg_labels <- c("Gross emissions", "Net emissions", "Carbon offset")
    leg_cols   <- c(th$text, net_col, th$orange)
    leg_lwd    <- c(3, 3, NA)
    leg_pch    <- c(NA, NA, 15)
    if (has_offset) {
      leg_labels <- c(leg_labels, "Offset uncertainty")
      leg_cols   <- c(leg_cols, th$cyan)
      leg_lwd    <- c(leg_lwd, 1.5)
      leg_pch    <- c(leg_pch, NA)
    }
    legend("topright",
           legend = leg_labels, col = leg_cols,
           lwd = leg_lwd, pch = leg_pch, lty = c(1, 1, NA, if (has_offset) 3),
           pt.cex = 1.8,
           bg = adjustcolor(th$panel, alpha.f = 0.9), box.col = th$border, text.col = th$text, cex = 1.0)
    text(2030, tail(d$Gross, 1), fmt(tail(d$Gross, 1)), col = th$text, cex = 1.0, adj = c(1.1, -0.5))
    text(2030, max(tail(d$Net, 1), 1000), fmt(round(tail(d$Net, 1))),
         col = net_col, cex = 1.0, adj = c(1.1, 1.5))
    box(col = th$border)
  }, bg = "transparent")

  # ─── STOCK EVOLUTION PLOT ───
  output$td_stocks_plot <- renderPlot({
    d <- td_sim()
    th <- current_th()
    par(bg = th$bg, fg = th$text, col.axis = th$textDim, col.lab = th$text,
        col.main = th$heading, family = "mono", mar = c(3.5, 5, 1, 1), cex.axis = 1.1, cex.lab = 1.15)
    plot(d$time, d$SAF * 100, type = "n", xlim = c(2024, 2030), ylim = c(0, 105),
         xlab = "", ylab = "", axes = FALSE)
    abline(h = seq(0, 100, 20), col = th$border, lwd = 0.5)
    abline(v = 2024:2030, col = th$border, lwd = 0.5)
    lines(d$time, d$SAF * 100,     col = th$blue,   lwd = 2.5, lty = 1)
    lines(d$time, d$RailSea * 100, col = th$cyan,   lwd = 2.5, lty = 2)
    lines(d$time, d$RE * 100,      col = th$green,  lwd = 2.5, lty = 1)
    lines(d$time, d$Remote * 100,  col = th$purple, lwd = 2.5, lty = 4)
    lines(d$time, d$Offset * 100,  col = th$orange, lwd = 2.5, lty = 5)
    axis(1, at = 2024:2030, labels = 2024:2030, col = th$border, col.ticks = th$borderLt)
    axis(2, at = seq(0, 100, 20), labels = paste0(seq(0, 100, 20), "%"),
         col = th$border, col.ticks = th$borderLt, las = 1)
    mtext("Adoption %", side = 2, line = 3.8, col = th$textDim, cex = 1.0)
    legend("topleft",
           legend = c("SAF adoption", "Rail/Sea share", "Renewable elec.", "Remote work", "Offset rate"),
           col = c(th$blue, th$cyan, th$green, th$purple, th$orange),
           lwd = 2.5, lty = c(1, 2, 1, 4, 5),
           bg = adjustcolor(th$panel, alpha.f = 0.9),
           box.col = th$border, text.col = th$text, cex = 1.0)
    box(col = th$border)
  }, bg = "transparent")

  # ─── 2030 STOCK VALUES ───
  output$td_stocks_2030_ui <- renderUI({
    d <- td_sim()
    th <- current_th()
    n <- nrow(d)
    items <- list(
      c("SAF Adoption",    sprintf("%.0f%%", d$SAF[n] * 100)),
      c("Rail/Sea Share",  sprintf("%.0f%%", d$RailSea[n] * 100)),
      c("Renewable Elec.", sprintf("%.0f%%", d$RE[n] * 100)),
      c("Remote Work",     sprintf("%.0f%%", d$Remote[n] * 100)),
      c("Offset Rate",     sprintf("%.0f%%", d$Offset[n] * 100)),
      c("Calendar",        sprintf("%.0f races", d$Calendar[n]))
    )
    tagList(lapply(items, function(f) {
      div(style = "display:flex;justify-content:space-between;margin-bottom:4px;",
        span(style = paste0("font-size:12px;color:", th$textDim, ";"), f[1]),
        span(style = paste0("font-size:13px;color:", th$text, ";font-family:'DM Mono',monospace;"), f[2]))
    }))
  })

  # ─── Force dynamic-tab outputs to render even when hidden ───
  # (Custom tab switching bypasses Shiny's visibility detection)
  outputOptions(output, "td_scenario_buttons", suspendWhenHidden = FALSE)
  outputOptions(output, "td_stocks_2030_ui",   suspendWhenHidden = FALSE)
  outputOptions(output, "td_emissions_plot",   suspendWhenHidden = FALSE)
  outputOptions(output, "td_stocks_plot",      suspendWhenHidden = FALSE)

  # ─── DYNAMIC RESULTS PANEL ───
  output$td_results_ui <- renderUI({
    d <- td_sim()
    th <- current_th()
    n <- nrow(d)
    r_net   <- d$Net[n]
    r_gross <- d$Gross[n]
    r_red   <- d$Reduction[n]
    is_nz   <- r_net < 1

    chart_data <- data.frame(
      label = c("Logistics", "Business Travel", "Event Ops", "Facilities", "Power Unit"),
      value = c(d$Logistics[n], d$BizTravel[n], d$EventOps[n], d$Facilities[n], d$PowerUnit[n]),
      color = c(th$red, th$orange, th$yellow, th$blue, th$gray), stringsAsFactors = FALSE)

    net_col <- if (is_nz) th$green else th$red
    hero_cls <- if (is_nz) "net-hero green" else "net-hero red"
    red_col <- if (r_red >= 100) th$green else if (r_red >= 50) th$blue else th$orange

    legend_html <- paste(sapply(seq_len(nrow(chart_data)), function(i) {
      pct <- if (r_gross > 0) (chart_data$value[i] / r_gross) * 100 else 0
      sprintf('<div class="legend-row"><div class="legend-dot" style="background:%s;"></div>
        <span class="legend-label">%s</span><span class="legend-val">%s</span>
        <span class="legend-pct">%s</span></div>',
        chart_data$color[i], chart_data$label[i], fmt(chart_data$value[i]),
        if (pct > 0.05) paste0(fmt1(pct), "%") else "<0.1%")
    }), collapse = "")

    tagList(
      div(class = hero_cls,
        div(style = paste0("font-size:12px;color:", th$textDim, ";text-transform:uppercase;letter-spacing:1.5px;margin-bottom:4px;"), "2030 Net Emissions"),
        div(class = "net-value", style = paste0("color:", net_col, ";"), if (is_nz) "0" else fmt(round(r_net))),
        div(style = paste0("font-size:12px;color:", th$textDim, ";margin-top:4px;"), HTML("tCO&#x2082;e")),
        div(style = paste0("font-size:14px;font-weight:600;margin-top:8px;color:", red_col, ";"),
            paste0(fmt1(min(r_red, 100)), "% reduction from baseline")),
        if (is_nz) div(class = "nz-badge", "NET ZERO ACHIEVED") else NULL
      ),
      div(class = "metric-row",
        div(class = "metric-card",
          div(class = "metric-label", "2030 Gross"), div(class = "metric-value", style = paste0("color:", th$text, ";"), fmt(round(r_gross))),
          div(class = "metric-unit", HTML("tCO&#x2082;e"))),
        div(class = "metric-card",
          div(class = "metric-label", "2024 Start"), div(class = "metric-value", style = paste0("color:", th$textDim, ";"), fmt(round(d$Gross[1]))),
          div(class = "metric-unit", HTML("tCO&#x2082;e"))),
        div(class = "metric-card",
          div(class = "metric-label", "Offset"), div(class = "metric-value", style = paste0("color:", th$orange, ";"),
              paste0(sprintf("%.0f%%", d$Offset[n]*100), " @ ", input$td_offset_effectiveness, "% eff.")),
          div(class = "metric-unit", paste0("\u2212", fmt(round(r_gross - r_net)), " t")),
          if (d$Offset[n] > 0) div(style = paste0("font-size:11px;color:", th$cyan, ";margin-top:2px;"),
              paste0("Range: ", fmt(round(d$Net_best[n])), "\u2013", fmt(round(d$Net_worst[n])), " net")) else NULL)
      ),
      div(class = "info-card", style = "padding:10px 0;text-align:center;", build_donut_svg(chart_data, 180, th)),
      div(class = "info-card",
        div(class = "info-card-title", "2030 Emission Breakdown"),
        HTML(legend_html),
        div(style = paste0("border-top:1px solid ", th$border, ";margin-top:8px;padding-top:8px;display:flex;justify-content:space-between;"),
          span(style = paste0("font-size:13px;font-weight:600;color:", th$text, ";"), "Gross Total"),
          span(style = paste0("font-size:13px;font-weight:700;color:", th$heading, ";font-family:'DM Mono',monospace;"),
               HTML(paste0(fmt(round(r_gross)), " tCO&#x2082;e"))))
      ),
      div(class = "info-card",
        div(class = "info-card-title", "Business Travel Split (2030)"),
        div(class = "metric-row", style = "margin-bottom:0;",
          div(class = "metric-card",
            div(class = "metric-label", "VIP (15%)"), div(class = "metric-value", style = paste0("color:", th$orange, ";"), fmt(round(d$VIP[n]))),
            div(class = "metric-unit", HTML("tCO&#x2082;e"))),
          div(class = "metric-card",
            div(class = "metric-label", "Staff (85%)"), div(class = "metric-value", style = paste0("color:", th$orange, ";"), fmt(round(d$Staff[n]))),
            div(class = "metric-unit", HTML("tCO&#x2082;e")))
        )
      ),
      )
  })

  outputOptions(output, "td_results_ui", suspendWhenHidden = FALSE)

}

# ═══════════════════════════════════════════════════════════════════════════
# ADD TAB-SWITCHING JS TO UI HEAD
# ═══════════════════════════════════════════════════════════════════════════
ui <- tagList(
  tags$head(tags$script(HTML("
    Shiny.addCustomMessageHandler('switchTab', function(tab) {
      var staticTab = document.getElementById('static_tab');
      var dynamicTab = document.getElementById('dynamic_tab');
      var btnStatic = document.getElementById('tab_static');
      var btnDynamic = document.getElementById('tab_dynamic');
      if (tab === 'static') {
        staticTab.style.display = 'flex';
        staticTab.classList.add('active');
        dynamicTab.style.display = 'none';
        dynamicTab.classList.remove('active');
        btnStatic.classList.add('active');
        btnDynamic.classList.remove('active');
      } else {
        dynamicTab.style.display = 'flex';
        dynamicTab.classList.add('active');
        staticTab.style.display = 'none';
        staticTab.classList.remove('active');
        btnDynamic.classList.add('active');
        btnStatic.classList.remove('active');
      }
      /* Trigger resize so Shiny recalculates plot dimensions after tab switch */
      setTimeout(function() { $(window).trigger('resize'); }, 150);
    });
    Shiny.addCustomMessageHandler('initTabs', function(msg) {});

    /* Theme toggle */
    $(document).on('click', '#theme_toggle', function() {
      var body = document.body;
      var btn = document.getElementById('theme_toggle');
      body.classList.toggle('light-theme');
      var isLight = body.classList.contains('light-theme');
      btn.innerHTML = isLight ? '&#x1F319;' : '&#x2600;';
      Shiny.setInputValue('app_theme', isLight ? 'light' : 'dark', {priority: 'event'});
    });
  "))),
  ui
)

shinyApp(ui = ui, server = server)
