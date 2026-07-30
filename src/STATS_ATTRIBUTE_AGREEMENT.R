# /***********************************************************************
# * Custom SPSS Statistics extension (not an IBM product)
# * Attribute Agreement Analysis extension for IBM SPSS Statistics
# * (C) Copyright 2026 Aruna Saraswathy
# ************************************************************************/

# ══════════════════════════════════════════════════════════════════════════════
# STATS ATTRIBUTE AGREEMENT — R Extension for IBM SPSS Statistics
# Author  : Aruna Saraswathy
# Version : 1.0.0
# Date    : 2026-06-20
#
# Attribute Agreement Analysis (categorical / Gage study for nominal or
# ordinal responses, e.g. Pass/Fail, Defect categories, visual ratings).
# Ported from the standalone "Attribute Agreement Analysis — Phase 2" Tk/GUI
# Python prototype to a native R extension, following the same architecture
# as STATS_GAGE_RNR (which this script was modeled on).
#
# Functional areas (mirrors every checkbox/tab in the Python Phase-2 GUI):
#   /GENERATE  - Worksheet Designer  (build an empty rating sheet, optionally
#                writing it straight into the active dataset as new cases)
#   /OPTIONS   - Which agreement statistics to compute (kappa, within/between
#                appraiser agreement, confusion matrices, vs-standard,
#                Kendall's W)
#   /OUTPUT    - Summary header + disagreement-source listing
#   /CHARTS    - Between-appraiser heatmap, within-appraiser bar, vs-standard
#                bar, confusion-matrix plot, category pie, key-metrics panel
#   /STUDYINFO - Study metadata (free-text, printed as a text block)
#
# Charts use base R graphics and rely on the standard IBM R Integration
# Plug-in behaviour of auto-capturing any open graphics device between
# spsspkg.StartProcedure()/spsspkg.EndProcedure() into the Viewer — the same
# mechanism STATS_GAGE_RNR.R uses (no explicit spssRGraphics.Submit() calls
# were present in that working reference, so none are added here either).
# ══════════════════════════════════════════════════════════════════════════════


# ── Utility helpers ──────────────────────────────────────────────────────────

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1]) && nzchar(as.character(a[1]))) a else b

strip_quotes <- function(s) {
    if (is.null(s) || !nzchar(as.character(s))) return(as.character(s))
    gsub('^["\']|["\']$', '', as.character(s))
}

# Every keyword in this extension that lacks an <EnumValue> list (i.e. is a
# free-form string, like APPRAISERNAMES or CATEGORIES) arrives in whatever
# case the user typed, per-call-site, with no normalization from SPSS.
# Keyword-style YES/NO values DO use EnumValue lists in the .xml, but SPSS's R
# plug-in still passes them through as literal strings ("YES"/"NO"), not
# logicals, so every call site re-derives its own boolean defensively.
to_bool <- function(x) {
    if (is.logical(x)) return(isTRUE(x))
    if (is.null(x) || length(x) == 0) return(FALSE)
    toupper(as.character(x)[1]) %in% c("YES", "TRUE", "1")
}

to_num <- function(x, default = NA) {
    v <- suppressWarnings(as.numeric(as.character(x)))
    if (is.null(v) || length(v) == 0 || is.na(v)) return(default)
    v
}

to_int <- function(x, default = NA) as.integer(to_num(x, default))

notempty <- function(x) !is.null(x) && length(x) > 0 && nzchar(as.character(x)[1]) && as.character(x)[1] != "."

StartProcedure <- function(nm, id) {
    ver <- tryCatch(as.numeric(substr(spsspkg.GetSPSSVersion(), 1, 2)), error = function(e) 19)
    if (ver >= 19) spsspkg.StartProcedure(nm, id) else spsspkg.StartProcedure(id)
}

spss_pivot_table <- function(title, rows, cols, data, corner = "", caption = "") {
    df <- as.data.frame(data, stringsAsFactors = FALSE)
    colnames(df) <- cols
    rownames(df) <- rows
    if (nzchar(corner)) {
        df <- cbind(data.frame(X = rows, stringsAsFactors = FALSE), df)
        colnames(df)[1] <- corner
    }
    # spsspivottable.Display() accepts caption= directly and attaches it as a
    # genuine footnote on the pivot table itself (confirmed working pattern,
    # e.g. STATS_GAGE_RNR.R's Variance Components / Gage R&R Metrics /
    # Type 1 Gauge Study tables all use exactly this). Passing caption
    # straight through here (instead of firing a separate, detached
    # spsspkg.TextBlock call) is what makes the footnote actually appear
    # under the table rather than as an unrelated floating text item.
    if (nzchar(caption)) {
        ok <- tryCatch({ spsspivottable.Display(df, title = title, caption = caption); TRUE },
                        error = function(e) FALSE)
        if (!ok) {
            spsspivottable.Display(df, title = title)
            tryCatch(spsspkg.TextBlock(paste0(title, " Note"), caption), error = function(e) NULL)
        }
    } else {
        spsspivottable.Display(df, title = title)
    }
    invisible(NULL)
}

aaa_colors <- list(
    good = "#2e7d32", warn = "#f9a825", bad = "#c62828",
    grid = "#cccccc", line = "#1565c0", bar = "#1976d2"
)

# Continuous green->amber->red gradient (101 steps, one per whole
# percentage point) for encoding a 0-100 rate as a single hex color.
# Used by the disagreement-network charts (base-R and HTML/Plotly) so that
# edges remain visually distinguishable even when every pair's rate falls
# within the same coarse bucket -- which is the common case for a
# well-functioning measurement system (every pair under ~10% disagreement)
# and is exactly the situation where a fixed 3-bucket threshold scheme
# renders every edge identically, making the chart look like a single-color
# blob with no information despite real (if small) differences between
# pairs. Every other rate-encoded chart in this script (the between-
# appraiser heatmap, the confusion-matrix heatmap) already uses a continuous
# gradient instead of fixed buckets; this brings the network chart in line.
.aaa_disagree_gradient <- colorRampPalette(c("#2e7d32", "#f9a825", "#c62828"))(101)
disagree_gradient_color <- function(pct) {
    idx <- max(1L, min(101L, round(pct) + 1L))
    .aaa_disagree_gradient[idx]
}

# ── Fleiss' kappa (multi-rater generalization of Cohen's kappa) ─────────────
# Reference: Fleiss, J.L. (1971) "Measuring nominal scale agreement among
# many raters", Psychological Bulletin 76(5). Significance test (SE/z/p
# under H0: kappa=0) from Fleiss, Nee & Landis (1979) "Large sample variance
# of kappa in the case of different sets of raters", Psychological Bulletin
# 86(5) -- the same large-sample variance formula used by R's irr package
# (kappam.fleiss) and cited in the AIAG MSA-4 manual's attribute analysis
# discussion. Each appraiser x trial reading is treated as one "rater" for
# a given sample, which is the standard way Attribute Agreement Analysis
# generalizes Fleiss' kappa to a repeated-trial design, consistent with the
# AIAG MSA-4 manual's attribute analysis discussion. Subjects (samples) with
# any missing reading among the m
# raters are dropped (complete-case), since Fleiss' kappa requires the same
# number of raters m for every subject.
#
# `mat` must be an n_subjects x n_categories matrix of counts n_ij (number of
# raters who placed subject i into category j); every row must sum to the
# same m.
fleiss_kappa_core <- function(mat) {
    n <- nrow(mat)
    if (n == 0) return(NULL)
    row_sums <- rowSums(mat)
    m <- round(mean(row_sums))
    if (m < 2 || n < 1) return(NULL)

    p_j <- colSums(mat) / (n * m)
    P_e <- sum(p_j^2)

    P_i <- (rowSums(mat^2) - m) / (m * (m - 1))
    P_bar <- mean(P_i)

    kappa <- if (P_e < 1) (P_bar - P_e) / (1 - P_e) else NA_real_

    se0 <- NA_real_; z <- NA_real_; pval <- NA_real_
    if (!is.na(kappa) && P_e < 1) {
        sum_pj2 <- sum(p_j^2); sum_pj3 <- sum(p_j^3)
        var0 <- (2 / (n * m * (m - 1))) *
            ((sum_pj2 - (2 * m - 3) * sum_pj2^2 + 2 * (m - 2) * sum_pj3) / (1 - sum_pj2)^2)
        if (is.finite(var0) && var0 > 0) {
            se0  <- sqrt(var0)
            z    <- kappa / se0
            pval <- 2 * pnorm(-abs(z))
        }
    }

    # Per-category kappa_j (one-vs-rest extent of agreement for category j).
    # Point estimate only (Fleiss 1971); no significance test is attached
    # here because the per-category large-sample variance formula could not
    # be verified against a live computation in this environment (no R
    # interpreter available) -- reporting an unverified SE/p-value as fact
    # would violate this project's verification discipline, so kappa_j is
    # shown as a descriptive statistic only.
    kappa_j <- vapply(seq_len(ncol(mat)), function(j) {
        pj <- p_j[j]
        if (pj <= 0 || pj >= 1) return(NA_real_)
        num <- sum(mat[, j] * (m - mat[, j]))
        denom <- n * m * (m - 1) * pj * (1 - pj)
        if (denom <= 0) return(NA_real_)
        1 - num / denom
    }, numeric(1))

    list(n = n, m = m, p_j = p_j, P_bar = P_bar, P_e = P_e,
         kappa = kappa, se0 = se0, z = z, p_value = pval, kappa_j = kappa_j)
}

# Build the n_subjects x n_categories rater-count matrix from a set of
# appraiser-major rating columns (every appraiser x trial column is one
# rater); rows with any missing reading are dropped (complete-case), as
# Fleiss' kappa requires a constant number of raters m per subject.
build_rater_count_matrix <- function(df, all_cols, categories) {
    sub <- df[all_cols]
    complete <- stats::complete.cases(sub)
    sub <- sub[complete, , drop = FALSE]
    if (nrow(sub) == 0) return(NULL)
    t(apply(sub, 1, function(row) {
        vapply(categories, function(cat) sum(row == cat), integer(1))
    }))
}

# ── AIAG-style one-vs-rest classification metrics per category vs Standard ──
# Generalizes the classic (binary Pass/Fail) AIAG MSA-4 "Effectiveness / Miss
# Rate / False Alarm Rate" table to any number of categories using standard
# one-vs-rest confusion-matrix decomposition (Effectiveness=accuracy,
# Sensitivity=recall, Specificity, PPV, NPV, Miss Rate=1-Sensitivity=FNR,
# False Alarm Rate=FPR). Pools every appraiser's primary-trial reading
# against Standard (same population the existing confusion matrices and
# vs-Standard table draw from), so the per-category breakdown below is
# additive detail, not a re-derivation under different assumptions.
classification_metrics <- function(actual, predicted, categories) {
    ok <- !is.na(actual) & !is.na(predicted)
    actual <- actual[ok]; predicted <- predicted[ok]
    n <- length(actual)
    rows <- lapply(categories, function(cat) {
        TP <- sum(actual == cat & predicted == cat)
        FN <- sum(actual == cat & predicted != cat)
        FP <- sum(actual != cat & predicted == cat)
        TN <- sum(actual != cat & predicted != cat)
        sens <- if ((TP + FN) > 0) TP / (TP + FN) else NA_real_
        spec <- if ((TN + FP) > 0) TN / (TN + FP) else NA_real_
        ppv  <- if ((TP + FP) > 0) TP / (TP + FP) else NA_real_
        npv  <- if ((TN + FN) > 0) TN / (TN + FN) else NA_real_
        acc  <- if (n > 0) (TP + TN) / n else NA_real_
        data.frame(Category = cat, Effectiveness = acc * 100, Sensitivity = sens * 100,
                   Specificity = spec * 100, PPV = ppv * 100, NPV = npv * 100,
                   MissRate = (1 - sens) * 100, FalseAlarmRate = (1 - spec) * 100,
                   stringsAsFactors = FALSE)
    })
    do.call(rbind, rows)
}


# ══════════════════════════════════════════════════════════════════════════════
# MAIN ENTRY POINT
# ══════════════════════════════════════════════════════════════════════════════

Run <- function(args) {
    cmdname <- args[[1]]
    args    <- args[[2]]

    oobj <- tryCatch({
        spsspkg.Syntax(templ = list(
            # Main
            spsspkg.Template("VARS",          subc="",         ktype="varname", var="vars", islist=TRUE),
            spsspkg.Template("NTRIALS",       subc="",         ktype="int",     var="ntrials"),
            spsspkg.Template("APPRAISERNAMES",subc="",         ktype="str",     var="appraisernames"),
            spsspkg.Template("SAMPLEID",      subc="",         ktype="varname", var="sampleid"),
            spsspkg.Template("REFVAR",        subc="",         ktype="varname", var="standard"),
            spsspkg.Template("CATEGORIES",    subc="",         ktype="str",     var="categories"),
            # /GENERATE
            spsspkg.Template("GENDATA",       subc="GENERATE", ktype="str",     var="gendata"),
            spsspkg.Template("APPRAISERS",    subc="GENERATE", ktype="int",     var="gen_appraisers"),
            spsspkg.Template("SAMPLES",       subc="GENERATE", ktype="int",     var="gen_samples"),
            spsspkg.Template("TRIALS",        subc="GENERATE", ktype="int",     var="gen_trials"),
            spsspkg.Template("GENCATS",       subc="GENERATE", ktype="str",     var="gen_cats"),
            spsspkg.Template("INCLUDESTD",    subc="GENERATE", ktype="str",     var="gen_includestd"),
            spsspkg.Template("CREATEVARS",    subc="GENERATE", ktype="str",     var="gen_createvars"),
            # /OPTIONS
            spsspkg.Template("KAPPA",         subc="OPTIONS",  ktype="str",     var="opt_kappa"),
            spsspkg.Template("WITHIN",        subc="OPTIONS",  ktype="str",     var="opt_within"),
            spsspkg.Template("BETWEEN",       subc="OPTIONS",  ktype="str",     var="opt_between"),
            spsspkg.Template("CONFUSION",     subc="OPTIONS",  ktype="str",     var="opt_confusion"),
            spsspkg.Template("VSSTANDARD",    subc="OPTIONS",  ktype="str",     var="opt_vsstandard"),
            spsspkg.Template("KENDALL",       subc="OPTIONS",  ktype="str",     var="opt_kendall"),
            spsspkg.Template("FLEISS",        subc="OPTIONS",  ktype="str",     var="opt_fleiss"),
            spsspkg.Template("PABAK",         subc="OPTIONS",  ktype="str",     var="opt_pabak"),
            spsspkg.Template("EFFECTIVENESS", subc="OPTIONS",  ktype="str",     var="opt_effectiveness"),
            # /OUTPUT
            spsspkg.Template("SUMMARY",       subc="OUTPUT",   ktype="str",     var="out_summary"),
            spsspkg.Template("DISAGREE",      subc="OUTPUT",   ktype="str",     var="out_disagree"),
            spsspkg.Template("MAXLIST",       subc="OUTPUT",   ktype="int",     var="out_maxlist"),
            spsspkg.Template("RRSUMMARY",     subc="OUTPUT",   ktype="str",     var="out_rrsummary"),
            spsspkg.Template("SHOWINTERP",    subc="OUTPUT",   ktype="str",     var="out_interpretation"),
            # /CHARTS
            spsspkg.Template("HEATMAP",       subc="CHARTS",   ktype="str",     var="ch_heatmap"),
            spsspkg.Template("WITHINBAR",     subc="CHARTS",   ktype="str",     var="ch_withinbar"),
            spsspkg.Template("VSSTDBAR",      subc="CHARTS",   ktype="str",     var="ch_vsstdbar"),
            spsspkg.Template("CONFUSIONPLOT", subc="CHARTS",   ktype="str",     var="ch_confusionplot"),
            spsspkg.Template("CATPIE",        subc="CHARTS",   ktype="str",     var="ch_catpie"),
            spsspkg.Template("METRICSPANEL",  subc="CHARTS",   ktype="str",     var="ch_metricspanel"),
            spsspkg.Template("KAPPAFOREST",   subc="CHARTS",   ktype="str",     var="ch_kappaforest"),
            spsspkg.Template("SANKEY",        subc="CHARTS",   ktype="str",     var="ch_sankey"),
            spsspkg.Template("MOSAIC",        subc="CHARTS",   ktype="str",     var="ch_mosaic"),
            spsspkg.Template("DISAGREENETWORK", subc="CHARTS", ktype="str",     var="ch_disagreenetwork"),
            # /STUDYINFO
            spsspkg.Template("STUDY",         subc="STUDYINFO",ktype="str",     var="study_name"),
            spsspkg.Template("BY",            subc="STUDYINFO",ktype="str",     var="study_by"),
            spsspkg.Template("DATE",          subc="STUDYINFO",ktype="str",     var="study_date")
        ))
    }, error = function(e) {
        spsspivottable.Display(
            data.frame(Error = conditionMessage(e), stringsAsFactors = FALSE),
            title = "Syntax Error"
        )
        NULL
    })

    if (!is.null(oobj)) spsspkg.processcmd(oobj, args, "aaa_main")
}


# ══════════════════════════════════════════════════════════════════════════════
# aaa_main — argument normalization, dispatch to generate vs analyze
# ══════════════════════════════════════════════════════════════════════════════

aaa_main <- function(
    vars=NULL, ntrials=1, appraisernames=NULL, sampleid=NULL, standard=NULL, categories=NULL,
    gendata="NO", gen_appraisers=3, gen_samples=30, gen_trials=2, gen_cats="Pass,Fail",
    gen_includestd="YES", gen_createvars="NO",
    opt_kappa="YES", opt_within="YES", opt_between="YES", opt_confusion="YES",
    opt_vsstandard="YES", opt_kendall="YES",
    opt_fleiss="YES", opt_pabak="NO", opt_effectiveness="YES",
    out_summary="YES", out_disagree="YES", out_maxlist=20, out_rrsummary="YES", out_interpretation="YES",
    ch_heatmap="YES", ch_withinbar="YES", ch_vsstdbar="YES", ch_confusionplot="YES",
    ch_catpie="YES", ch_metricspanel="YES",
    ch_kappaforest="YES", ch_sankey="YES", ch_mosaic="YES", ch_disagreenetwork="YES",
    study_name=NULL, study_by=NULL, study_date=NULL
) {
    gendata        <- to_bool(gendata)
    gen_includestd <- to_bool(gen_includestd)
    gen_createvars <- to_bool(gen_createvars)
    opt_kappa      <- to_bool(opt_kappa)
    opt_within     <- to_bool(opt_within)
    opt_between    <- to_bool(opt_between)
    opt_confusion  <- to_bool(opt_confusion)
    opt_vsstandard <- to_bool(opt_vsstandard)
    opt_kendall    <- to_bool(opt_kendall)
    opt_fleiss     <- to_bool(opt_fleiss)
    opt_pabak      <- to_bool(opt_pabak)
    opt_effectiveness <- to_bool(opt_effectiveness)
    out_summary    <- to_bool(out_summary)
    out_disagree   <- to_bool(out_disagree)
    out_rrsummary  <- to_bool(out_rrsummary)
    out_interpretation <- to_bool(out_interpretation)
    ch_heatmap     <- to_bool(ch_heatmap)
    ch_withinbar   <- to_bool(ch_withinbar)
    ch_vsstdbar    <- to_bool(ch_vsstdbar)
    ch_confusionplot <- to_bool(ch_confusionplot)
    ch_catpie      <- to_bool(ch_catpie)
    ch_metricspanel<- to_bool(ch_metricspanel)
    ch_kappaforest <- to_bool(ch_kappaforest)
    ch_sankey      <- to_bool(ch_sankey)
    ch_mosaic      <- to_bool(ch_mosaic)
    ch_disagreenetwork <- to_bool(ch_disagreenetwork)

    out_maxlist <- to_int(out_maxlist, 20); if (is.na(out_maxlist) || out_maxlist < 1) out_maxlist <- 20
    ntrials     <- to_int(ntrials, 1);      if (is.na(ntrials) || ntrials < 1) ntrials <- 1

    study_name <- strip_quotes(study_name %||% "")
    study_by   <- strip_quotes(study_by   %||% "")
    study_date <- strip_quotes(study_date %||% "")
    # GUI sends 'NONE' as the sentinel for "not filled in" — treat it as blank
    # so it never leaks into the summary table or HTML report header.
    if (toupper(study_name) == "NONE") study_name <- ""
    if (toupper(study_by)   == "NONE") study_by   <- ""
    if (toupper(study_date) == "NONE") study_date <- ""

    # ── 1. Worksheet generation mode ───────────────────────────────────────
    if (isTRUE(gendata)) {
        ga <- to_int(gen_appraisers, 3); if (is.na(ga) || ga < 1) ga <- 3
        gs <- to_int(gen_samples, 30);   if (is.na(gs) || gs < 1) gs <- 30
        gt <- to_int(gen_trials, 2);     if (is.na(gt) || gt < 1) gt <- 2
        cats <- trimws(strsplit(strip_quotes(gen_cats %||% "Pass,Fail"), ",")[[1]])
        cats <- cats[nzchar(cats)]

        # NOTE: aaa_generate_worksheet() manages its own StartProcedure/
        # EndProcedure cycles internally (one for the worksheet preview
        # table, and -- when CREATEVARS=YES -- a separate one for the
        # confirmation note, with the actual dataset-creation API calls
        # sitting strictly *between* those two cycles, outside any open
        # procedure). This mirrors the proven Gage R&R reference, whose
        # gage_generate_worksheet() is called completely bare, with no
        # StartProcedure/EndProcedure wrapper at all, around its dataset-
        # creation calls. An earlier version of this code wrapped the
        # entire function call (preview table AND dataset creation) in one
        # single StartProcedure/EndProcedure pair -- that nested the
        # spssdictionary/spssdata calls inside an open procedure, which is
        # why CREATEVARS=YES could write the worksheet table to the Output
        # Viewer but never actually register it as a live SPSS active
        # dataset. Splitting the procedure cycles fixes that without
        # removing any existing output.
        tryCatch({
            aaa_generate_worksheet(ga, gs, gt, cats, isTRUE(gen_includestd), isTRUE(gen_createvars), appraisernames)
        }, error = function(e) {
            StartProcedure("Attribute Agreement Analysis - Generate Worksheet", "STATS_ATTRIBUTE_AGREEMENT")
            spsspivottable.Display(
                data.frame(Error = conditionMessage(e), stringsAsFactors = FALSE),
                title = "Error"
            )
            spsspkg.EndProcedure()
        })
        return(invisible(NULL))
    }

    # spsspkg passes a list-type (islist=TRUE) VariableNameList parameter to R
    # as a list, not a plain character vector -- flatten it now so every
    # downstream use (length(), c(), data.frame column subscripting, etc.)
    # works the same way it would for an ordinary character vector. Without
    # this, df[vars] fails with "invalid subscript type 'list'".
    vars <- unlist(vars)

    # ── 2. Validate required inputs ─────────────────────────────────────────
    if (is.null(vars) || length(vars) < 2)
        stop("VARS must list at least two rater/trial variables (e.g. VARS=app1t1 app1t2 app2t1 app2t2).")
    if (length(vars) %% ntrials != 0)
        stop(sprintf("The number of VARS (%d) is not evenly divisible by NTRIALS (%d). Every appraiser must contribute the same number of trial columns.",
                      length(vars), ntrials))

    n_appraisers <- length(vars) / ntrials
    if (n_appraisers < 2)
        stop("At least two appraisers are required for attribute agreement analysis. Check VARS/NTRIALS.")

    # APPRAISERNAMES is scoped to the GENERATE/GENDATA=YES worksheet-creation
    # path only (where it labels the synthetic appraiser columns -- see the
    # aaa_generate_worksheet() call above, which already returned out of this
    # function before this point whenever gendata was TRUE). This branch only
    # Derive readable labels from the variable names themselves.
    # e.g. Alice_T1, Alice_T2, Bob_T1, Bob_T2 → Alice, Bob
    # Falls back to generic "AppraiserN" only if stripping produces non-unique or
    # empty results (e.g. variables named purely with digits).
    first_vars <- vars[seq(1, length(vars), by = ntrials)]
    appraiser_names <- local({
        for (pat in c("(?i)[_. -]?t(rial)?\\s*\\d+$",  # _T1  _Trial1  T1
                      "[_. -]\\d+$",                       # _1  -1  .1
                      "\\d+$")) {                          # bare trailing digit
            nm <- trimws(gsub(pat, "", first_vars, perl = TRUE))
            if (length(unique(nm)) == n_appraisers && all(nzchar(nm))) return(nm)
        }
        paste0("Appraiser", seq_len(n_appraisers))
    })

    sampleid <- strip_quotes(sampleid %||% "")
    standard <- strip_quotes(standard %||% "")

    vars_needed <- c(vars,
                      if (notempty(sampleid)) sampleid else NULL,
                      if (notempty(standard)) standard else NULL)

    dict <- spssdata.GetDataFromSPSS(vars_needed, keepUserMissing = FALSE)
    df   <- as.data.frame(dict, stringsAsFactors = FALSE)
    names(df) <- vars_needed

    # Normalize every rater/standard value to a trimmed character string so
    # comparisons are exact-match; do NOT change case here (case handling is
    # Normalize every rater/standard value: trim whitespace AND fold to lowercase
    # so that "Pass", "PASS", and "pass" are all treated as the same category.
    # This prevents silent all-zero confusion matrices caused by case mismatches
    # between the Standard variable and the appraiser rating columns.
    for (v in c(vars, if (notempty(standard)) standard else NULL)) {
        df[[v]] <- tolower(ifelse(is.na(df[[v]]), NA_character_, trimws(as.character(df[[v]]))))
        df[[v]][df[[v]] == ""] <- NA_character_
    }

    # Build named list: appraiser_name -> character vector of its trial column names
    appraiser_map <- setNames(
        lapply(seq_len(n_appraisers), function(i) vars[((i - 1) * ntrials + 1):(i * ntrials)]),
        appraiser_names
    )

    # Derive categories: explicit CATEGORIES list wins; otherwise collect from data
    if (notempty(categories)) {
        # Normalize to lowercase to match the tolower() applied to all data values.
        cats <- tolower(trimws(strsplit(strip_quotes(categories), ",")[[1]]))
        cats <- cats[nzchar(cats)]
    } else {
        # Auto-derived: already lowercase because df values were tolower()'d above.
        cats <- sort(unique(na.omit(unlist(c(df[vars], if (notempty(standard)) df[standard] else NULL)))))
    }
    if (length(cats) < 2)
        stop("At least two response categories are required for analysis. Supply CATEGORIES or check your data.")

    n_samples <- nrow(df)
    sample_labels <- if (notempty(sampleid)) as.character(df[[sampleid]]) else as.character(seq_len(n_samples))

    StartProcedure("Attribute Agreement Analysis", "STATS_ATTRIBUTE_AGREEMENT")
    tryCatch({
        results <- aaa_analyze(df, vars, appraiser_map, if (notempty(standard)) standard else NULL,
                                cats, sample_labels,
                                opt_kappa, opt_within, opt_between, opt_confusion,
                                opt_vsstandard, opt_kendall,
                                opt_fleiss || opt_pabak, opt_effectiveness)

        if (out_summary) print_aaa_summary(results, study_name, study_by, study_date)
        if (opt_kappa)      print_kappa_table(results, out_interpretation)
        if (opt_fleiss)     print_fleiss_table(results, out_interpretation)
        if (opt_pabak)      print_pabak_table(results)
        if (opt_kendall)    print_kendall_table(results, out_interpretation)
        if (opt_within)     print_within_table(results)
        if (opt_between)    print_between_table(results)
        if (out_rrsummary && (opt_within || opt_between)) print_rr_summary_table(results)
        if (opt_vsstandard) print_vsstandard_table(results)
        if (opt_effectiveness) print_effectiveness_table(results)
        if (opt_confusion)  print_confusion_tables(results)
        if (out_disagree)   print_disagreement_table(results, out_maxlist)

        if (ch_heatmap && opt_between)        chart_between_heatmap(results)
        if (ch_withinbar && opt_within)        chart_within_bar(results)
        if (ch_vsstdbar && opt_vsstandard)     chart_vsstandard_bar(results)
        if (ch_confusionplot && opt_confusion) chart_confusion_plot(results)
        if (ch_catpie && !is.null(results$std_col)) chart_category_pie(results, df)
        if (ch_metricspanel)                   chart_metrics_panel(results)
        if (ch_kappaforest && opt_kappa)       chart_kappa_forest(results)
        # Rating-flow/Sankey chart removed at user's request (the alluvial
        # layout wasn't useful with more than a couple of appraisers). The
        # ch_sankey/SANKEY parameter is still parsed harmlessly above so
        # existing syntax with SANKEY=YES keeps running without error -- it
        # just no longer produces any chart.
        # Mosaic plot chart removed at the user's request (not confident the
        # equal-height-row/proportional-column-width layout was a
        # mathematically sound way to represent AAA data). The ch_mosaic
        # parameter is still parsed harmlessly above so existing syntax with
        # MOSAIC=YES keeps running without error -- it just no longer
        # produces any chart.
        if (ch_disagreenetwork && opt_between) chart_disagreement_network(results)

        # Interactive HTML report: always generated after all other SPSS output,
        # not gated behind a checkbox -- this is a standing feature of the procedure.
        results$study_name <- study_name
        results$study_by   <- study_by
        results$study_date <- study_date
        results$ch_kappaforest     <- ch_kappaforest
        results$ch_sankey          <- ch_sankey
        results$ch_mosaic          <- ch_mosaic
        results$ch_disagreenetwork <- ch_disagreenetwork
        # Stored so the forest chart and HTML metrics bar can gate Fleiss
        # output correctly: do_fleiss = opt_fleiss || opt_pabak (PABAK needs
        # the Fleiss computation), but Fleiss kappa should only be *shown*
        # in charts/metrics when the user explicitly requested FLEISS=YES.
        results$opt_fleiss <- opt_fleiss
        generate_aaa_html(results, df)

    }, error = function(e) {
        spsspivottable.Display(
            data.frame(Error = conditionMessage(e), stringsAsFactors = FALSE),
            title = "Error"
        )
    })
    spsspkg.EndProcedure()
    invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
# ANALYSIS — direct port of the Python Phase-2 `analyze()` logic
# ══════════════════════════════════════════════════════════════════════════════

aaa_analyze <- function(df, vars, appraiser_map, std_col, categories, sample_labels,
                         do_kappa, do_within, do_between, do_confusion, do_vsstd, do_kendall,
                         do_fleiss = FALSE, do_effectiveness = FALSE) {

    appraisers   <- names(appraiser_map)
    primary_cols <- vapply(appraisers, function(a) appraiser_map[[a]][1], character(1))

    results <- list(
        appraisers = appraisers, n_samples = nrow(df), std_col = std_col,
        vars = vars, categories = categories
    )

    # Overall percent agreement (all trial columns for a sample must match)
    agree_flags_raw <- apply(df[vars], 1, function(row) {
        vals <- row[!is.na(row) & nzchar(row)]
        if (length(vals) > 1) as.integer(length(unique(vals)) == 1) else NA_integer_
    })
    # Kept (with NAs preserved, in original sample order) for the sample-order
    # run chart below -- lets a reviewer spot drift/fatigue effects across the
    # run, which a single overall percentage cannot show.
    results$sample_labels <- sample_labels
    results$agree_by_sample <- agree_flags_raw
    agree_flags <- agree_flags_raw[!is.na(agree_flags_raw)]
    results$overall_pct   <- if (length(agree_flags) > 0) mean(agree_flags) * 100 else 0
    results$overall_agree <- sum(agree_flags)
    results$overall_n     <- length(agree_flags)

    # Kappa (pairwise, simple/Cohen-style, primary trial column per appraiser)
    if (do_kappa && length(appraisers) >= 2) {
        kappa <- list(); kappa_n <- list(); kappa_pe <- list()
        for (pair in combn(appraisers, 2, simplify = FALSE)) {
            a1 <- pair[1]; a2 <- pair[2]
            d1 <- df[[primary_cols[a1]]]; d2 <- df[[primary_cols[a2]]]
            ok <- !is.na(d1) & !is.na(d2)
            d1 <- d1[ok]; d2 <- d2[ok]
            if (length(d1) > 0) {
                po <- mean(d1 == d2)
                cset <- sort(unique(c(d1, d2)))
                pe <- sum(sapply(cset, function(c) mean(d1 == c) * mean(d2 == c)))
                k <- if (pe < 1) (po - pe) / (1 - pe) else 1.0
                nm <- paste0(a1, "_vs_", a2)
                kappa[[nm]]    <- k
                kappa_n[[nm]]  <- length(d1)
                kappa_pe[[nm]] <- pe
            }
        }
        results$kappa    <- kappa
        results$kappa_n  <- kappa_n
        results$kappa_pe <- kappa_pe
    }

    # Within-appraiser agreement (repeat-trial consistency)
    if (do_within) {
        within <- list(); within_agree <- list(); within_n <- list()
        for (a in appraisers) {
            cols <- appraiser_map[[a]]
            if (length(cols) > 1) {
                flags <- apply(df[cols], 1, function(row) {
                    vals <- row[!is.na(row) & nzchar(row)]
                    if (length(vals) > 1) as.integer(length(unique(vals)) == 1) else NA_integer_
                })
                flags <- flags[!is.na(flags)]
                within[[a]]       <- if (length(flags) > 0) mean(flags) * 100 else 0
                within_agree[[a]] <- sum(flags)
                within_n[[a]]     <- length(flags)
            }
        }
        results$within       <- within
        results$within_agree <- within_agree
        results$within_n     <- within_n
    }

    # Between-appraiser agreement (pairwise, primary trial column)
    if (do_between) {
        between <- list(); between_agree <- list(); between_n <- list()
        for (pair in combn(appraisers, 2, simplify = FALSE)) {
            a1 <- pair[1]; a2 <- pair[2]
            d1 <- df[[primary_cols[a1]]]; d2 <- df[[primary_cols[a2]]]
            ok <- !is.na(d1) & !is.na(d2)
            total <- sum(ok)
            agree <- sum(d1[ok] == d2[ok])
            nm <- paste0(a1, "_vs_", a2)
            between[[nm]]       <- if (total > 0) agree / total * 100 else 0
            between_agree[[nm]] <- agree
            between_n[[nm]]     <- total
        }
        results$between       <- between
        results$between_agree <- between_agree
        results$between_n     <- between_n
    }

    # Confusion matrices: each appraiser's primary column vs. Standard
    if (do_confusion && !is.null(std_col)) {
        confusion <- list()
        confusion_std_vals <- list()   # actual Standard values seen, for diagnostics
        for (a in appraisers) {
            d_std <- df[[std_col]]; d_app <- df[[primary_cols[a]]]
            ok <- !is.na(d_std) & !is.na(d_app)
            if (any(ok)) {
                tab <- table(factor(d_std[ok], levels = categories),
                             factor(d_app[ok], levels = categories))
                key <- paste0(a, "_vs_Standard")
                confusion[[key]] <- tab
                confusion_std_vals[[key]] <- sort(unique(d_std[ok]))
            }
        }
        results$confusion          <- confusion
        results$confusion_std_vals <- confusion_std_vals
    }

    # Each appraiser vs. Standard (percent agreement)
    if (do_vsstd && !is.null(std_col)) {
        vs_standard <- list(); vsstd_agree <- list(); vsstd_n <- list()
        for (a in appraisers) {
            d_std <- df[[std_col]]; d_app <- df[[primary_cols[a]]]
            ok <- !is.na(d_std) & !is.na(d_app)
            total <- sum(ok)
            agree <- sum(d_std[ok] == d_app[ok])
            vs_standard[[a]] <- if (total > 0) agree / total * 100 else 0
            vsstd_agree[[a]] <- agree
            vsstd_n[[a]]     <- total
        }
        results$vs_standard <- vs_standard
        results$vsstd_agree <- vsstd_agree
        results$vsstd_n     <- vsstd_n
    }

    # Kendall's W (concordance across appraisers' primary columns, ranked by
    # category order — meaningful only for ordinal category scales)
    if (do_kendall) {
        cat_rank <- setNames(seq_along(categories), categories)
        rank_mat <- sapply(primary_cols, function(col) unname(cat_rank[df[[col]]]))
        complete_rows <- stats::complete.cases(rank_mat)
        rank_mat <- rank_mat[complete_rows, , drop = FALSE]
        if (nrow(rank_mat) > 0) {
            n_obj <- nrow(rank_mat); m_raters <- ncol(rank_mat)
            rank_sums <- rowSums(rank_mat)
            s_val <- sum((rank_sums - mean(rank_sums))^2)
            # Tie correction for Kendall's coefficient of concordance
            # (Kendall, 1962, "Rank Correlation Methods", Ch. 6). Each
            # rater's "rank" here is really its category label re-expressed
            # as a fixed category order (1..k), so for any dataset with more
            # samples than categories (the normal case -- e.g. a binary
            # Pass/Fail study with 30 samples), most objects necessarily
            # share a rater's value: that's a genuine tie, not an artifact.
            # Without the correction term below, the denominator is the
            # untied-ranks value, which is too large whenever ties are
            # present and biases W low. The correction subtracts
            # m * sum_j(T_j), where T_j = sum over each rater j's tied-value
            # groups of (group_size^3 - group_size).
            tie_term <- sum(vapply(seq_len(m_raters), function(j) {
                grp <- table(rank_mat[, j])
                sum(grp^3 - grp)
            }, numeric(1)))
            denom <- (m_raters^2) * (n_obj^3 - n_obj) - m_raters * tie_term
            # n_obj==1, or every rater's values tying into one single group
            # (zero variance), both legitimately drive denom to 0 or below;
            # W is undefined in that case rather than divide-by-zero/negative.
            results$kendall_w <- if (denom > 0) 12 * s_val / denom else NA_real_
        } else {
            results$kendall_w <- NA_real_
        }
    }

    # Fleiss' kappa (overall + per-category) and PABAK, treating every
    # appraiser x trial column as one rater (see fleiss_kappa_core comments).
    if (do_fleiss) {
        mat <- build_rater_count_matrix(df, vars, categories)
        if (!is.null(mat)) {
            fk <- fleiss_kappa_core(mat)
            if (!is.null(fk)) {
                fk$categories <- categories
                results$fleiss <- fk
            }
        }
    }

    # Effectiveness / Miss Rate / False Alarm Rate per category, pooled
    # across every appraiser's primary-trial reading vs. Standard.
    if (do_effectiveness && !is.null(std_col)) {
        actual_all <- character(0); pred_all <- character(0)
        for (a in appraisers) {
            d_std <- df[[std_col]]; d_app <- df[[primary_cols[a]]]
            ok <- !is.na(d_std) & !is.na(d_app)
            actual_all <- c(actual_all, d_std[ok])
            pred_all   <- c(pred_all, d_app[ok])
        }
        if (length(actual_all) > 0)
            results$effectiveness <- classification_metrics(actual_all, pred_all, categories)
    }

    # Disagreement list (any sample where trial columns don't all match)
    disagreements <- list()
    for (i in seq_len(nrow(df))) {
        vals <- unlist(df[i, vars])
        vals <- vals[!is.na(vals) & nzchar(vals)]
        if (length(vals) > 1 && length(unique(vals)) > 1) {
            entry <- list(sample = sample_labels[i])
            if (!is.null(std_col) && !is.na(df[[std_col]][i])) entry$standard <- df[[std_col]][i]
            for (a in appraisers) {
                v <- df[[primary_cols[a]]][i]
                if (!is.na(v)) entry[[a]] <- v
            }
            disagreements[[length(disagreements) + 1]] <- entry
        }
    }
    results$disagreements <- disagreements
    results$primary_cols  <- primary_cols

    results
}


# ══════════════════════════════════════════════════════════════════════════════
# OUTPUT TABLES
# ══════════════════════════════════════════════════════════════════════════════

print_aaa_summary <- function(results, study_name, study_by, study_date) {
    lines <- c(
        sprintf("Samples: %d", results$n_samples),
        sprintf("Appraisers: %s", paste(results$appraisers, collapse = ", ")),
        sprintf("Categories: %s", paste(results$categories, collapse = ", ")),
        sprintf("Overall Percent Agreement: %.2f%%", results$overall_pct)
    )
    if (!is.null(results$overall_n) && results$overall_n > 0) {
        ci <- wilson_ci(results$overall_agree, results$overall_n)
        lines <- c(lines, sprintf("95%% CI (Wilson): %.1f%% - %.1f%%", ci["low"], ci["high"]))
    }
    if (notempty(study_name)) lines <- c(sprintf("Study: %s", study_name), lines)
    if (notempty(study_by))   lines <- c(lines, sprintf("Conducted by: %s", study_by))
    if (notempty(study_date)) lines <- c(lines, sprintf("Date: %s", study_date))

    spss_pivot_table("Attribute Agreement Analysis - Summary",
                      rows = "Overall", cols = "Value",
                      data = matrix(paste(lines, collapse = "\n"), nrow = 1),
                      corner = "")
}

# Landis & Koch (1977) full 6-tier interpretation scale for kappa-type
# statistics: <0 Poor, 0-0.20 Slight, 0.21-0.40 Fair, 0.41-0.60 Moderate,
# 0.61-0.80 Substantial, 0.81-1.00 Almost Perfect.
kappa_interp <- function(k) {
    if (is.na(k))        "n/a"
    else if (k < 0)       "Poor"
    else if (k <= 0.20)   "Slight"
    else if (k <= 0.40)   "Fair"
    else if (k <= 0.60)   "Moderate"
    else if (k <= 0.80)   "Substantial"
    else                  "Almost Perfect"
}

# Wilson score confidence interval for a proportion (x agreements out of n),
# returned as percentages. More reliable than the normal (Wald) approximation
# at the small/extreme proportions common in attribute agreement studies.
wilson_ci <- function(x, n, z = 1.96) {
    if (is.na(n) || n <= 0) return(c(low = NA_real_, high = NA_real_))
    phat   <- x / n
    denom  <- 1 + z^2 / n
    center <- (phat + z^2 / (2 * n)) / denom
    margin <- (z * sqrt((phat * (1 - phat) + z^2 / (4 * n)) / n)) / denom
    c(low = max(0, center - margin) * 100, high = min(1, center + margin) * 100)
}

print_kappa_table <- function(results, show_interp = TRUE) {
    if (is.null(results$kappa) || length(results$kappa) == 0) return(invisible(NULL))
    pairs <- names(results$kappa)
    vals  <- unlist(results$kappa)

    # Large-sample z-test of H0: kappa = 0 for each pairwise kappa, using the
    # standard SE0 = sqrt(pe / (n*(1-pe))) (same form as the Fleiss' kappa
    # significance test above, specialized to the 2-rater case).
    ns  <- if (!is.null(results$kappa_n))  unname(unlist(results$kappa_n[pairs]))  else rep(NA_real_, length(pairs))
    pes <- if (!is.null(results$kappa_pe)) unname(unlist(results$kappa_pe[pairs])) else rep(NA_real_, length(pairs))
    se0 <- ifelse(!is.na(ns) & !is.na(pes) & ns > 0 & pes < 1, sqrt(pes / (ns * (1 - pes))), NA_real_)
    zst <- ifelse(!is.na(se0) & se0 > 0, vals / se0, NA_real_)
    pv  <- ifelse(!is.na(zst), 2 * (1 - pnorm(abs(zst))), NA_real_)

    base_cols <- c("Kappa", "SE (H0: Kappa=0)", "Z", "p-value")
    base_data <- cbind(sprintf("%.4f", vals),
                        ifelse(is.na(se0), "n/a", sprintf("%.4f", se0)),
                        ifelse(is.na(zst), "n/a", sprintf("%.3f", zst)),
                        ifelse(is.na(pv),  "n/a", sprintf("%.4f", pv)))
    if (show_interp) {
        interp <- vapply(vals, kappa_interp, character(1))
        caption_interp <- "Significance test is the large-sample z-test of H0: Kappa=0 (SE0 = sqrt(pe/(n*(1-pe)))). Interpretation tiers follow the Landis & Koch agreement scale: <0 Poor, 0.00-0.20 Slight, 0.21-0.40 Fair, 0.41-0.60 Moderate, 0.61-0.80 Substantial, 0.81-1.00 Almost Perfect. This is a widely used convention, not a universal statistical standard -- treat it as descriptive guidance alongside the significance test, not a pass/fail rule on its own."
        spss_pivot_table("Kappa Statistics (Appraiser Pairs)",
                          rows = pairs, cols = c(base_cols, "Interpretation"),
                          data = cbind(base_data, interp), corner = "Pair", caption = caption_interp)
    } else {
        spss_pivot_table("Kappa Statistics (Appraiser Pairs)",
                          rows = pairs, cols = base_cols,
                          data = base_data, corner = "Pair")
    }
}

print_fleiss_table <- function(results, show_interp = TRUE) {
    fk <- results$fleiss
    if (is.null(fk) || is.null(fk$kappa) || is.na(fk$kappa)) return(invisible(NULL))

    base_vals <- c(sprintf("%.4f", fk$kappa),
                   if (!is.na(fk$se0)) sprintf("%.4f", fk$se0) else "n/a",
                   if (!is.na(fk$z)) sprintf("%.3f", fk$z) else "n/a",
                   if (!is.na(fk$p_value)) sprintf("%.4f", fk$p_value) else "n/a")
    base_cols <- c("Kappa", "SE (H0: Kappa=0)", "Z", "p-value")
    cap <- ""
    if (show_interp) {
        base_vals <- c(base_vals, kappa_interp(fk$kappa))
        base_cols <- c(base_cols, "Interpretation")
        cap <- sprintf("Based on %d raters (appraisers x trials) and %d complete-case samples. Interpretation tiers follow the Landis & Koch agreement scale: <0 Poor, 0.00-0.20 Slight, 0.21-0.40 Fair, 0.41-0.60 Moderate, 0.61-0.80 Substantial, 0.81-1.00 Almost Perfect. This is a widely used convention, not a universal statistical standard.",
                        fk$m, fk$n)
    }
    overall <- matrix(base_vals, nrow = 1)
    spss_pivot_table("Fleiss' Kappa Statistics (All Appraisers, All Trials)",
                      rows = "Overall", cols = base_cols,
                      data = overall, corner = "",
                      caption = cap)

    if (!is.null(fk$kappa_j)) {
        kj <- fk$kappa_j
        valid <- !is.na(kj)
        if (any(valid)) {
            spss_pivot_table("Fleiss' Kappa by Category (One-vs-Rest)",
                              rows = fk$categories[valid], cols = "Kappa (point estimate)",
                              data = matrix(sprintf("%.4f", kj[valid]), ncol = 1), corner = "Category")
        }
    }
}

print_pabak_table <- function(results) {
    fk <- results$fleiss
    if (is.null(fk) || is.null(fk$P_bar) || is.na(fk$P_bar)) return(invisible(NULL))
    pabak <- 2 * fk$P_bar - 1
    spss_pivot_table("Prevalence-Adjusted Bias-Adjusted Kappa (PABAK)",
                      rows = "PABAK", cols = "Value",
                      data = matrix(sprintf("%.4f", pabak), nrow = 1), corner = "Statistic")
}

print_effectiveness_table <- function(results) {
    eff <- results$effectiveness
    if (is.null(eff) || nrow(eff) == 0) return(invisible(NULL))
    cols <- c("Effectiveness %", "Sensitivity %", "Specificity %", "PPV %", "NPV %", "Miss Rate %", "False Alarm Rate %")
    mat <- sapply(c("Effectiveness", "Sensitivity", "Specificity", "PPV", "NPV", "MissRate", "FalseAlarmRate"),
                   function(col) ifelse(is.na(eff[[col]]), "n/a", sprintf("%.2f%%", eff[[col]])))
    spss_pivot_table("Effectiveness / Miss Rate / False Alarm Rate (vs. Standard, by Category)",
                      rows = eff$Category, cols = cols, data = mat, corner = "Category")
}

print_rr_summary_table <- function(results) {
    has_within  <- !is.null(results$within)  && length(results$within)  > 0
    has_between <- !is.null(results$between) && length(results$between) > 0
    if (!has_within && !has_between) return(invisible(NULL))
    rows <- c(); vals <- c()
    if (has_within)  { rows <- c(rows, "Repeatability (avg. within-appraiser %)");  vals <- c(vals, mean(unlist(results$within))) }
    if (has_between) { rows <- c(rows, "Reproducibility (avg. between-appraiser %)"); vals <- c(vals, mean(unlist(results$between))) }
    spss_pivot_table("Repeatability & Reproducibility Summary (AIAG MSA-4 terminology)",
                      rows = rows, cols = "Average Percent",
                      data = matrix(sprintf("%.2f%%", vals), ncol = 1), corner = "")
}

print_kendall_table <- function(results, show_interp = TRUE) {
    if (is.null(results$kendall_w) || is.na(results$kendall_w)) return(invisible(NULL))
    w <- results$kendall_w
    if (show_interp) {
        interp <- if (w > 0.7) "Strong" else if (w > 0.3) "Moderate" else "Weak"
        cap <- "Strong/Moderate/Weak cutoffs at W>0.7/0.3 are an informal, commonly used rule of thumb (no single citable universal standard sets these exact cutoffs) -- read alongside the W value itself, not as a certified threshold."
        spss_pivot_table("Kendall's W Coefficient of Concordance",
                          rows = "Kendall's W", cols = c("W", "Interpretation"),
                          data = matrix(c(sprintf("%.4f", w), interp), nrow = 1), corner = "Statistic",
                          caption = cap)
    } else {
        spss_pivot_table("Kendall's W Coefficient of Concordance",
                          rows = "Kendall's W", cols = "W",
                          data = matrix(sprintf("%.4f", w), nrow = 1), corner = "Statistic")
    }
}

# Shared helper: builds a "low% - high%" Wilson CI string column, one row
# per key, given parallel agree-count and n lists (NULL-safe: falls back to
# "n/a" if counts weren't collected for this results object).
wilson_ci_strings <- function(keys, agree_list, n_list) {
    if (is.null(agree_list) || is.null(n_list)) return(rep("n/a", length(keys)))
    vapply(keys, function(k) {
        ci <- wilson_ci(agree_list[[k]], n_list[[k]])
        if (any(is.na(ci))) "n/a" else sprintf("%.1f%% - %.1f%%", ci["low"], ci["high"])
    }, character(1))
}

print_within_table <- function(results) {
    if (is.null(results$within) || length(results$within) == 0) return(invisible(NULL))
    apps <- names(results$within); vals <- unlist(results$within)
    ci_s <- wilson_ci_strings(apps, results$within_agree, results$within_n)
    spss_pivot_table("Within-Appraiser Agreement (Repeat-Trial Consistency)",
                      rows = apps, cols = c("Percent Agreement", "95% CI (Wilson)"),
                      data = cbind(sprintf("%.2f%%", vals), ci_s), corner = "Appraiser")
}

print_between_table <- function(results) {
    if (is.null(results$between) || length(results$between) == 0) return(invisible(NULL))
    pairs <- names(results$between); vals <- unlist(results$between)
    ci_s <- wilson_ci_strings(pairs, results$between_agree, results$between_n)
    spss_pivot_table("Between-Appraiser Agreement",
                      rows = pairs, cols = c("Percent Agreement", "95% CI (Wilson)"),
                      data = cbind(sprintf("%.2f%%", vals), ci_s), corner = "Pair")
}

print_vsstandard_table <- function(results) {
    if (is.null(results$vs_standard) || length(results$vs_standard) == 0) return(invisible(NULL))
    apps <- names(results$vs_standard); vals <- unlist(results$vs_standard)
    ci_s <- wilson_ci_strings(apps, results$vsstd_agree, results$vsstd_n)
    spss_pivot_table("Each Appraiser vs. Standard",
                      rows = apps, cols = c("Percent Agreement", "95% CI (Wilson)"),
                      data = cbind(sprintf("%.2f%%", vals), ci_s), corner = "Appraiser")
}

print_confusion_tables <- function(results) {
    if (is.null(results$confusion) || length(results$confusion) == 0) return(invisible(NULL))
    categories <- results$categories
    for (nm in names(results$confusion)) {
        tab <- results$confusion[[nm]]
        spss_pivot_table(paste0("Confusion Matrix: ", nm),
                          rows = paste0("Standard=", rownames(tab)),
                          cols = colnames(tab),
                          data = matrix(as.character(tab), nrow = nrow(tab)),
                          corner = "")
        # Emit a diagnostic note when the matrix is all zeros so the user can
        # identify the mismatch without digging into R console output.
        if (sum(tab) == 0) {
            std_vals <- results$confusion_std_vals[[nm]]
            unmatched <- if (!is.null(std_vals)) std_vals[!std_vals %in% categories] else character(0)
            detail <- if (length(unmatched) > 0)
                sprintf("Standard column contains: {%s}  |  Expected categories: {%s}  |  Fix: ensure values match exactly (check case, spaces, and data type).",
                        paste(unmatched[seq_len(min(6, length(unmatched)))], collapse=", "),
                        paste(categories, collapse=", "))
            else if (!is.null(std_vals) && length(std_vals) > 0)
                sprintf("Standard column values {%s} matched categories but produced no counts. Check data encoding.",
                        paste(std_vals, collapse=", "))
            else
                "Standard column appears empty or all NA. Please enter reference values in the Standard variable."
            spss_pivot_table(paste0("Confusion Matrix Data Issue: ", nm),
                              rows = "Diagnosis", cols = "Detail",
                              data = matrix(detail, nrow = 1), corner = "")
        }
    }
}

print_disagreement_table <- function(results, max_list) {
    n_dis <- length(results$disagreements)
    pct   <- if (results$n_samples > 0) n_dis / results$n_samples * 100 else 0

    spss_pivot_table("Disagreement Source Analysis - Summary",
                      rows = "Disagreements", cols = c("Count", "Percent of Samples"),
                      data = matrix(c(as.character(n_dis), sprintf("%.1f%%", pct)), nrow = 1),
                      corner = "")

    if (n_dis == 0) return(invisible(NULL))

    shown <- results$disagreements[seq_len(min(n_dis, max_list))]
    apps  <- results$appraisers
    # Only include "Standard" column when a reference variable was supplied;
    # without REFVAR the column would always be blank, which is misleading.
    cols  <- c(if (!is.null(results$std_col)) "Standard" else NULL, apps)
    rows  <- vapply(shown, function(d) as.character(d$sample), character(1))
    mat   <- t(vapply(shown, function(d) {
        vapply(cols, function(c) {
            key <- if (c == "Standard") "standard" else c
            if (!is.null(d[[key]])) as.character(d[[key]]) else ""
        }, character(1))
    }, character(length(cols))))
    title <- if (nrow(mat) >= n_dis) sprintf("Disagreement Detail (all %d)", n_dis)
             else sprintf("Disagreement Detail (first %d of %d)", nrow(mat), n_dis)
    spss_pivot_table(title, rows = rows, cols = cols, data = mat, corner = "Sample")
}


# ══════════════════════════════════════════════════════════════════════════════
# CHARTS — base R graphics, auto-captured by the R Integration Plug-in
# ══════════════════════════════════════════════════════════════════════════════

chart_between_heatmap <- function(results) {
    if (is.null(results$between) || length(results$between) == 0) return(invisible(NULL))
    apps <- results$appraisers; n <- length(apps)
    m <- matrix(100, n, n, dimnames = list(apps, apps))
    for (key in names(results$between)) {
        parts <- strsplit(key, "_vs_")[[1]]
        m[parts[1], parts[2]] <- results$between[[key]]
        m[parts[2], parts[1]] <- results$between[[key]]
    }
    colors <- colorRampPalette(c("#c62828", "#f9a825", "#2e7d32"))(100)
    op <- par(mar = c(5, 7, 4, 2))
    image(seq_len(n), seq_len(n), t(m)[, n:1], col = colors, zlim = c(0, 100),
          axes = FALSE, xlab = "", ylab = "", main = "Between-Appraiser Agreement Heatmap")
    axis(1, at = seq_len(n), labels = apps, las = 2)
    axis(2, at = seq_len(n), labels = rev(apps), las = 2)
    for (i in seq_len(n)) for (j in seq_len(n))
        text(i, n - j + 1, sprintf("%.1f", m[j, i]), cex = 0.9)
    par(op)
}

chart_within_bar <- function(results) {
    if (is.null(results$within) || length(results$within) == 0) return(invisible(NULL))
    apps <- names(results$within); vals <- unlist(results$within)
    cols <- ifelse(vals >= 90, aaa_colors$good, ifelse(vals >= 80, aaa_colors$warn, aaa_colors$bad))
    barplot(vals, names.arg = apps, col = cols, ylim = c(0, 110),
            ylab = "% Agreement", main = "Within-Appraiser Consistency", border = NA,
            yaxt = "n")
    axis(2, at = seq(0, 100, 25))
    abline(h = 90, lty = 2, col = aaa_colors$good)
    abline(h = 80, lty = 2, col = aaa_colors$warn)
    legend("topright", c("90% threshold", "80% threshold"), lty = 2,
           col = c(aaa_colors$good, aaa_colors$warn), bty = "o", bg = "white", cex = 0.8)
}

chart_vsstandard_bar <- function(results) {
    if (is.null(results$vs_standard) || length(results$vs_standard) == 0) return(invisible(NULL))
    apps <- names(results$vs_standard); vals <- unlist(results$vs_standard)
    cols <- ifelse(vals >= 90, aaa_colors$good, ifelse(vals >= 80, aaa_colors$warn, aaa_colors$bad))
    barplot(vals, names.arg = apps, col = cols, ylim = c(0, 110),
            ylab = "% Agreement", main = "Each Appraiser vs. Standard", border = NA,
            yaxt = "n")
    axis(2, at = seq(0, 100, 25))
    abline(h = 90, lty = 2, col = aaa_colors$good)
    abline(h = 80, lty = 2, col = aaa_colors$warn)
    legend("topright", c("90% threshold", "80% threshold"), lty = 2,
           col = c(aaa_colors$good, aaa_colors$warn), bty = "o", bg = "white", cex = 0.8)
}

chart_confusion_plot <- function(results) {
    if (is.null(results$confusion) || length(results$confusion) == 0) return(invisible(NULL))
    for (nm in names(results$confusion)) {
        tab <- results$confusion[[nm]]
        image(seq_len(ncol(tab)), seq_len(nrow(tab)), t(as.matrix(tab))[, nrow(tab):1],
              col = colorRampPalette(c("white", "#1565c0"))(100),
              axes = FALSE, xlab = "Appraiser", ylab = "Standard",
              main = paste0("Confusion Matrix: ", nm))
        axis(1, at = seq_len(ncol(tab)), labels = colnames(tab), las = 2)
        axis(2, at = seq_len(nrow(tab)), labels = rev(rownames(tab)), las = 2)
        for (i in seq_len(nrow(tab))) for (j in seq_len(ncol(tab)))
            text(j, nrow(tab) - i + 1, as.character(tab[i, j]), cex = 1)
    }
}

chart_category_pie <- function(results, df) {
    if (is.null(results$std_col)) return(invisible(NULL))
    counts <- table(df[[results$std_col]])
    counts <- counts[counts > 0]
    if (length(counts) == 0) return(invisible(NULL))
    pct <- as.numeric(counts) / sum(as.numeric(counts)) * 100
    pie_labels <- sprintf("%s\n%d (%.1f%%)", names(counts), as.integer(counts), pct)
    pie(counts, labels = pie_labels, main = "Category Distribution (Standard)",
        col = rainbow(length(counts), s = 0.6, v = 0.9))
}

chart_metrics_panel <- function(results) {
    plot.new()
    text(0.05, 0.95, "KEY METRICS", cex = 1.3, font = 2, adj = c(0, 1))
    y <- 0.82
    line <- function(txt) { text(0.05, y, txt, adj = c(0, 1), cex = 1.0, family = "mono"); y <<- y - 0.08 }
    line(sprintf("Overall Agreement: %.1f%%", results$overall_pct))
    if (!is.null(results$kendall_w) && !is.na(results$kendall_w)) line(sprintf("Kendall's W: %.3f", results$kendall_w))
    if (!is.null(results$kappa) && length(results$kappa) > 0) line(sprintf("Avg Kappa: %.3f", mean(unlist(results$kappa))))
    n_dis <- length(results$disagreements)
    pct_dis <- if (results$n_samples > 0) n_dis / results$n_samples * 100 else 0
    line(sprintf("Disagreements: %d (%.1f%%)", n_dis, pct_dis))
}

# Base-R counterparts of the 4 new Plotly-only charts, so they also land in
# the SPSS Output Viewer (every other chart has both an interactive HTML
# version and a base-R version captured via the R Integration Plug-in --
# these were previously HTML-only, which is the gap being closed here).

chart_kappa_forest <- function(results) {
    if (is.null(results$kappa) || length(results$kappa) == 0) return(invisible(NULL))
    pairs <- names(results$kappa)
    vals  <- unname(unlist(results$kappa[pairs]))
    ns    <- unname(unlist(results$kappa_n[pairs]))
    pes   <- unname(unlist(results$kappa_pe[pairs]))
    po    <- vals * (1 - pes) + pes
    se    <- ifelse(!is.na(ns) & !is.na(pes) & ns > 0 & pes < 1,
                     sqrt(pmax(po * (1 - po), 0) / (ns * (1 - pes)^2)), NA_real_)
    labels <- gsub("_vs_", " vs ", pairs)
    ci_lo  <- vals - 1.96 * se
    ci_hi  <- vals + 1.96 * se

    # Only add Fleiss' kappa to the forest plot when the user explicitly
    # requested FLEISS=YES -- results$fleiss may be populated for PABAK even
    # when opt_fleiss=FALSE, and silently showing it would contradict the
    # user's intent.
    if (isTRUE(results$opt_fleiss) && !is.null(results$fleiss) && !is.na(results$fleiss$kappa)) {
        fk    <- results$fleiss
        se_fk <- if (!is.na(fk$se0)) fk$se0 else NA_real_
        labels <- c(labels, "Fleiss' Kappa (Overall)")
        vals   <- c(vals, fk$kappa)
        ci_lo  <- c(ci_lo, fk$kappa - 1.96 * se_fk)
        ci_hi  <- c(ci_hi, fk$kappa + 1.96 * se_fk)
    }

    k <- length(vals)
    op <- par(mar = c(4, 14, 4, 2))
    plot(NA, xlim = c(-1, 1), ylim = c(0.5, k + 0.5), yaxt = "n", xlab = "Kappa",
         ylab = "", main = "Kappa Forest Plot")
    zones <- c(-1, 0, 0.20, 0.40, 0.60, 0.80, 1)
    zone_cols <- c("#ffebee", "#fff3e0", "#fffde7", "#f1f8e9", "#e8f5e9", "#c8e6c9")
    for (z in seq_along(zone_cols))
        rect(zones[z], 0, zones[z + 1], k + 1, col = zone_cols[z], border = NA)
    box()
    y <- k:1
    segments(ci_lo, y, ci_hi, y, col = "#1976d2", lwd = 2)
    points(vals, y, pch = 19, col = "#1976d2", cex = 1.3)
    abline(v = 0, lty = 2, col = "#666666")
    axis(2, at = y, labels = labels, las = 2, cex.axis = 0.85)
    par(op)
}

# Mosaic plot chart (base-R version) removed at the user's request -- not
# confident the proportional-column/equal-height-row layout was a
# mathematically sound way to represent AAA confusion-matrix data. Both this
# function and its HTML/Plotly equivalent further below have been deleted;
# ch_mosaic is still parsed harmlessly elsewhere so MOSAIC=YES in existing
# pasted syntax keeps running without error -- it just no longer produces
# any chart.

chart_disagreement_network <- function(results) {
    if (is.null(results$between) || length(results$between) == 0) return(invisible(NULL))
    apps <- results$appraisers; n <- length(apps)
    if (n < 2) return(invisible(NULL))
    angle <- 2 * pi * (seq_len(n) - 1) / n
    nx <- cos(angle); ny <- sin(angle)
    op <- par(mar = c(5, 1, 4, 1))
    plot(NA, xlim = c(-1.9, 1.9), ylim = c(-1.9, 1.9), axes = FALSE, xlab = "", ylab = "",
         main = "Appraiser Disagreement Network", asp = 1)
    for (key in names(results$between)) {
        parts <- strsplit(key, "_vs_")[[1]]
        a1 <- parts[1]; a2 <- parts[2]
        i1 <- match(a1, apps); i2 <- match(a2, apps)
        if (is.na(i1) || is.na(i2)) next
        disagree <- 100 - results$between[[key]]
        col <- disagree_gradient_color(disagree)
        w <- max(1, disagree / 5)
        segments(nx[i1], ny[i1], nx[i2], ny[i2], col = col, lwd = w)
    }
    points(nx, ny, pch = 21, bg = "#1976d2", col = "white", cex = 4)
    # Place labels at 1.3x radius with angle-based horizontal justification
    # so they never sit on top of nodes or edges regardless of appraiser count
    label_r <- 1.3
    lx <- label_r * cos(angle); ly <- label_r * sin(angle)
    cex_lbl <- max(0.65, min(0.9, 0.9 - 0.04 * (n - 3)))
    for (i in seq_len(n)) {
        ax <- if (cos(angle[i]) > 0.15) 0 else if (cos(angle[i]) < -0.15) 1 else 0.5
        ay <- if (sin(angle[i]) > 0.15) 0 else if (sin(angle[i]) < -0.15) 1 else 0.5
        text(lx[i], ly[i], apps[i], adj = c(ax, ay), cex = cex_lbl, font = 2)
    }
    legend("bottom", legend = c("Low disagreement", "Moderate", "High disagreement"),
           col = c("#2e7d32", "#f9a825", "#c62828"), lwd = 3, bty = "n", cex = 0.75,
           horiz = TRUE, xpd = TRUE, inset = c(0, -0.12),
           title = "Edge color: disagreement rate")
    par(op)
}


# ══════════════════════════════════════════════════════════════════════════════
# INTERACTIVE HTML REPORT — Plotly.js, mirroring the proven STATS_GAGE_RNR
# pattern: build a single self-contained HTML string (Plotly loaded from the
# public CDN, exactly as the Gage R&R reference does), write it to disk, tell
# the user where it is via spsspkg.TextBlock, and try to auto-open it with
# browseURL(). This is NOT routed through the .cfe's HTMLOutput Execution
# container -- the Gage reference does not use that mechanism either, it
# writes a standalone report file, so this follows the same proven path
# rather than an unverified one.
# ══════════════════════════════════════════════════════════════════════════════

js_num_arr <- function(x) paste0("[", paste(ifelse(is.na(x), "null", sprintf("%.4f", x)), collapse = ","), "]")
js_str_arr <- function(x) paste0("[", paste0('"', gsub('"', '\\\\"', as.character(x)), '"', collapse = ","), "]")
xml_esc <- function(s) {
    s <- gsub("&", "&amp;", as.character(s))
    s <- gsub("<", "&lt;", s); s <- gsub(">", "&gt;", s); s <- gsub('"', "&quot;", s)
    s
}

# Shared Plotly config for every chart: turns off the small Plotly logomark
# in the modebar (displaylogo) while keeping the camera/"Download plot as
# png" button, and lets charts resize with their container.
PLOTLY_CFG <- '{displaylogo:false,responsive:true}'

# Color themes offered for the two heatmaps. Switching the dropdown calls
# Plotly.restyle() on the live figure, so the chart the camera/"Download
# plot as png" button captures is always the currently-selected theme --
# there is no separate "export" copy to fall out of sync.
HEATMAP_THEMES <- c("RdYlGn", "Viridis", "Blues", "Reds", "Portland", "Picnic", "Jet", "Hot", "Greys", "YlOrRd")

heatmap_theme_picker_html <- function(id, default) {
    opts <- paste0(sprintf('<option value="%s"%s>%s</option>', HEATMAP_THEMES,
                            ifelse(HEATMAP_THEMES == default, " selected", ""), HEATMAP_THEMES), collapse = "")
    sprintf('<label class="theme-picker">Color theme: <select onchange="Plotly.restyle(\'%s\',{colorscale:[this.value]})">%s</select></label>', id, opts)
}

generate_aaa_html <- function(results, df = NULL) {
    apps <- results$appraisers
    n    <- length(apps)
    blocks <- character(0)
    scripts <- character(0)
    chart_id <- 0
    next_id <- function() { chart_id <<- chart_id + 1; paste0("chart_", chart_id) }

    # Between-appraiser heatmap (with per-cell value labels + theme picker)
    if (!is.null(results$between) && length(results$between) > 0) {
        m <- matrix(100, n, n, dimnames = list(apps, apps))
        for (key in names(results$between)) {
            parts <- strsplit(key, "_vs_")[[1]]
            m[parts[1], parts[2]] <- results$between[[key]]
            m[parts[2], parts[1]] <- results$between[[key]]
        }
        id <- next_id()
        z_rows <- apply(m, 1, function(r) js_num_arr(r))
        blocks <- c(blocks, sprintf(
            '<div class="card"><h3>Between-Appraiser Agreement Heatmap</h3>%s<div id="%s" class="plot"></div></div>',
            heatmap_theme_picker_html(id, "RdYlGn"), id))
        scripts <- c(scripts, sprintf(
            'Plotly.newPlot("%s",[{z:[%s],x:%s,y:%s,type:"heatmap",colorscale:"RdYlGn",zmin:0,zmax:100,texttemplate:"%%{z:.1f}",textfont:{size:11,color:"#222"}}],{margin:{t:20}},%s);',
            id, paste(z_rows, collapse = ","), js_str_arr(apps), js_str_arr(apps), PLOTLY_CFG))
    }

    # Within-appraiser bar
    if (!is.null(results$within) && length(results$within) > 0) {
        id <- next_id()
        blocks <- c(blocks, sprintf('<div class="card"><h3>Within-Appraiser Consistency</h3><div id="%s" class="plot"></div></div>', id))
        scripts <- c(scripts, sprintf(
            'Plotly.newPlot("%s",[{x:%s,y:%s,type:"bar",marker:{color:%s.map(v=>v>=90?"#2e7d32":v>=80?"#f9a825":"#c62828")}}],{yaxis:{range:[0,100],title:"%% Agreement"},margin:{t:20}},%s);',
            id, js_str_arr(names(results$within)), js_num_arr(unlist(results$within)), js_num_arr(unlist(results$within)), PLOTLY_CFG))
    }

    # Each appraiser vs Standard bar
    if (!is.null(results$vs_standard) && length(results$vs_standard) > 0) {
        id <- next_id()
        blocks <- c(blocks, sprintf('<div class="card"><h3>Each Appraiser vs. Standard</h3><div id="%s" class="plot"></div></div>', id))
        scripts <- c(scripts, sprintf(
            'Plotly.newPlot("%s",[{x:%s,y:%s,type:"bar",marker:{color:%s.map(v=>v>=90?"#2e7d32":v>=80?"#f9a825":"#c62828")}}],{yaxis:{range:[0,100],title:"%% Agreement"},margin:{t:20}},%s);',
            id, js_str_arr(names(results$vs_standard)), js_num_arr(unlist(results$vs_standard)), js_num_arr(unlist(results$vs_standard)), PLOTLY_CFG))
    }

    # Confusion matrices — one card per appraiser vs Standard
    if (!is.null(results$confusion) && length(results$confusion) > 0) {
        for (nm in names(results$confusion)) {
            tab <- results$confusion[[nm]]
            id <- next_id()
            z_rows <- apply(as.matrix(tab), 1, function(r) js_num_arr(as.numeric(r)))
            blocks <- c(blocks, sprintf(
                '<div class="card"><h3>Confusion Matrix: %s</h3>%s<div id="%s" class="plot"></div></div>',
                nm, heatmap_theme_picker_html(id, "Blues"), id))
            scripts <- c(scripts, sprintf(
                'Plotly.newPlot("%s",[{z:[%s],x:%s,y:%s,type:"heatmap",colorscale:"Blues",texttemplate:"%%{z:.0f}",textfont:{size:11,color:"#222"}}],{xaxis:{title:"Appraiser"},yaxis:{title:"Standard"},margin:{t:20}},%s);',
                id, paste(z_rows, collapse = ","), js_str_arr(colnames(tab)), js_str_arr(rownames(tab)), PLOTLY_CFG))
        }
    }

    # Category distribution pie (Standard) -- real counts from the data,
    # not the category labels themselves (that placeholder produced a blank
    # chart, since Plotly's numeric "values" can't be a list of strings).
    if (!is.null(results$std_col) && !is.null(results$categories) && !is.null(df) && results$std_col %in% names(df)) {
        std_vals <- as.character(df[[results$std_col]])
        counts <- vapply(results$categories, function(cat) sum(std_vals == cat, na.rm = TRUE), numeric(1))
        if (sum(counts) > 0) {
            id <- next_id()
            blocks <- c(blocks, sprintf('<div class="card"><h3>Category Distribution (Standard)</h3><div id="%s" class="plot"></div></div>', id))
            scripts <- c(scripts, sprintf(
                'Plotly.newPlot("%s",[{labels:%s,values:%s,type:"pie",textinfo:"label+value+percent"}],{margin:{t:20}},%s);',
                id, js_str_arr(results$categories), js_num_arr(counts), PLOTLY_CFG))
        }
    }

    # Fleiss per-category kappa
    if (!is.null(results$fleiss) && !is.null(results$fleiss$kappa_j)) {
        kj <- results$fleiss$kappa_j; cats <- results$fleiss$categories
        valid <- !is.na(kj)
        if (any(valid)) {
            id <- next_id()
            blocks <- c(blocks, sprintf('<div class="card"><h3>Fleiss\' Kappa by Category</h3><div id="%s" class="plot"></div></div>', id))
            scripts <- c(scripts, sprintf(
                'Plotly.newPlot("%s",[{x:%s,y:%s,type:"bar",marker:{color:"#1976d2"}}],{yaxis:{title:"Kappa"},margin:{t:20}},%s);',
                id, js_str_arr(cats[valid]), js_num_arr(kj[valid]), PLOTLY_CFG))
        }
    }

    # Effectiveness / Miss Rate / False Alarm Rate grouped bar
    if (!is.null(results$effectiveness) && nrow(results$effectiveness) > 0) {
        eff <- results$effectiveness
        id <- next_id()
        blocks <- c(blocks, sprintf('<div class="card"><h3>Effectiveness, Miss Rate &amp; False Alarm Rate by Category</h3><div id="%s" class="plot"></div></div>', id))
        scripts <- c(scripts, sprintf(
            'Plotly.newPlot("%s",[{x:%s,y:%s,name:"Effectiveness %%",type:"bar"},{x:%s,y:%s,name:"Miss Rate %%",type:"bar"},{x:%s,y:%s,name:"False Alarm Rate %%",type:"bar"}],{barmode:"group",yaxis:{range:[0,100],title:"%%"},margin:{t:20}},%s);',
            id, js_str_arr(eff$Category), js_num_arr(eff$Effectiveness),
            js_str_arr(eff$Category), js_num_arr(eff$MissRate),
            js_str_arr(eff$Category), js_num_arr(eff$FalseAlarmRate), PLOTLY_CFG))
    }

    # Sample-order run chart -- flags each sample green (all readings agreed)
    # or red (disagreement) in original run order, so a reviewer can spot
    # clustering (e.g. fatigue/drift late in a long run, or a bad batch of
    # samples) that the single overall percentage above cannot reveal.
    if (!is.null(results$agree_by_sample) && !is.null(results$sample_labels) &&
        length(results$agree_by_sample) == length(results$sample_labels)) {
        flags <- results$agree_by_sample
        id <- next_id()
        blocks <- c(blocks, sprintf(
            '<div class="card"><h3>Sample-by-Sample Agreement (Run Order)</h3><div class="legend-note" style="font-size:12px;color:#666;margin-bottom:6px"><span style="display:inline-block;width:10px;height:10px;background:#2e7d32;margin-right:4px;border-radius:2px"></span>Agree&nbsp;&nbsp;<span style="display:inline-block;width:10px;height:10px;background:#c62828;margin-right:4px;border-radius:2px"></span>Disagree&nbsp;&nbsp;<span style="display:inline-block;width:10px;height:10px;background:#bdbdbd;margin-right:4px;border-radius:2px"></span>No data</div><div id="%s" class="plot"></div></div>',
            id))
        # Bar height is always 1 -- color (not height) carries agree/disagree/missing,
        # so a disagreement renders as a visible red bar instead of an invisible gap.
        scripts <- c(scripts, sprintf(
            'Plotly.newPlot("%s",[{x:%s,y:%s.map(v=>1),type:"bar",marker:{color:%s.map(v=>v===1?"#2e7d32":v===0?"#c62828":"#bdbdbd")},text:%s.map(v=>v===1?"Agree":v===0?"Disagree":"No data"),hoverinfo:"x+text"}],{yaxis:{visible:false,range:[0,1.15]},xaxis:{title:"Sample (run order)"},margin:{t:20},showlegend:false},%s);',
            id, js_str_arr(results$sample_labels), js_num_arr(flags), js_num_arr(flags), js_num_arr(flags), PLOTLY_CFG))
    }

    # Kappa forest plot -- every pairwise Cohen's kappa (with a proper
    # asymptotic 95% CI derived from Po/Pe/n, distinct from the H0:kappa=0
    # significance-test SE used elsewhere) plus Fleiss' overall kappa,
    # plotted against Landis & Koch (1977) interpretation-zone background bands.
    if (isTRUE(results$ch_kappaforest) && !is.null(results$kappa) && length(results$kappa) > 0) {
        pairs <- names(results$kappa)
        vals  <- unname(unlist(results$kappa[pairs]))
        ns    <- unname(unlist(results$kappa_n[pairs]))
        pes   <- unname(unlist(results$kappa_pe[pairs]))
        po    <- vals * (1 - pes) + pes
        se    <- ifelse(!is.na(ns) & !is.na(pes) & ns > 0 & pes < 1,
                         sqrt(pmax(po * (1 - po), 0) / (ns * (1 - pes)^2)), NA_real_)
        labels <- gsub("_vs_", " vs ", pairs)
        ci_lo  <- vals - 1.96 * se
        ci_hi  <- vals + 1.96 * se

        if (isTRUE(results$opt_fleiss) && !is.null(results$fleiss) && !is.na(results$fleiss$kappa)) {
            fk    <- results$fleiss
            se_fk <- if (!is.na(fk$se0)) fk$se0 else NA_real_
            labels <- c(labels, "Fleiss' Kappa (Overall)")
            vals   <- c(vals, fk$kappa)
            ci_lo  <- c(ci_lo, fk$kappa - 1.96 * se_fk)
            ci_hi  <- c(ci_hi, fk$kappa + 1.96 * se_fk)
        }

        err_plus  <- ci_hi - vals
        err_minus <- vals - ci_lo
        id <- next_id()
        zones <- paste0(
            '{type:"rect",xref:"x",yref:"paper",x0:-1,x1:0,y0:0,y1:1,fillcolor:"#ffebee",line:{width:0},layer:"below"},',
            '{type:"rect",xref:"x",yref:"paper",x0:0,x1:0.20,y0:0,y1:1,fillcolor:"#fff3e0",line:{width:0},layer:"below"},',
            '{type:"rect",xref:"x",yref:"paper",x0:0.20,x1:0.40,y0:0,y1:1,fillcolor:"#fffde7",line:{width:0},layer:"below"},',
            '{type:"rect",xref:"x",yref:"paper",x0:0.40,x1:0.60,y0:0,y1:1,fillcolor:"#f1f8e9",line:{width:0},layer:"below"},',
            '{type:"rect",xref:"x",yref:"paper",x0:0.60,x1:0.80,y0:0,y1:1,fillcolor:"#e8f5e9",line:{width:0},layer:"below"},',
            '{type:"rect",xref:"x",yref:"paper",x0:0.80,x1:1,y0:0,y1:1,fillcolor:"#c8e6c9",line:{width:0},layer:"below"}')
        blocks <- c(blocks, sprintf(
            '<div class="card"><h3>Kappa Forest Plot</h3><div class="legend-note" style="font-size:12px;color:#666;margin-bottom:6px">Background bands show Landis &amp; Koch interpretation zones (Poor / Slight / Fair / Moderate / Substantial / Almost Perfect). Error bars are 95%% CI.</div><div id="%s" class="plot"></div></div>',
            id))
        scripts <- c(scripts, sprintf(
            'Plotly.newPlot("%s",[{x:%s,y:%s,type:"scatter",mode:"markers",marker:{size:10,color:"#1976d2"},error_x:{type:"data",symmetric:false,array:%s,arrayminus:%s,color:"#1976d2"}}],{xaxis:{title:"Kappa",range:[-1,1]},yaxis:{automargin:true},shapes:[%s],margin:{t:20,l:170}},%s);',
            id, js_num_arr(vals), js_str_arr(labels), js_num_arr(err_plus), js_num_arr(err_minus), zones, PLOTLY_CFG))
    }

    # Rating-flow/Sankey chart removed at the user's request -- neither the
    # multi-column Plotly version nor the two-column SVG alluvial version
    # was useful once there were more than a couple of appraisers/stages.
    # results$ch_sankey is still set above (harmless) so SANKEY=YES in
    # existing pasted syntax keeps running without error; it simply no
    # longer produces any chart.

    # Mosaic plot (HTML/Plotly version) removed at the user's request, same
    # reasoning and same as the base-R version above: ch_mosaic is still set
    # on results above (harmless) so MOSAIC=YES in existing pasted syntax
    # keeps running without error; it simply no longer produces any chart.

    # Appraiser disagreement network -- circular node layout (nodes =
    # appraisers; small N makes a force-directed layout unnecessary), with
    # edge width/color encoding the pairwise disagreement rate
    # (100 - between-appraiser agreement %).
    if (isTRUE(results$ch_disagreenetwork) && !is.null(results$between) && length(results$between) > 0 && n >= 2) {
        angle <- 2 * pi * (seq_len(n) - 1) / n
        node_x <- setNames(cos(angle), apps); node_y <- setNames(sin(angle), apps)
        edge_traces <- character(0)
        for (key in names(results$between)) {
            parts <- strsplit(key, "_vs_")[[1]]
            a1 <- parts[1]; a2 <- parts[2]
            if (!(a1 %in% apps) || !(a2 %in% apps)) next
            disagree <- 100 - results$between[[key]]
            col <- disagree_gradient_color(disagree)
            w <- max(1, disagree / 5)
            edge_traces <- c(edge_traces, sprintf(
                '{x:[%.4f,%.4f],y:[%.4f,%.4f],mode:"lines",line:{width:%.2f,color:"%s"},hoverinfo:"text",text:"%s",showlegend:false}',
                node_x[[a1]], node_x[[a2]], node_y[[a1]], node_y[[a2]], w, col,
                gsub('"', "'", sprintf("%s vs %s: %.1f%% disagreement", a1, a2, disagree))))
        }
        if (length(edge_traces) > 0) {
            # Per-node textposition derived from angle so Plotly anchors each
            # label AWAY from the node circle (no overlap regardless of N).
            textpos_vals <- vapply((angle %% (2 * pi)) * 180 / pi, function(deg) {
                if      (deg <  45 || deg >= 315) "middle right"
                else if (deg < 135)               "top center"
                else if (deg < 225)               "middle left"
                else                              "bottom center"
            }, character(1))
            node_trace <- sprintf(
                '{x:%s,y:%s,mode:"markers+text",text:%s,textposition:%s,textfont:{size:13,color:"#1a237e",family:"Arial Black,Arial,sans-serif"},marker:{size:24,color:"#1976d2"},hoverinfo:"text",showlegend:false}',
                js_num_arr(unname(node_x[apps])), js_num_arr(unname(node_y[apps])),
                js_str_arr(apps), js_str_arr(textpos_vals))
            id <- next_id()
            blocks <- c(blocks, sprintf(
                '<div class="card"><h3>Appraiser Disagreement Network</h3><div class="legend-note" style="font-size:12px;color:#666;margin-bottom:6px">Edge color/width encodes pairwise disagreement rate on a continuous green&#8594;amber&#8594;red gradient (0%% to 100%%); hover an edge for its exact value.</div><div id="%s" class="plot"></div></div>',
                id))
            scripts <- c(scripts, sprintf(
                'Plotly.newPlot("%s",[%s,%s],{xaxis:{visible:false,range:[-1.6,1.6]},yaxis:{visible:false,range:[-1.6,1.6],scaleanchor:"x"},margin:{t:20},showlegend:false},%s);',
                id, paste(edge_traces, collapse = ","), node_trace, PLOTLY_CFG))
        }
    }

    if (length(blocks) == 0) return(invisible(NULL))

    metrics_lines <- c(sprintf("Overall Agreement: %.1f%%", results$overall_pct))
    if (!is.null(results$kendall_w) && !is.na(results$kendall_w)) metrics_lines <- c(metrics_lines, sprintf("Kendall's W: %.3f", results$kendall_w))
    if (!is.null(results$kappa) && length(results$kappa) > 0) metrics_lines <- c(metrics_lines, sprintf("Avg Cohen's Kappa: %.3f", mean(unlist(results$kappa))))
    if (isTRUE(results$opt_fleiss) && !is.null(results$fleiss) && !is.na(results$fleiss$kappa)) metrics_lines <- c(metrics_lines, sprintf("Fleiss' Kappa: %.3f (p=%s)", results$fleiss$kappa, if (!is.na(results$fleiss$p_value)) sprintf("%.4f", results$fleiss$p_value) else "n/a"))

    # ── Executive summary banner ───────────────────────────────────────────
    # AIAG MSA-4 conventional thresholds for attribute agreement studies:
    # >=90% acceptable, 80-90% marginal (use with caution), <80% unacceptable.
    pct <- results$overall_pct
    verdict <- if (pct >= 90) "ACCEPTABLE" else if (pct >= 80) "MARGINAL" else "UNACCEPTABLE"
    verdict_color <- if (pct >= 90) "#2e7d32" else if (pct >= 80) "#f9a825" else "#c62828"
    verdict_note <- if (pct >= 90) "Measurement system agreement meets the AIAG MSA-4 acceptable threshold (>=90%)."
                    else if (pct >= 80) "Measurement system is marginal (80-90%): may be acceptable depending on application; investigate sources of disagreement."
                    else "Measurement system agreement is below the AIAG MSA-4 acceptable threshold (<80%); review appraiser training, operational definitions, or instrument resolution."
    n_dis <- length(results$disagreements)

    study_bits <- character(0)
    if (!is.null(results$study_name) && nzchar(results$study_name)) study_bits <- c(study_bits, sprintf("<strong>%s</strong>", results$study_name))
    if (!is.null(results$study_by)   && nzchar(results$study_by))   study_bits <- c(study_bits, sprintf("Conducted by %s", results$study_by))
    if (!is.null(results$study_date) && nzchar(results$study_date)) study_bits <- c(study_bits, results$study_date)
    study_line <- if (length(study_bits) > 0) paste0('<p class="study-line">', paste(study_bits, collapse = " &middot; "), '</p>') else ""

    summary_html <- sprintf(
        '<div class="summary-banner" style="border-left:6px solid %s">
           <div class="verdict" style="color:%s">%s</div>
           <p class="verdict-note">%s</p>
           %s
           <div class="summary-stats">
             <div class="stat"><span class="stat-num">%.1f%%</span><span class="stat-label">Overall Agreement</span></div>
             <div class="stat"><span class="stat-num">%d</span><span class="stat-label">Samples</span></div>
             <div class="stat"><span class="stat-num">%d</span><span class="stat-label">Appraisers</span></div>
             <div class="stat"><span class="stat-num">%d</span><span class="stat-label">Disagreements</span></div>
           </div>
         </div>',
        verdict_color, verdict_color, verdict, verdict_note, study_line, pct, results$n_samples, n, n_dis)

    html <- paste0(
        '<!DOCTYPE html><html lang="en"><head><meta charset="utf-8">',
        '<title>Attribute Agreement Analysis Report</title>',
        '<script src="https://cdn.plot.ly/plotly-2.27.0.min.js"></script>',
        '<style>body{font-family:Segoe UI,Arial,sans-serif;background:#f4f6f8;margin:0;padding:24px;color:#222}',
        'h1{margin:0 0 4px}.sub{color:#666;margin:0 0 20px}',
        '.metrics{background:#fff;border-radius:8px;padding:16px;margin-bottom:20px;box-shadow:0 1px 3px rgba(0,0,0,.1);font-family:monospace}',
        '.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(420px,1fr));gap:18px}',
        '.card{background:#fff;border-radius:8px;padding:16px;box-shadow:0 1px 3px rgba(0,0,0,.1)}',
        '.card h3{margin:0 0 8px;font-size:15px;color:#333}.plot{width:100%;height:360px}',
        '.theme-picker{display:block;font-size:12px;color:#666;margin-bottom:6px}',
        '.theme-picker select{font-size:12px;margin-left:4px}',
        '.summary-banner{background:#fff;border-radius:8px;padding:18px 20px;margin-bottom:20px;box-shadow:0 1px 3px rgba(0,0,0,.1)}',
        '.summary-banner .verdict{font-size:22px;font-weight:700;letter-spacing:.5px}',
        '.summary-banner .verdict-note{margin:4px 0 12px;color:#555}',
        '.summary-banner .study-line{margin:0 0 12px;color:#444;font-size:13px}',
        '.summary-stats{display:flex;gap:28px;flex-wrap:wrap}',
        '.summary-stats .stat{display:flex;flex-direction:column}',
        '.summary-stats .stat-num{font-size:22px;font-weight:700;color:#222}',
        '.summary-stats .stat-label{font-size:12px;color:#777}',
        '</style></head><body>',
        '<h1>Attribute Agreement Analysis</h1>',
        sprintf('<p class="sub">%d samples &middot; %d appraisers &middot; generated %s</p>', results$n_samples, n, format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        summary_html,
        '<div class="metrics">', paste(metrics_lines, collapse = "<br>"), '</div>',
        '<div class="grid">', paste(blocks, collapse = ""), '</div>',
        '<script>', paste(scripts, collapse = "\n"), '</script>',
        '</body></html>'
    )

    ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
    fn <- paste0("AttributeAgreementAnalysis_Report_", ts, ".html")

    # Cross-platform writable-directory search (Windows / macOS / Linux).
    # path.expand("~") resolves correctly on all three platforms even when
    # HOME/USERPROFILE env vars are unset or differently named.
    home <- tryCatch(path.expand("~"), error = function(e) "")
    is_writable_dir <- function(d) isTRUE(nzchar(d)) && dir.exists(d) && file.access(d, mode = 2) == 0
    candidates <- c(
        file.path(Sys.getenv("USERPROFILE"), "Documents"),  # Windows
        file.path(home, "Documents"),                        # macOS / Linux
        file.path(Sys.getenv("USERPROFILE"), "Desktop"),
        file.path(home, "Desktop"),
        Sys.getenv("USERPROFILE"),
        home,
        tempdir()
    )
    candidates <- candidates[nzchar(candidates)]
    writable <- candidates[sapply(candidates, is_writable_dir)]
    sd <- if (length(writable) > 0) writable[1] else tempdir()

    # Keep reports tidy in their own subfolder; fall back to the bare
    # directory if the subfolder can't be created for any reason.
    report_dir <- file.path(sd, "SPSS_AAA_Reports")
    made_subdir <- tryCatch({
        if (!dir.exists(report_dir)) dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)
        is_writable_dir(report_dir)
    }, error = function(e) FALSE)
    if (!isTRUE(made_subdir)) report_dir <- sd

    out_file <- file.path(report_dir, fn)

    tryCatch({
        writeLines(html, out_file, useBytes = FALSE)
        # Report the saved location prominently in the Output Viewer: both as
        # a pivot table (always visible/exportable) and as a console message.
        spss_pivot_table(
            "Interactive HTML Report (Plotly)",
            rows = "Saved to", cols = "File location",
            data = matrix(out_file, nrow = 1, ncol = 1),
            caption = "This self-contained interactive report was opened automatically in your default web browser. You can reopen it at any time from the path shown above."
        )
        message(sprintf("Interactive HTML report saved to: %s", out_file))
        tryCatch(browseURL(out_file), error = function(e) NULL)
    }, error = function(e) warning(paste("HTML report write failed:", conditionMessage(e))))

    invisible(out_file)
}


# ══════════════════════════════════════════════════════════════════════════════
# WORKSHEET GENERATION (Worksheet Designer tab equivalent)
# ══════════════════════════════════════════════════════════════════════════════

aaa_generate_worksheet <- function(n_appraisers, n_samples, n_trials, categories,
                                    include_std, create_vars, appraisernames = NULL) {
    cols <- c("Sample")
    if (include_std) cols <- c(cols, "Standard")

    # Use the user's APPRAISERNAMES (if supplied and the count matches the
    # number of appraisers being generated) as the column-name stems instead
    # of generic App1/App2/... -- e.g. APPRAISERNAMES='GFD,HGDF,j' with
    # APPRAISERS=3 produces GFD_T1, HGDF_T1, j_T1, ... Falls back to generic
    # names if the count doesn't match or none was supplied.
    app_stems <- NULL
    if (notempty(appraisernames)) {
        nm <- trimws(strsplit(strip_quotes(appraisernames), ",")[[1]])
        nm <- nm[nzchar(nm)]
        if (length(nm) == n_appraisers) app_stems <- nm
    }
    if (is.null(app_stems)) app_stems <- sprintf("App%d", seq_len(n_appraisers))
    # Sanitize into valid SPSS variable-name stems: letters/digits/underscore
    # only, must start with a letter.
    app_stems <- gsub("[^A-Za-z0-9_]", "_", app_stems)
    app_stems <- ifelse(grepl("^[A-Za-z]", app_stems), app_stems, paste0("V", app_stems))
    # Guard against duplicate stems after sanitizing (e.g. two names that
    # differ only in punctuation) -- make.unique keeps them valid variables.
    app_stems <- make.unique(app_stems, sep = "_")

    for (a in seq_len(n_appraisers)) for (t in seq_len(n_trials))
        cols <- c(cols, sprintf("%s_T%d", app_stems[a], t))

    df <- as.data.frame(matrix("", nrow = n_samples, ncol = length(cols)), stringsAsFactors = FALSE)
    names(df) <- cols
    df$Sample <- as.character(seq_len(n_samples))

    caption <- sprintf("Worksheet: %d appraisers x %d trials, %d samples. Categories: %s",
                        n_appraisers, n_trials, n_samples, paste(categories, collapse = ", "))

    # Preview table gets its own complete StartProcedure/EndProcedure cycle.
    # Critically, this procedure is fully closed *before* any of the
    # dataset-creation API calls below run -- see the long comment at the
    # GENERATE call site for why that separation matters.
    StartProcedure("Attribute Agreement Analysis - Generate Worksheet", "STATS_ATTRIBUTE_AGREEMENT")
    spss_pivot_table("Attribute Agreement Analysis - Generated Worksheet",
                      rows = df$Sample, cols = cols, data = as.matrix(df), corner = "Row",
                      caption = caption)
    spsspkg.EndProcedure()

    if (isTRUE(create_vars)) {
        # Writes the generated worksheet into a brand-new active dataset,
        # mirroring -- call for call -- the proven gage_generate_worksheet()
        # in this project's own STATS_GAGE_RNR extension (which ships the
        # identical worksheet-generation feature and is confirmed working
        # against a live SPSS session): CreateSPSSDictionary takes one plain
        # vector per variable (varName, varLabel, varType, varFormat,
        # varMeasurementLevel) -- not separate name/type arguments -- then
        # SetDictionaryToSPSS declares the new dataset, SetDataToSPSS writes
        # the case data, and EndDataStep closes out the data step. The Gage
        # reference does NOT call spssdictionary.SetActive() -- an earlier
        # version of this code added that extra call, which isn't part of
        # the proven sequence and is removed here to match exactly. There is
        # also no spssdictionary.StartDataStep()/spssdata.StartDataStep() in
        # this API (an even earlier version incorrectly called those, which
        # don't exist and is why CREATEVARS=YES originally failed silently).
        # Every column is created as a string so blank cells are legal and
        # appraisers can type ratings straight in. The dataset name is
        # de-duplicated against already-open datasets so re-running GENERATE
        # doesn't collide with a worksheet from an earlier run.
        existing_ds <- tryCatch(spssdata.GetDataSetList(), error = function(e) character(0))
        base_name <- "AAAWorksheet"
        new_dataset_name <- base_name
        suffix <- 1L
        while (tolower(new_dataset_name) %in% tolower(existing_ds)) {
            suffix <- suffix + 1L
            new_dataset_name <- paste0(base_name, suffix)
        }

        result <- tryCatch({
            var_specs <- lapply(cols, function(nm) c(nm, nm, 32, "A32", "nominal"))
            varDict <- do.call(spssdictionary.CreateSPSSDictionary, var_specs)
            spssdictionary.SetDictionaryToSPSS(new_dataset_name, varDict)
            spssdata.SetDataToSPSS(new_dataset_name, df)
            spssdictionary.EndDataStep()
            TRUE
        }, error = function(e) {
            tryCatch(spssdictionary.EndDataStep(), error = function(e2) NULL)
            conditionMessage(e)
        })

        # This confirmation note gets its own StartProcedure/EndProcedure
        # cycle, opened only *after* the dataset-creation calls above have
        # fully completed (spssdictionary.EndDataStep() already ran). The
        # active-dataset switch the SPSS R plugin performs as part of
        # SetDataToSPSS/EndDataStep needs to happen with no procedure open
        # at all -- doing the TextBlock display in a fresh cycle here, never
        # straddling the dataset-creation calls, is what lets that switch
        # actually take effect instead of silently being swallowed.
        StartProcedure("Attribute Agreement Analysis - Generate Worksheet", "STATS_ATTRIBUTE_AGREEMENT")
        if (isTRUE(result)) {
            tryCatch(spsspkg.TextBlock(
                "AAAWorksheet Note",
                sprintf("CREATEVARS=YES: a new active dataset named \"%s\" was created with the generated worksheet (%d rows x %d columns: %s). Enter ratings directly into it.",
                        new_dataset_name, n_samples, length(cols), paste(cols, collapse = ", "))
            ), error = function(e) NULL)
        } else {
            tryCatch(spsspkg.TextBlock(
                "AAAWorksheet Note",
                sprintf("CREATEVARS=YES was requested, but creating the new active dataset failed (%s). The worksheet table above is still available -- copy it out via the Viewer's Export, or re-run without CREATEVARS and build the dataset manually from that table.",
                        result)
            ), error = function(e) NULL)
        }
        spsspkg.EndProcedure()
    }
    invisible(NULL)
}
