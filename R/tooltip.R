#' Create a bootstrap tooltip
#' Copied from RLumShiny package
#' Create bootstrap tooltips for any HTML element to be used in shiny applications.
#'
#' @param refId [`character`] (**required**):
#' id of the element the tooltip is to be attached to.
#'
#' @param text [`character`] (**required**):
#' Text to be displayed in the tooltip.
#'
#' @param attr [`character`] (*optional*):
#' Attach tooltip to all elements with attribute `attr='refId'`.
#'
#' @param animation [`logical`] (*with default*):
#' Apply a CSS fade transition to the tooltip.
#'
#' @param delay [`numeric`] (*with default*):
#' Delay showing and hiding the tooltip (ms).
#'
#' @param html [`logical`] (*with default*):
#' Insert HTML into the tooltip.
#'
#' @param placement [`character`] (*with default*):
#' How to position the tooltip - `top` | `bottom` | `left` | `right` | `auto`.
#' When 'auto' is specified, it will dynamically reorient the tooltip.
#' For example, if placement is 'auto left', the tooltip will display to the
#' left when possible, otherwise it will display right.
#'
#' @param trigger [`character`] (*with default*):
#' How tooltip is triggered - `click` | `hover` | `focus` | `manual`.
#' You may pass multiple triggers; separate them with a space.
#'
#' @import shiny
#'
tooltip <- function(
    refId,
    text,
    attr = NULL,
    animation = TRUE,
    delay = 100,
    html = TRUE,
    placement = 'auto',
    trigger = 'hover') {

  if (is.null(attr))
    el <- sprintf("'#%s'", refId)
  else
    el <- sprintf("\"[%s='%s']\"", attr, refId)

  tagList(
    tags$head(
      tags$script(
        HTML(
          sprintf("$(window).load(function(){ $(%s).tooltip({ html: %s,
                  trigger: '%s', title: '%s', animation: %s, delay:
                  {'show': %i, 'hide': %i}, placement: '%s' }); })",
                  el, tolower(html), trigger, text, tolower(animation), delay, delay, placement)
        )
      )
    )
  )
}
