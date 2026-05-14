#' format character vector

#' @description Convert lowercase to uppercase, full-width characters to
#'   half-width, and drop meaningless characters according to language.
#' @param x a character vector
#' @param lang a string. The default value is "zh_CN" (Chinese) or
#'   "en_US" (English, default). "zh_CN" keeps CJK ideographs.
#' @examples
#' x <- c("\uff22  ", "\uff23\uff10\uff11\uff12,,,\uff13", "cabc=dd")
#' fmt_lang(x)
#' @export
fmt_lang <- function(x, lang = "en_US") {
    DBC <- paste0(
        "\uff10\uff11\uff12\uff13\uff14\uff15\uff16\uff17\uff18\uff19",
        "\uff21\uff22\uff23\uff24\uff25\uff26\uff27\uff28\uff29\uff2a",
        "\uff2b\uff2c\uff2d\uff2e\uff2f\uff30\uff31\uff32\uff33\uff34",
        "\uff35\uff36\uff37\uff38\uff39\uff3a",
        "\uff41\uff42\uff43\uff44\uff45\uff46\uff47\uff48\uff49\uff4a",
        "\uff4b\uff4c\uff4d\uff4e\uff4f\uff50\uff51\uff52\uff53\uff54",
        "\uff55\uff56\uff57\uff58\uff59\uff5a"
    )
    SBC <- "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

    x <- stringr::str_to_upper(x)
    x <- chartr(DBC, SBC, x)
    if (lang == "zh_CN") {
        x <- stringr::str_replace_all(x, "[^0-9A-Za-z\\u4E00-\\u9FFF]", "")
    } else if (lang == "en_US") {
        x <- stringr::str_replace_all(x, "[^0-9A-Za-z]", "")
    }
    x
}
