#' format character vector

#' @description Delete non-ideographic text and standardize
#' @param x a character vector
#' @param lang a string. The default value is "zh_CN"
#' @examples
#' x <- c('Ｂ  ', 'Ｃ０１２,,,３', 'cabc=dd')
#' fmt_lang(x)
#' y <- c(x, "中文３４５６Ｚａｂ")
#' fmt_lang(y)
#' fmt_lang(y, lang = "zh_CN")
#' @export

fmt_lang <- function(x, lang = "en_US") {
    DBC<- paste("０１２３４５６７８９ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵ",
                "ＶＷＸＹＺａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ",
                sep = "")
    SBC=c("0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

    x <- stringr::str_to_upper(x)
    x <- chartr(DBC, SBC, x)
    if (lang == "zh_CH") {
        x <- stringr::str_replace_all(x, "[^0-9A-Za-z\\u4E00-\\u9FFFh]", "")
    } else if (lang == "en_US") {
        x <- stringr::str_replace_all(x, "[^0-9A-Za-z]", "")
    }
    x
}
