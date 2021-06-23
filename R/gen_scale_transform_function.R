##' 建立两个坐标轴之间的映射，方便在 `ggplot` 上做双轴图
##'
##' @title 生成从一个坐标轴到另一个坐标轴的转化函数
##' @param x 两个数字组成的向量，且必须从小到大排列
##' @param y 两个数字组成的向量，且必须从小到大排列
##' @return 返回两个函数构成的列表
##' @author Liubianshi
##' @export
gen_scale_transform_function <- function(x, y) {
    stopifnot(all(length(x) == 2L, length(y) == 2L))
    stopifnot(all(x[2] > x[1], y[2] > y[1]))
    multi <- (y[2] - y[1]) / (x[2] - x[1])
    list(
         forword  = function(z) (z - x[1]) * multi + y[1],
         backword = function(z) (z - y[1]) / multi + x[1]
    )
}
