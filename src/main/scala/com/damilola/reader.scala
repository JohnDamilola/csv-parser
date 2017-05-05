import scala.io.Source

case class CSVFile(filename: String)

object CSVReader {

    def main(args: Array[String]): Unit = {
        val csvFile = CSVFile("finance.csv")
        val data = readCSV(csvFile, 1)

        analyze(data, 2)
    }

    def readFile(filename: String): Vector[String] =
        Source.fromFile(filename).getLines.toVector

    def readCSV(file: CSVFile, display_flag: Int = 0): Array[Array[String]] = {
        val fileContents: Vector[String] = readFile(file.filename)
        val nrows: Int = fileContents.length
        val ncols: Int = fileContents.length
        val rows = Array.ofDim[String](nrows, ncols)

        for ((line, count) <- readFile(file.filename).zipWithIndex)  {
            rows(count) = line.split(",").map(_.trim)
        }

        if (display_flag == 1) {
            displayCSV(nrows, ncols, rows)
        }
        rows
    }

    def displayCSV(nrows: Int, ncols: Int, rows: Array[Array[String]]) = {
        for (i <- 0 until nrows) {
            var tempList:List[String] = List()
            for (j <- 0 until ncols) {
                val current:List[String] = List(rows(i)(j) + " | ")
                tempList = tempList ::: current
            }
            println(listToString(tempList))
        }
    }

    def listToString(vector: List[String]): String = {
        var string: String = ""
        for (i <- vector) {
            string += i
        }
        string
    }

    def sum(data: Array[Array[String]], col: Int): BigDecimal = {
        var rows_count = 0
        for ((line) <- data.zipWithIndex)  {
            rows_count += 1
        }
        var sum: BigDecimal = 0.0
        for (j <- 0 until rows_count) {
            val num = BigDecimal(data(j)(col))
            sum = sum + num
        }
        sum
    }

    def average(data: Array[Array[String]], col: Int): BigDecimal = {
        var rows_count = 0
        for ((line) <- data.zipWithIndex)  {
            rows_count += 1
        }
        sum(data, col) / rows_count
    }

    def min(data: Array[Array[String]], col: Int): BigDecimal = {
        var rows_count = 0
        for ((line) <- data.zipWithIndex)  {
            rows_count += 1
        }
        var min:BigDecimal = BigDecimal(data(0)(col))
        for (j <- 0 until rows_count) {
            val current = BigDecimal(data(j)(col))
            if (min > current)
                min = current
        }
        min
    }

    def max(data: Array[Array[String]], col: Int): BigDecimal = {
        var rows_count = 0
        for ((line) <- data.zipWithIndex)  {
            rows_count += 1
        }
        var min:BigDecimal = BigDecimal(data(0)(col))
        for (j <- 0 until rows_count) {
            val current = BigDecimal(data(j)(col))
            if (min < current)
                min = current
        }
        min
    }

    def analyze(data: Array[Array[String]], col: Int): Unit = {
        val ave = average(data, col)
        val total = sum(data, col)
        val minimum = min(data, col)
        val maximum = max(data, col)

        println(s"The sum total of column $col is $total")
        println(s"The mean average of column $col is $ave")
        println(s"The minimum value of column $col is $minimum")
        println(s"The maximum value of column $col is $maximum")

    }
}
