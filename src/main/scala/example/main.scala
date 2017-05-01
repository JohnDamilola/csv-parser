import scala.io.Source

case class CSVFile(filename: String)

object CSVReader {

    def main(args: Array[String]): Unit = {
        val csvFile = CSVFile("finance.csv")
        displayCSV(csvFile)
    }

    def readFile(filename: String): List[String] =
        Source.fromFile(filename).getLines.toList

    def displayCSV(file: CSVFile): Unit = {
        val fileContents: List[String] = readFile(file.filename)
        val nrows: Int = fileContents.length
        val ncols: Int = fileContents.length
        val rows = Array.ofDim[String](nrows, ncols)

        for ((line, count) <- readFile(file.filename).zipWithIndex)  {
            rows(count) = line.split(",").map(_.trim)
        }

        for (i <- 0 until nrows) {
            var tempList:List[String] = List()
            for (j <- 0 until ncols) {
                val current:List[String] = List(rows(i)(j) + "\t\t")
                tempList = tempList ::: current
            }
            println(listToString(tempList))
        }
    }

    def listToString(list: List[String]): String = {
        var string: String = ""
        for (i <- list) {
            string += i
        }
        string
    }
}
