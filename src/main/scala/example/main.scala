import scala.io.Source

case class CSVFile(filename: String)

object CSVReader {

    def main(args: Array[String]): Unit = {
        val csvFile = CSVFile("finance.csv")
        getLines(csvFile)
    }

    def readFile(filename: String): List[String] =
        Source.fromFile(filename).getLines.toList

    def getLines(file: CSVFile): Unit = {
        val fileContents: List[String] = readFile(file.filename);
        val nrows: Int = fileContents.length
        val ncols: Int = fileContents.length
        val rows = Array.ofDim[String](nrows, ncols)
        var tempList:List[String] = List()

        for ((line, count) <- readFile(file.filename).zipWithIndex)  {
            rows(count) = line.split(",").map(_.trim)
        }

        for (i <- 0 until nrows) {
            for (j <- 0 until ncols) {
                tempList = rows(i)(j) :: tempList
            }
            println(tempList)
        }
    }
}
