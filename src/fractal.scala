import scala.util.Try
object Main {
    def main(args: Array[String]): Unit = {
        printLog()
        var input = ""
        while(input != "q") {
            input = scala.io.StdIn.readLine()
            val n = isInteger(input)
            if(n >= 1 && n <= 8) {
                println("----------------------------------------------------------------")
                println(makeStar(multipleThree(n)))
            }
        }
    }

    def isInteger(input: String): Int = {
        if(Try(input.toInt).isSuccess) input.toInt
        else -1
    }

    def multipleThree(n: Int): Int = {
        def loop(i: Int, acc: Int): Int = {
            if(i <= 0) acc
            else loop(i - 1, acc * 3)
        }
        loop(n, 1)
    }

    def makeStar(n: Int): String = {
        def loop(i: Int, arr: Array[String]): Array[String] = {
            if(i == 3) arr
            else loop(i / 3, multipleStr(arr))
        }
        loop(n, Array("***", "* *", "***")).mkString("\n")
    }

    def multipleStr(arr: Array[String]): Array[String] = {
        val l = arr(0).length()
        val side = arr.map(i => i + " " * l + i)
        val upDown = arr.map(_ * 3)
        Array.concat(upDown, side, upDown)
    }

    def printLog(): Unit = {
        println(" _______  ______    _______  _______  _______  _______  ___     ")
        println("|       ||    _ |  |   _   ||       ||       ||   _   ||   |    ")
        println("|    ___||   | ||  |  |_|  ||       ||_     _||  |_|  ||   |    ")
        println("|   |___ |   |_||_ |       ||       |  |   |  |       ||   |    ")
        println("|    ___||    __  ||       ||      _|  |   |  |       ||   |___ ")
        println("|   |    |   |  | ||   _   ||     |_   |   |  |   _   ||       |")
        println("|___|    |___|  |_||__| |__||_______|  |___|  |__| |__||_______|")
        println()
        println("----------------------------------------------------------------")
        println("Insert the value of exponent (1 ~ 8)")
        println("----------------------------------------------------------------")
    }
}
