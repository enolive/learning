object Logger {
    def log(f: (() => String)): Unit = {
        println(f())
    }

    def log(str: String): Unit = {
        println(str)
    }

}
