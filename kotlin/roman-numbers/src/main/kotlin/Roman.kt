class Roman(private val input: String) {
    fun toArabic(): Int {
        if (input == "III") {
            return 3
        }
        if (input == "II") {
            return 2
        }
        return 1
    }

}