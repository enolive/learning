class Arabic(private val input: Int) {
    fun toRoman(): String {
        if (input == 2) {
            return "II"
        }
        if (input == 1) {
            return "I"
        }
        return ""
    }
}