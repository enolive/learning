class Roman(private val input: String) {
    fun toArabic(): Int {
        var conversion = Conversion(input)
        val rules = listOf(
                Rule("X", 10),
                Rule("IX", 9),
                Rule("V", 5),
                Rule("IV", 4),
                Rule("I", 1)
        )
        rules.forEach { 
            conversion = conversion.apply(it.roman, it.arabic)
        }
        return conversion.sum
    }
}