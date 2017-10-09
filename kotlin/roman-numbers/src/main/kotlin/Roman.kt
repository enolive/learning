class Roman(private val input: String) {
    private val rules = listOf(
            Rule("L", 50),
            Rule("X", 10),
            Rule("IX", 9),
            Rule("V", 5),
            Rule("IV", 4),
            Rule("I", 1)
    )

    fun toArabic(): Int {
        var conversion = Conversion(input)
        rules.forEach { 
            conversion = conversion.apply(it)
        }
        conversion.failIfInputRemains()
        return conversion.sum
    }
}