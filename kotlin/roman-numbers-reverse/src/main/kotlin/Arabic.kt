class Arabic(private val input: Int) {
    private val rules = listOf(
            Rule(10, "X"),
            Rule(5, "V"),
            Rule(1, "I")
    )

    fun toRoman(): String = rules
            .fold(Conversion(input)) { conversion, rule -> conversion.apply(rule) }
            .result

}

