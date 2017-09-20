class Arabic(private val input: Int) {
    private val ruleChain = listOf(
            Rule(1000, "M"),
            Rule(900, "CM"),
            Rule(500, "D"),
            Rule(400, "CD"),
            Rule(100, "C"),
            Rule(90, "XC"),
            Rule(50, "L"),
            Rule(40, "XL"),
            Rule(10, "X"),
            Rule(9, "IX"),
            Rule(5, "V"),
            Rule(4, "IV"),
            Rule(1, "I")
    )

    fun toRoman() = ruleChain
            .fold(Conversion(input)) { acc, rule -> acc.apply(rule) }
            .result
}