class Rule(private val denominator: Int, val result: String) {
    fun appliesTo(input: Int) : Boolean = input % denominator == 0
}