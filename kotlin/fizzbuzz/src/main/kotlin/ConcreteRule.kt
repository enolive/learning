class ConcreteRule(private val denominator: Int, override val result: String) : Rule {
    override fun appliesTo(input: Int): Boolean = input % denominator == 0
}