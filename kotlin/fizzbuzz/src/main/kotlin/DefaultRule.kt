class DefaultRule constructor(input: Int) : Rule {
    override val result = input.toString()
    override fun appliesTo(input: Int): Boolean = true
}