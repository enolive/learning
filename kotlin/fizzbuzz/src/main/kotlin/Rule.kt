interface Rule {
    fun appliesTo(input: Int): Boolean
    val result: String
}