class Roman(private val input: String) {
    fun toArabic(): Int {
        var conversion = Conversion(input)
        conversion = conversion.apply("V", 5)
        conversion = conversion.apply("I", 1)
        return conversion.sum
    }
}