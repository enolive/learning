class Roman(private val input: String) {
    fun toArabic(): Int {
        var conversion = Conversion(input)
        conversion = conversion.apply("IX", 9)
        conversion = conversion.apply("V", 5)
        conversion = conversion.apply("IV", 4)
        conversion = conversion.apply("I", 1)
        return conversion.sum
    }
}