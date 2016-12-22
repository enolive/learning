class Converter {
    def toArabic(roman: String) : Int = {
        AllDigits.of(roman)
                .map(d => d.toArabic)
                .sum
    }
}
