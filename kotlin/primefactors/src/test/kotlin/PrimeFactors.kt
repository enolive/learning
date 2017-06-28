class PrimeFactors {

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val list = ArrayList<Int>()
            val input = number
            for (i in arrayOf(2, 3, 5)) {
                split(input, i, list)
            }
            return list
        }

        private fun split(remainder: Int, factor: Int, extractedFactors: ArrayList<Int>) {
            var copy = remainder
            while (copy % factor == 0) {
                extractedFactors.add(factor)
                copy /= factor
            }
        }
    }
}