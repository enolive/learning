class PrimeFactors {
    fun split(i: Int): Iterable<Int> {
        return PrimeFactors.of(i)
    }

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val list = ArrayList<Int>()
            val input = number
            split(input, 2, list)
            split(input, 3, list)
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