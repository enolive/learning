class PrimeFactors {

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val list = ArrayList<Int>()
            val input = number
            val result = Result(input)
            for (i in arrayOf(2, 3, 5)) {
                split(result, i, list)
            }
            return list
        }

        private fun split(result: Result, factor: Int, list: ArrayList<Int>) {
            split(result.remainder, factor, list)
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

class Result(input: Int) {
    var remainder = input

}
