class PrimeFactors {

    companion object {
        fun of(number: Int): ArrayList<Int> {
            val input = number
            val result = Result(input)
            for (i in arrayOf(2, 3, 5)) {
                split(result, i, result.list)
            }
            return result.list
        }

        private fun split(result: Result, factor: Int, list: ArrayList<Int>) {
            while (result.remainder % factor == 0) {
                list.add(factor)
                result.remainder /= factor
            }
        }

    }
}

class Result(input: Int) {
    var remainder = input
    val list = ArrayList<Int>()

}
