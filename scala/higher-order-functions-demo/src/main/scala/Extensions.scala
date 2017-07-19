object Extensions {
    implicit class Division(val number: Int) extends AnyVal {
        def isDivisibleBy(divisor: Int): Boolean = {
            number % divisor == 0
        }

    }
    
}
