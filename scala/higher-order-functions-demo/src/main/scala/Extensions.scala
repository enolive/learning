object Extensions {
    implicit class Division(val number: Int) extends AnyVal {
        def isDivisibleBy(divisor: Int) = {
            number % divisor == 0
        }

    }
    
}
