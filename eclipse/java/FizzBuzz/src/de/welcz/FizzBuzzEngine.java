package de.welcz;

import java.util.Arrays;
import java.util.List;

public class FizzBuzzEngine {

	private static final List<Rule> rules = Arrays.asList(
			Rule.divisibleBy(15).returns("FizzBuzz"),
			Rule.divisibleBy(3).returns("Fizz"), 
			Rule.divisibleBy(5).returns("Buzz"));

	public String calculateNext(int number) {
		return rules.stream()
				.filter(r -> r.appliesTo(number))
				.map(Rule::giveResult)
				.findFirst()
				.orElse(Integer.toString(number));
	}

}
