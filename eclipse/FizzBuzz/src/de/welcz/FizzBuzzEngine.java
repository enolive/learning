package de.welcz;

import java.util.ArrayList;
import java.util.List;

public class FizzBuzzEngine {

	private static final List<Rule> rules = new ArrayList<Rule>();
	static {
		rules.add(Rule.divisibleBy(15).returns("FizzBuzz"));
		rules.add(Rule.divisibleBy(3).returns("Fizz"));
		rules.add(Rule.divisibleBy(5).returns("Buzz"));
	}

	public String calculateNext(int number) {
		for (Rule rule : rules) {
			if (rule.appliesTo(number)) {
				return rule.giveResult();
			}
		}

		return Integer.toString(number);
	}

}
