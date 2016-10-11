class Rule {
	private final int divisor;
	private final String result;

	Rule(int divisor, String result) {
		this.divisor = divisor;
		this.result = result;
	}

	static RuleSelector divisibleBy(int divisor) {
		return new RuleSelector(divisor);
	}

	boolean appliesTo(int number) {
		return number % divisor == 0;
	}

	String giveResult() {
		return result;
	}
}