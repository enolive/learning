class RuleSelector {

	private final int divisor;

	RuleSelector(int divisor) {
		this.divisor = divisor;
	}

	Rule returns(String result) {
		return new Rule(divisor, result);
	}
}