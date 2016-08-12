package de.welcz;

class RuleSelector {

	private int divisor;

	public RuleSelector(int divisor) {
		this.divisor = divisor;
	}

	public Rule returns(String result) {
		return new Rule(divisor, result);
	}
}