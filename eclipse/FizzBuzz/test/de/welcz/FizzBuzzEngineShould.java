package de.welcz;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import static org.hamcrest.core.Is.*;

public class FizzBuzzEngineShould {

	private FizzBuzzEngine target;

	@Before
	public void setUp() {
		target = new FizzBuzzEngine();

	}

	@Test
	public void returnNormalNumbersAsIs() {
		assertThat(target.calculateNext(1), is("1"));
		assertThat(target.calculateNext(2), is("2"));
		assertThat(target.calculateNext(11), is("11"));
	}

	@Test
	public void returnFizzOnDivisibleByThree() {
		assertThat(target.calculateNext(3), is("Fizz"));
		assertThat(target.calculateNext(6), is("Fizz"));
	}

	@Test
	public void returnBuzzOnDivisibleByFive() {
		assertThat(target.calculateNext(5), is("Buzz"));
		assertThat(target.calculateNext(10), is("Buzz"));
	}

	@Test
	public void returnFizzBuzzOnDivisibleByThreeAndFive() {
		assertThat(target.calculateNext(15), is("FizzBuzz"));
		assertThat(target.calculateNext(30), is("FizzBuzz"));
	}
}
