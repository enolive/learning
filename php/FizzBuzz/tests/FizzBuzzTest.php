<?php
namespace Core\tests;

use Core\FizzFuzzEngine;
use PHPUnit_Framework_TestCase;

class FizzBuzzTest extends PHPUnit_Framework_TestCase
{
    /**
     * @var FizzFuzzEngine
     */
    private $target;

    protected function setUp()
    {
        $this->target = new FizzFuzzEngine();
    }

    public function fizzBuzzProvider()
    {
        return [
            [1, "1"],
            [2, "2"],
            [3, "Fizz"],
            [6, "Fizz"],
            [5, "Buzz"],
            [10, "Buzz"],
            [15, "Fizz-Buzz"],
        ];
    }

    /**
     * @dataProvider fizzBuzzProvider
     */
    public function testThatFizzBuzzEngineReturnsExpectedResults($number, $expected)
    {
        // act
        $result = $this->target->calculateResult($number);
        // assert
        $this->assertThat($result, self::equalTo($expected));
    }
}
