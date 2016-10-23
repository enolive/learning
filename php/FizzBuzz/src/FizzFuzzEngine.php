<?php

namespace Core;

class FizzFuzzEngine
{
    private $rules;

    public function __construct()
    {
        $this->rules = [
            new Rule(15, "Fizz-Buzz"),
            new Rule(3, "Fizz"),
            new Rule(5, "Buzz"),
        ];
    }


    /**
     * @param $number int
     * @return string
     */
    function calculateResult($number):string
    {
        $matchingRules = array_filter(
            $this->rules, function ($rule) use ($number) {
            /** @noinspection PhpUndefinedMethodInspection */
            return $rule->appliesTo($number);
        });

        if (count($matchingRules) == 0) {
            return strval($number);
        }

        return self::first($matchingRules)->giveResult();
    }

    private static function first($matchingRule):Rule
    {
        return array_values($matchingRule)[0];
    }
}