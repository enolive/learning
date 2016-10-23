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

    function calculateResult(int $number):string
    {
        $filteredRules = array_filter($this->rules, function (Rule $rule) use ($number) {
            return $rule->appliesTo($number);
        });

        $matchingRule = self::firstOrDefault($filteredRules);
        return $matchingRule != null
            ? $matchingRule->giveResult()
            : strval($number);
    }

    private static function firstOrDefault(array $items):Rule
    {
        if (count($items) == 0) {
            return null;
        }

        return array_values($items)[0];
    }
}