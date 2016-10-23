<?php

namespace Core;


class Rule
{
    private $denominator;
    private $result;

    public function __construct(int $denominator, string $result)
    {
        $this->denominator = $denominator;
        $this->result = $result;
    }

    public function appliesTo(int $number)
    {
        return self::isDivisibleBy($number, $this->denominator);
    }

    private static function isDivisibleBy(int $number, int $denominator):bool
    {
        return $number % $denominator == 0;
    }

    public function giveResult():string
    {
        return $this->result;
    }
}