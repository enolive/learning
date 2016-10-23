<?php

namespace Core;


class Rule
{
    private $denominator;
    private $result;

    /**
     * Rule constructor.
     */
    public function __construct($denominator, $result)
    {
        $this->denominator = $denominator;
        $this->result = $result;
    }

    /**
     * @param $number
     * @param $denominator
     * @return bool
     */
    private static function isDivisibleBy($number, $denominator):bool
    {
        return $number % $denominator == 0;
    }

    public function appliesTo($number)
    {
        return self::isDivisibleBy($number, $this->denominator);
    }

    public function giveResult()
    {
        return $this->result;
    }
}