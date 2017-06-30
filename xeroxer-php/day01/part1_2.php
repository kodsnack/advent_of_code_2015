<?php
// Get input data from file, if it exists
if (!is_file(__DIR__ . '/input')) {
    die('Input file not found');
}
$input = trim(file_get_contents(__DIR__ . '/input'));
// Get and check length of data
$length = strlen($input);
if (!$length) {
    die('No input data found');
}
// Setup base variables
$currentFloor = 0;
$basementEntry = 0;
// Loop all the characters
for ($i = 0; $i < $length; ++$i) {
    // ( = Increase current floor number
    if ($input[$i] == '(') {
        ++$currentFloor;
    // ) = Decrease current floor number
    } elseif ($input[$i] == ')') {
        --$currentFloor;
        // The first time we visit a floor below 0 = basement,
        // set entry variable to current character number
        if ($basementEntry == 0
            && $currentFloor < 0
        ) {
            $basementEntry = ($i + 1);
        }
    // Change no floor number, invalid character
    } else {
        echo 'Found invalid character:' . PHP_EOL;
        var_dump($input[$i]);
    }
}
// Echo response
echo 'part one: ' . $currentFloor . PHP_EOL;
echo 'part two: ' . $basementEntry . PHP_EOL;
