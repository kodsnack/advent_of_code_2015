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
$deliveredCoordinates = ['0x0' => 1];
$posX = 0;
$posY = 0;
$deliveredCoordinatesP2 = ['0x0' => 2];
$posXs = [0, 0];
$posYs = [0, 0];
$move = 0;
// Loop all the characters
for ($i = 0; $i < $length; ++$i) {
    // Move west
    if ($input[$i] == '<') {
        --$posX;
        --$posXs[$move];
    // Move east
    } elseif ($input[$i] == '>') {
        ++$posX;
        ++$posXs[$move];
    // Move south
    } elseif ($input[$i] == 'v') {
        --$posY;
        --$posYs[$move];
    // Move north
    } elseif ($input[$i] == '^') {
        ++$posY;
        ++$posYs[$move];
    // Move no direction, invalid character
    } else {
        echo 'Found invalid character:' . PHP_EOL;
        var_dump($input[$i]);
        continue;
    }
    // Increase of set delivered packages to current coordinates
    if (isset($deliveredCoordinates["{$posX}x{$posY}"])) {
        ++$deliveredCoordinates["{$posX}x{$posY}"];
    } else {
        $deliveredCoordinates["{$posX}x{$posY}"] = 1;
    }
    if (isset($deliveredCoordinatesP2["{$posXs[$move]}x{$posYs[$move]}"])) {
        ++$deliveredCoordinatesP2["{$posXs[$move]}x{$posYs[$move]}"];
    } else {
        $deliveredCoordinatesP2["{$posXs[$move]}x{$posYs[$move]}"] = 1;
    }
    // Set who is to move the next loop
    $move = ($move == 0 ? 1 : 0);
}
// Echo response
echo 'part one: ' . count($deliveredCoordinates) . PHP_EOL;
echo 'part one: ' . count($deliveredCoordinatesP2) . PHP_EOL;
