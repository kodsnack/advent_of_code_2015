<?php
// Get input data from file, if it exists
if (!is_file(__DIR__ . '/input')) {
    die('Input file not found');
}
$input = new SplFileObject(__DIR__ . '/input');
$input->setFlags(SplFileObject::READ_AHEAD | SplFileObject::SKIP_EMPTY);
// Setup base variables
$niceStringsCount = 0;
// Loop all the lines
while (!$input->eof()) {
    $currentLine = trim($input->current());
    // Move to the next line for the next loop now
    $input->next();
    // Booleans to hold the validity of the line
    $repeatingChar = false;
    $pairFound = false;
    // Get number of characters, skip the two last since it doesn't have any
    // next pair of character to test against
    $lineLength = (strlen($currentLine) - 2);
    // Loop all the characters
    for ($i = 0; $i < $lineLength; ++$i) {
        // Setup variables for next and second next character position
        $j = ($i + 1);
        $k = ($j + 1);
        // Check if current and second next character is the same
        if (!$repeatingChar
            && $currentLine[$i] == $currentLine[$k]
        ) {
            $repeatingChar = true;
        }
        // Check if the remaining line string contains another instance of the
        // current and next character combination
        if (!$pairFound
            && $k <= $lineLength
            && false !== strpos($currentLine, "{$currentLine[$i]}{$currentLine[$j]}", $k)
        ) {
            $pairFound = true;
        }
        // Break loop if requirements are already met
        if ($repeatingChar
            && $pairFound
        ) {
            break;
        }
    }
    // One (or both) of the requirements was not met, invalid line
    if (!$repeatingChar
        || !$pairFound
    ) {
        continue;
    }
    // This was a valid (nice) line, count and continue looping lines
    ++$niceStringsCount;
}
// Echo response
echo 'part two: ' . $niceStringsCount . PHP_EOL;
