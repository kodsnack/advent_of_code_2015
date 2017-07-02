<?php
// Get input data from file, if it exists
if (!is_file(__DIR__ . '/input')) {
    die('Input file not found');
}
$input = new SplFileObject(__DIR__ . '/input');
$input->setFlags(SplFileObject::READ_AHEAD | SplFileObject::SKIP_EMPTY);
// Setup base variables
$niceStringsCount = 0;
$vowels = ['a', 'e', 'i', 'o', 'u'];
$invalidStrs = ['ab', 'cd', 'pq', 'xy'];
// Loop all the lines
while (!$input->eof()) {
    $currentLine = $input->current();
    // Since we already have the current line and will run a few possible continue,
    // we move to the next line for the next loop now
    $input->next();
    // Loop the invalid strings, they should not exist inside line
    foreach ($invalidStrs as $invalidStr) {
        // If we find a match inside line, move to next line
        if (false !== strpos($currentLine, $invalidStr)) {
            continue 2;
        }
    }
    // Let's loop the vowels and count the instances
    $vowelCount = 0;
    foreach ($vowels as $vowel) {
        // Add the current vowel count to the line vowel count
        $vowelCount += substr_count($currentLine, $vowel);
        // If we have three or more, move to next check
        if ($vowelCount >= 3) {
            continue;
        }
    }
    // If we found less than three vowels inside line, move to next line
    if ($vowelCount < 3) {
        continue;
    }
    // Now we need to check if any character is repeated at least twice in a row
    $twiceInARow = false;
    // Get number of characters, skip the last since it doesn't have any
    // next character to test against
    $lineLength = (strlen($currentLine) - 1);
    // Loop all the characters
    for ($i = 0; $i < $lineLength; ++$i) {
        if ($currentLine[$i] == $currentLine[($i + 1)]) {
            $twiceInARow = true;
            break;
        }
    }
    // We found no character repeated two times in a row, move to next line
    if (!$twiceInARow) {
        continue;
    }
    // This was a valid (nice) line, count and continue looping lines
    ++$niceStringsCount;
}
// Echo response
echo 'part one: ' . $niceStringsCount . PHP_EOL;
