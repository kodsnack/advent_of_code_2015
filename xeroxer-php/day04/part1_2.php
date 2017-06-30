<?php
// Setup base variables
$input = 'bgvyzdsv';
$answerPart1 = 0;
$answer = 0;
// Loop until we find matching hash
while (true) {
    ++$answer;
    $md5 = md5("{$input}{$answer}");
    // Check if first five characters are all zeroes, set variable and continue
    if (!$answerPart1 && substr($md5, 0, 5) === '00000') {
        $answerPart1 = $answer;
    }
    // Check if first six characters are all zeroes, then exit loop, we are done
    if (substr($md5, 0, 6) === '000000') {
        break;
    }
}
// Echo response
echo 'part one: ' . $answerPart1 . PHP_EOL;
echo 'part two: ' . $answer . PHP_EOL;
