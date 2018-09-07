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
    if (!$answerPart1 && strpos($md5, '00000') === 0) {
        $answerPart1 = $answer;
    }
    // Check if first six characters are all zeroes, then exit loop, we are done
    if (strpos($md5, '000000') === 0) {
        break;
    }
}
// Echo response
echo 'part one: ' . $answerPart1 . PHP_EOL;
echo 'part two: ' . $answer . PHP_EOL;
