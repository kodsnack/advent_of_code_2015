<?php
// Get input data from file, if it exists
if (!is_file(__DIR__ . '/input')) {
    die('Input file not found');
}
$input = new SplFileObject(__DIR__ . '/input');
$input->setFlags(SplFileObject::READ_AHEAD | SplFileObject::SKIP_EMPTY);
// Setup base variables
$neededPaper = 0;
$feetOfRibbon = 0;
// Loop all the lines
while (!$input->eof()) {
    // Get all dimensions and make sure they are integers
    $dimensions = array_map('intval', explode('x', $input->current()));
    // Make sure this was a valid line
    if (count($dimensions) != 3) {
        echo 'Found invalid line data:' . PHP_EOL;
        var_dump($dimensions);
    }
    // Calculate all the surfaces
    $surfaces = [
        ($dimensions[0] * $dimensions[1]),
        ($dimensions[1] * $dimensions[2]),
        ($dimensions[2] * $dimensions[0]),
    ];
    // Calculate all the surfaces the paper needs to cover
    $neededPaper += (2 * $surfaces[0]);
    $neededPaper += (2 * $surfaces[1]);
    $neededPaper += (2 * $surfaces[2]);
    // And add the smallest number for calculated slack
    $neededPaper += min($surfaces);
    // Sort the dimensions so we can extract the ribbon length needed
    sort($dimensions, SORT_NUMERIC);
    // Add ribbon to wrap package and make a bow
    $feetOfRibbon += $dimensions[0] + $dimensions[0];
    $feetOfRibbon += $dimensions[1] + $dimensions[1];
    $feetOfRibbon += $dimensions[0] * $dimensions[1] * $dimensions[2];
    // Go to next line
    $input->next();
}
// Echo response
echo 'part one: ' . $neededPaper . PHP_EOL;
echo 'part two: ' . $feetOfRibbon . PHP_EOL;
