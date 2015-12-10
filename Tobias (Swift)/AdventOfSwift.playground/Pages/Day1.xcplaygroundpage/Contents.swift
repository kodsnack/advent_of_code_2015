import Cocoa

/*
--- Day 1: Not Quite Lisp ---

Santa was hoping for a white Christmas, but his weather machine's "snow" function is powered by stars, and he's fresh out! To save Christmas, he needs you to collect fifty stars by December 25th.

Collect stars by helping Santa solve puzzles. Two puzzles will be made available on each day in the advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

Here's an easy puzzle to warm you up.

Santa is trying to deliver presents in a large apartment building, but he can't find the right floor - the directions he got are a little confusing. He starts on the ground floor (floor 0) and then follows the instructions one character at a time.

An opening parenthesis, (, means he should go up one floor, and a closing parenthesis, ), means he should go down one floor.

The apartment building is very tall, and the basement is very deep; he will never find the top or bottom floors.
*/

func getFloor(code: String) -> (Int, Int)
{
  var floor = 0
  var posWhenBasement = -1
  var currentPos = 1

  for input in code.characters
  {
    switch input
    {
    case "(":
      floor += 1;
    case ")":
      floor -= 1;
    default:
      print("Don't know what to do with: input")
    }

    if floor == -1 && posWhenBasement == -1
    {
      posWhenBasement = currentPos
    }

    currentPos += 1
  }

  return (floor, posWhenBasement);
}

// (()) and ()() both result in floor 0.
getFloor("(())").0 == 0
getFloor("()()").0 == 0

// ((( and (()(()( both result in floor 3.
getFloor("(((").0 == 3
getFloor("(()(()(").0 == 3

// ))((((( also results in floor 3.
getFloor("))(((((").0 == 3

// ()) and ))( both result in floor -1 (the first basement level).
getFloor("())").0 == -1
getFloor("))(").0 == -1

// ))) and )())()) both result in floor -3.
getFloor(")))").0 == -3
getFloor(")())())").0 == -3

let adventInput = getInput("1")

getFloor(adventInput).0

/*
--- Part Two ---

Now, given the same instructions, find the position of the first character that causes him to enter the basement (floor -1).
The first character in the instructions has position 1, the second character has position 2, and so on.
*/


// ) causes him to enter the basement at character position 1.
getFloor(")").1 == 1

// ()()) causes him to enter the basement at character position 5.
getFloor("()())").1 == 5

let adventInput2 = getInput("1")

getFloor(adventInput2).1



