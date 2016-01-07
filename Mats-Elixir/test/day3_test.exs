defmodule Day3Test do
  use ExUnit.Case
  require Logger

  
  @tag advent: true
  test "Visited houses in AdventOfCode Day 3, task 1" do
    list = "test/day3.txt"
    |> File.read!
    |> String.strip
    |> String.codepoints

    visited_houses_count = Day3.houses_visited(list)
    |> Day3.unique_house_visit_count

    Logger.debug ("Day 3, Task 1, Houses visited = #{visited_houses_count}")
    assert visited_houses_count == 2565
  end

  @tag advent: true
  test "Visited houses in AdventOfCode Day 3, task 2" do
    list = "test/day3.txt"
    |> File.read!
    |> String.strip
    |> String.codepoints

    visited_houses_count = Day3.houses_visited_together_with_robot(list)
    |> Day3.unique_house_visit_count

    Logger.debug ("Day 3, Task 2, Houses visited together with robot = #{visited_houses_count}")
    assert visited_houses_count == 2639
  end
  
  test "Visited houses using simple input" do
    assert Day3.houses_visited(String.codepoints("^v^v^v^v^v"))
    |> Day3.unique_house_visit_count == 2

    assert Day3.houses_visited(String.codepoints("^>v<"))
    |> Day3.unique_house_visit_count == 4
  end
  
  test "Visited houses using simple input and robot" do
    assert Day3.houses_visited_together_with_robot(String.codepoints("^v^v^v^v^v"))
    |> Day3.unique_house_visit_count == 11

    assert Day3.houses_visited_together_with_robot(String.codepoints("^>v<"))
    |> Day3.unique_house_visit_count == 3
  end
  
end
