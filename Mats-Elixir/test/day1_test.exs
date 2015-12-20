defmodule Day1Test do
  use ExUnit.Case
  require Logger

  test "count occurrance of a" do
    result = Day1.countchars(String.codepoints("abcabc"),"b") 
    assert result == 2
  end

  
  
  @tag advent: true
  test "Compute level in AdventOfCode Day 1, task 1" do
    list = "test/day1.txt"
    |> File.read!
    |> String.strip
    |> String.codepoints

    level =  Day1.countchars(list,"(") - Day1.countchars(list,")")

    Logger.debug ("Day 1, Task 1, Level after all chars = #{level}")
    assert level == 232
  end

  
  test "Find position for level -1 in simple string" do
    list = "(())))"
    |> String.codepoints
    
    {level,pos} = Day1.find_level_minus_one(list, 0, 0)
    assert pos == 5 && level == -1
  end

  
  @tag advent: true
  test "Find position for level -1 in AdventOfCode Day 1, task 2" do

    list = "test/day1.txt"
    |> File.read!
    |> String.strip
    |> String.codepoints

    {level,pos} = Day1.find_level_minus_one(list, 0, 0)
    
    Logger.debug ("Day1, Task2, Position is at #{pos} when reaching level #{level}")
    assert {level, pos} == {-1,1783}
  end

end
