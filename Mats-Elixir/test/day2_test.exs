defmodule Day2Test do
  use ExUnit.Case
  require Logger

  test "surface" do
    assert Day2.surface(2,3) == 12 
  end

  test "packagesurface" do
    assert Day2.packagesurface({2,2,2}) == 24
  end

  test "smallestsurface" do
    assert Day2.smallestsurface({1,2,3}) == 2
  end

  test "volume" do
    assert Day2.volume({2,3,4}) == 24
  end

  test "two smallest items" do
    assert Day2.two_smallest_items([2,3,4,1]) == [1,2]
    assert Day2.two_smallest_items([1,2,3,4]) == [1,2]
    assert Day2.two_smallest_items([4,3,2,1]) == [1,2]
  end

  test "ribbon length" do
    assert Day2.ribbon_length({3,4,2}) == 34
    assert Day2.ribbon_length({1,2,3}) == 12
    assert Day2.ribbon_length({4,2,2}) == 24
  end
    
  test "parsemeasure" do
    assert Day2.parse_measure("1x22x333") == {1, 22, 333}
  end

  test "readlines" do
    lines = "test/tworows_of_measures.txt"
    |> Day2.readlines
    assert Enum.count(lines) == 2
  end

  test "Total surface" do
    totalsurface = "test/tworows_of_measures.txt"
    |> Day2.readlines
    |> Day2.total_surface
    assert totalsurface == (28 * 2)
  end

  @tag advent: true
  test "Compute total surface in AdventOfCode Day 2, task 1" do
    totalsurface = "test/day2.txt"
    |> Day2.readlines
    |> Day2.total_surface

    Logger.debug("Day 2, Task 1. Total surface = #{totalsurface}")
    assert totalsurface == 1586300 
  end

  @tag advent: true
  test "Compute total ribbon lenght in AdventOfCode Day 2, task 1" do
    totalribbon = "test/day2.txt"
    |> Day2.readlines
    |> Day2.total_ribbon
    
    Logger.debug("Day 2, Task 2. Total ribbon length = #{totalribbon}")
    assert totalribbon == 3737498 
  end

end

