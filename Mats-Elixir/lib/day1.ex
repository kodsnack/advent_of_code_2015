defmodule Day1 do 
  require Logger

  def countchars(list, string_to_find) do
    Enum.filter(list, fn(v) -> v == string_to_find end)
    |> Enum.count()
  end

  def find_level_minus_one(list, -1, pos) do
    {-1,pos}
  end

  def find_level_minus_one([head|tail], level, pos) do
    case head do
      "(" -> find_level_minus_one(tail, level+1, pos+1)
      ")" -> find_level_minus_one(tail, level-1, pos+1)
      _other -> find_level_minus_one(tail, level, pos+1)
    end
  end
  
end
