defmodule Day3 do
  require Logger

	def houses_visited(list) do
		Enum.reduce(list, [{0, 0}], fn(inputchar, visited) ->
			{x, y} = List.first(visited)
			case inputchar do
				"^" -> [{x, y + 1} | visited] 
				"v" -> [{x, y - 1} | visited]
				"<" -> [{x - 1, y} | visited]
				">" -> [{x + 1, y} | visited]					
			end			
		end)
	end
	

	def houses_visited_together_with_robot(list) do

		visited_by_santa = Enum.take_every(list, 2)
		|> houses_visited

		[head | tail] = list
		visited_by_robot = Enum.take_every(tail, 2)
		|> houses_visited

		visited_by_santa ++ visited_by_robot
		
	end

	
	def unique_house_visit_count(list) do
		list |> Enum.uniq |> Enum.count
	end

	
end


  
