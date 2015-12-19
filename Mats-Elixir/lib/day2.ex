defmodule Day2 do
	require Logger

	def surface(x,y) do
		2 * x * y
	end		

	def packagesurface({l,w,h}) do
		surface(l,w) + surface(w,h) + surface(h,l) 
	end

	def smallestsurface({l,w,h}) do
		Enum.min([ l * w, w * h, h * l])
	end

	def volume({l,w,h}) do
		l * w * h
	end

	def two_smallest_items(list) do
		Enum.sort(list)
		|> Enum.slice(0..1)
	end

	def ribbon_length({l,w,h}) do
		ribbon = two_smallest_items([l,w,h])
		|> Enum.reduce(0, fn(x, total) ->
			total + (x * 2)
		end)
		bow = volume({l,w,h})
		ribbon + bow
	end

	def parse_measure(measure) do
		[{l,_},{w,_},{h,_}] = String.split(measure,"x") |> Enum.map(&Integer.parse/1)
		{l,w,h}
	end

	def readlines(filename) do
		filename |> File.read! |> String.strip |> String.split("\n")
	end

	def total_surface(measures) do
		Enum.reduce(measures, 0, fn(measure, total) -> 
			parsed = parse_measure(measure)
			packsurface = parsed |> packagesurface
			smallsurface = parsed |> smallestsurface
			packsurface + smallsurface + total
		end)
	end		

	def total_ribbon(measures) do
		Enum.reduce(measures, 0, fn(measure, total) -> 
			parsed = parse_measure(measure)
			ribbon = parsed |> ribbon_length
			ribbon + total
		end)
	end		

end


	
