using System;
using System.Linq;

namespace Adventofcode
{
    class Day6
    {
        static void UpdateLights(bool[,] grid, int rowFrom, int colFrom, int rowTo, int colTo, Func<bool, bool> action)
        {
            for (int row = rowFrom; row <= rowTo; row++)
            {
                for (int col = colFrom; col <= colTo; col++)
                {
                    grid[row, col] = action(grid[row, col]);
                }
            }

        }
        static void UpdateLights2(int[,] grid, int rowFrom, int colFrom, int rowTo, int colTo, int change)
        {
            for (int row = rowFrom; row <= rowTo; row++)
            {
                for (int col = colFrom; col <= colTo; col++)
                {
                    grid[row, col] += change;
                    grid[row, col] = Math.Max(grid[row, col], 0);
                }
            }

        }
        public static void Run()
        {
            bool[,] grid = new bool[1000, 1000];
            string[] input = System.IO.File.ReadAllLines("day6input.txt");
            //string[] input = { "toggle 0,0 through 999,999", "turn off 499,499 through 500,500" };
            foreach (string line in input)
            {
                var parts = line.Split(new string[] { "through" }, StringSplitOptions.None);
                var to = parts[1].Split(',');
                var from = parts[0].Split(new string[] { "turn on", "toggle", "turn off", " " }, StringSplitOptions.RemoveEmptyEntries)[0].Split(',');
                if (line.StartsWith("turn on"))
                {
                    UpdateLights(grid, Convert.ToInt32(from[0]), Convert.ToInt32(from[1]),
                                       Convert.ToInt32(to[0]), Convert.ToInt32(to[1]), b => true);
                }
                else if (line.StartsWith("toggle"))
                {
                    UpdateLights(grid, Convert.ToInt32(from[0]), Convert.ToInt32(from[1]),
                                       Convert.ToInt32(to[0]), Convert.ToInt32(to[1]), b => !b);
                }
                else if (line.StartsWith("turn off"))
                {
                    UpdateLights(grid, Convert.ToInt32(from[0]), Convert.ToInt32(from[1]),
                                      Convert.ToInt32(to[0]), Convert.ToInt32(to[1]), b => false);
                }
                else
                {
                    Console.WriteLine("Could not parse line: " + line);
                }
            }
            Console.WriteLine("Num lit: " + grid.Cast<bool>().Sum(b => Convert.ToInt32(b)));

            int[,] grid2 = new int[1000, 1000];
            foreach (string line in input)
            {
                var parts = line.Split(new string[] { "through" }, StringSplitOptions.None);
                var to = parts[1].Split(',');
                var from = parts[0].Split(new string[] { "turn on", "toggle", "turn off", " " }, StringSplitOptions.RemoveEmptyEntries)[0].Split(',');
                if (line.StartsWith("turn on"))
                {
                    UpdateLights2(grid2, Convert.ToInt32(from[0]), Convert.ToInt32(from[1]),
                                       Convert.ToInt32(to[0]), Convert.ToInt32(to[1]), 1);
                }
                else if (line.StartsWith("toggle"))
                {
                    UpdateLights2(grid2, Convert.ToInt32(from[0]), Convert.ToInt32(from[1]),
                                       Convert.ToInt32(to[0]), Convert.ToInt32(to[1]), 2);
                }
                else if (line.StartsWith("turn off"))
                {
                    UpdateLights2(grid2, Convert.ToInt32(from[0]), Convert.ToInt32(from[1]),
                                      Convert.ToInt32(to[0]), Convert.ToInt32(to[1]), -1);
                }
                else
                {
                    Console.WriteLine("Could not parse line: " + line);
                }
            }
            Console.WriteLine("Total brightness: " + grid2.Cast<int>().Sum());

        }
    }
}
