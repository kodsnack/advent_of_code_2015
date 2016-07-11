using System;
using System.Collections.Generic;

namespace AdventOfCode
{
    partial class Program
    {
        static void Main(string[] args)
        {
            List<Action> days = new List<Action> {
                () => Day1(),
                () => Day2(),
                () => Day3(),
                () => Day4(),
                () => Day5(),
                () => Day6.Run(),
                () => Day7.Run(),
                () => Day8(),
                () => Day9.Run(),
                () => Day10(),
                () => Day11.Run(),
                () => Day12.Run(),
                () => Day13.Run(),
                () => Day14.Run(),
                () => Day15.Run(),
                () => Day16(),
                () => Day17.Run(),
                () => Day18.Run(),
                () => Day19.Run(),
                () => Day20(),
                () => Day21.Run(),
                () => Day22.Run(),
                () => Day23.Run(),
                () => Day24.Run(),
                () => Day25.Run()
            };
            for (int i = 0; i < days.Count; i++)
            {
                Console.WriteLine("Day " + (i + 1) + ":");
                days[i].Invoke();
                Console.WriteLine();
            }
            Console.WriteLine("Färdig!");
            Console.ReadKey();
        }
    }
}
