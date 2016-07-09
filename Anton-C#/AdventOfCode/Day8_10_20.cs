using System;
using System.Linq;

namespace AdventOfCode
{
    partial class Program
    {
        static void Day8()
        {
            var input = System.IO.File.ReadAllLines("day8input.txt");
            //var input = new string[] { "\"\"",  "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\"" };
            int charCount = 0;
            foreach (string line in input)
            {
                for (int i = 1; i < line.Length - 1; i++)
                {
                    if (line[i] == '\\')
                    {
                        if (line[i + 1] == 'x')
                        {
                            i += 3;
                        }
                        else i += 1;
                    }
                    charCount++;
                }
            }
            int totalChars = Enumerable.Sum(input, l => l.Length);
            Console.WriteLine("Difference " + (totalChars - charCount));

            var newStrings = new string[input.Length];
            int charCountNew = 0;
            foreach (string line in input)
            {
                charCountNew += 2;
                for (int i = 0; i < line.Length; i++)
                {

                    if (line[i] == '\\')
                    {
                        charCountNew++;
                    }
                    else if (line[i] == '"')
                    {
                        charCountNew++;
                    }
                    charCountNew++;
                }
            }
            Console.WriteLine("Difference " + (charCountNew - totalChars));

        }

        static void Day10()
        {
            //var input = "1";
            var input = "1113222113";
            const int NumLoops = 50;

            System.Text.StringBuilder sb = new System.Text.StringBuilder();
            var currentString = input;
            for (int i = 0; i < NumLoops; i++)
            {
                sb.Clear();
                for (int j = 0; j < currentString.Length; j++)
                {
                    int count = 1;
                    char currentDigit = currentString[j];
                    while (j < currentString.Length - 1 && currentString[j + 1] == currentDigit)
                    {
                        count++;
                        j++;
                    }
                    sb.Append(count);
                    sb.Append(currentDigit);
                }
                currentString = sb.ToString();
            }
            Console.WriteLine("Length of results: " + currentString.Length);
        }
        static void Day20()
        {
            uint target = 29000000;

            int numHouses = 1000 * 1000 + 1; 
            var houses = new uint[numHouses];
            for (uint elf = 1; elf < numHouses; elf++)
            {
                for (uint house = elf; house < numHouses; house += elf)
                {
                    houses[house] += 10 * elf;
                }

            }
            for (uint house = 1; house < numHouses; house++)
            {
                if (houses[house] >= target)
                {
                    Console.WriteLine("Part 1, house number " + house);
                    break;
                }
            }

            houses = new uint[numHouses];
            for (uint elf = 1; elf < numHouses; elf++)
            {
                for (uint house = elf, numPresents = 0; house < numHouses && numPresents < 50; house += elf, numPresents++)
                {
                    houses[house] += 11 * elf;
                }

            }
            for (uint house = 1; house < numHouses; house++)
            {
                if (houses[house] >= target)
                {
                    Console.WriteLine("Part 2, house number " + house);
                    break;
                }
            }
        }
    }
}