using System;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Text;

namespace AdventOfCode
{
    partial class Program
    {
        static void Day1()
        {
            string input = System.IO.File.ReadAllText("day1input.txt");
            int level = 0;
            bool basementFound = false;
            for (int i = 0; i < input.Length; i++)
            {
                if (input[i] == '(')
                {
                    level++;
                }
                else if (input[i] == ')')
                {
                    level--;
                }
                if (level == -1 && !basementFound)
                {
                    Console.WriteLine("Basement at step " + (i + 1));
                    basementFound = true;
                }
            }
            Console.WriteLine("Final level: " + level);
        }
        static void Day2()
        {
            int requiredPaper = 0;
            int requiredRibbon = 0;
            var input = System.IO.File.ReadAllLines("day2input.txt");
            foreach (var line in input)
            {
                var parts = line.Split(new char[] { 'x' });
                int[] lengths = { Int32.Parse(parts[0]), Int32.Parse(parts[1]), Int32.Parse(parts[2]) };
                int[] sides = { lengths[0] * lengths[1], lengths[1] * lengths[2], lengths[2] * lengths[0] };
                requiredPaper += 2 * sides.Sum() + sides.Min();

                int shortestDistance = 2 * (lengths.Sum() - lengths.Max());
                requiredRibbon += shortestDistance + lengths[0] * lengths[1] * lengths[2];
            }
            Console.WriteLine("Required paper: " + requiredPaper);
            Console.WriteLine("Required ribbon: " + requiredRibbon);
        }
        static void Day3()
        {
            var input = System.IO.File.ReadAllText("day3input.txt");
            int x = 0, y = 0;
            HashSet<Tuple<int, int>> visitedHouses = new HashSet<Tuple<int, int>>();
            visitedHouses.Add(new Tuple<int, int>(x, y));
            for (int i = 0; i < input.Length; i++)
            {
                switch (input[i])
                {
                    case '<':
                        x--;
                        break;
                    case '>':
                        x++;
                        break;
                    case '^':
                        y++;
                        break;
                    case 'v':
                        y--;
                        break;
                }
                visitedHouses.Add(new Tuple<int, int>(x, y));
            }
            Console.WriteLine("Number of houses: " + visitedHouses.Count);
            visitedHouses.Clear();
            x = y = 0;
            visitedHouses.Add(new Tuple<int, int>(x, y));
            for (int i = 0; i < input.Length; i = i + 2)
            {
                switch (input[i])
                {
                    case '<':
                        x--;
                        break;
                    case '>':
                        x++;
                        break;
                    case '^':
                        y++;
                        break;
                    case 'v':
                        y--;
                        break;
                }
                visitedHouses.Add(new Tuple<int, int>(x, y));
            }
            x = y = 0;
            for (int i = 1; i < input.Length; i = i + 2)
            {
                switch (input[i])
                {
                    case '<':
                        x--;
                        break;
                    case '>':
                        x++;
                        break;
                    case '^':
                        y++;
                        break;
                    case 'v':
                        y--;
                        break;
                }
                visitedHouses.Add(new Tuple<int, int>(x, y));
            }
            Console.WriteLine("Number of houses, with robot: " + visitedHouses.Count);
        }
        static void Day4()
        {
            var input = "yzbqklnj";
            MD5 md5Hash = MD5.Create();
            StringBuilder stringBuilder = new StringBuilder();
            int number = -1;
            while (true)
            {
                number++;
                string test = input + number.ToString();
                byte[] hashBytes = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(test));
                stringBuilder.Clear();
                for (int i = 0; i < 3; i++)
                {
                    stringBuilder.Append(hashBytes[i].ToString("x2"));
                }
                if (stringBuilder.ToString().StartsWith("000000"))
                {
                    Console.WriteLine("Day4 Answer: " + number);
                    break;
                }

            }
        }
        static void Day5()
        {
            string[] input = System.IO.File.ReadAllLines("day5input.txt");
            //string[] input = { "ieodomkazucvgmuy" };
            string vowels = "aeiou";
            string[] naughtyStrings = { "ab", "cd", "pq", "xy" };

            int numNice = 0;
            foreach (string line in input)
            {
                bool niceString = false;
                for (int i = 0; i < line.Length - 1; i++)
                {
                    if (line[i] == line[i + 1])
                    {
                        niceString = true;
                    }
                }
                int numVowels = line.Sum(c => Convert.ToInt32(vowels.Contains(c)));
                if (numVowels < 3)
                {
                    niceString = false;
                }
                foreach (string s in naughtyStrings)
                {
                    if (line.Contains(s))
                    {
                        niceString = false;
                    }
                }
                if (niceString)
                {
                    numNice++;
                }
            }
            Console.WriteLine("Nice strings, part 1: " + numNice);
            numNice = 0;
            foreach (string line in input)
            {
                bool niceString = false;
                for (int i = 0; i < line.Length - 3; i++)
                {
                    string pair = line.Substring(i, 2);
                    if (line.LastIndexOf(pair) > i + 1)
                    {
                        niceString = true;
                    }
                }
                bool niceString2 = false;
                for (int i = 0; i < line.Length - 2; i++)
                {
                    if (line[i] == line[i + 2] && line[i] != line[i + 1])
                    {
                        niceString2 = true;
                    }
                }
                if (niceString && niceString2)
                {
                    numNice++;
                }
            }
            Console.WriteLine("Nice strings, part 2: " + numNice);

        }
    }
}