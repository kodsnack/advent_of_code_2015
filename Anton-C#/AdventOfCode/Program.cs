using System;
using System.Linq;

namespace Adventofcode
{
    partial class Program
    {
        static void Main(string[] args)
        {
            /*Day1();
            Day2();
            Day3();
            Day4();
            Day5();
            Day6.Run();
            Day7.Run();
            Day8();*/
            //Day9.Run();
            //Day10();
            //Day11.Run();
            Day12.Run();
            Console.WriteLine("Färdig!");
            Console.ReadKey();

        }        

        static void Day8()
        {
            var input = System.IO.File.ReadAllLines("day8input.txt");
            //var input = new string[] { "\"\"",  "\"abc\"", "\"aaa\\\"aaa\"", "\"\\x27\"" };
            int charCount = 0;
            foreach(string line in input)
            {
                for (int i = 1; i < line.Length - 1; i++)
                {
                    if (line[i] == '\\') {
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
            for(int i = 0; i < NumLoops; i++)
            {
                sb.Clear();
                for (int j = 0; j < currentString.Length; j++)
                {
                    int count = 1;
                    char currentDigit = currentString[j];
                    while(j < currentString.Length - 1 && currentString[j+1] == currentDigit)
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

    }
}
