using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day12
    {
        public static void Run()
        {

            var input = System.IO.File.ReadAllText("day12input.txt");
            Console.WriteLine("sum: " + SumAllNumbers(input));
            Console.WriteLine("sum, part 2: " + ComputeSum(input));
        }

        public static string SumAllNumbers(string s)
        {
            var matches = System.Text.RegularExpressions.Regex.Matches(s, "[-]*[0-9][0-9]*");
            int sum = 0;
            foreach (System.Text.RegularExpressions.Match match in matches)
            {
                sum += int.Parse(match.Value);
            }
            return sum.ToString();
        }

        static string ComputeSum(string s)
        {
            var sb = new StringBuilder();
            int currentIndex = 0;
            while (currentIndex < s.Length)
            {
                int openingBraceIndex = s.IndexOf('{', currentIndex);
                if (openingBraceIndex == -1)
                {
                    sb.Append(s, currentIndex, s.Length - currentIndex);
                    currentIndex = s.Length;
                } else
                {
                    sb.Append(s, currentIndex, openingBraceIndex - currentIndex);
                    int closingBraceIndex = FindClosingBrace(s, openingBraceIndex);
                
                    sb.Append(ComputeSum(s.Substring(openingBraceIndex + 1, closingBraceIndex - openingBraceIndex - 1)));
                    currentIndex = closingBraceIndex + 1;
                }
            }
            if (sb.ToString().Contains(":\"red\""))
            {
                return "";
            }

            return SumAllNumbers(sb.ToString());
        }

        static int FindClosingBrace(string s, int openingBraceIndex)
        {
            int closingBraceIndex;
            int depth = 0;
            for (closingBraceIndex = openingBraceIndex + 1; ; closingBraceIndex++)
            {
                if (s[closingBraceIndex] == '{')
                {
                    depth++;
                }
                else if (s[closingBraceIndex] == '}')
                {
                    depth--;
                    if (depth == -1)
                    {
                        break;
                    }
                }
            }
            return closingBraceIndex;
        }
    }
}
