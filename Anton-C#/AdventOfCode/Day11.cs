using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Adventofcode
{
    class Day11
    {
        static string IncrementPassword(string p)
        {
            var sb = new StringBuilder(p);
            int index = sb.Length - 1;
            bool overflows;
            do
            {
                if (sb[index] == 'z')
                {
                    sb[index] = 'a';
                    overflows = true;
                    index--;
                }
                else
                {
                    sb[index]++;
                    overflows = false;
                }

            } while (overflows);

            return sb.ToString();
        }
        static bool TestStraight(string s)
        {
            int count = 1;
            for(int i = 0; i < s.Length - 1; i++)
            {
                if (s[i + 1] - s[i] == 1) {
                    count++;
                }
                else count = 1;
                
                if (count == 3)
                {
                    return true;
                }
            }
            return false;
        }
        static bool TestPairs(string s)
        {
            int numPairs = 0;
            for(int i = 0; i < s.Length - 1; i++)
            {
                if(s[i] == s[i + 1])
                {
                    i++;
                    numPairs++;
                }
            }
            if (numPairs >= 2) return true;

            return false;
        }
        static string NextPassword(string password)
        {
            var currentString = password;
            while (true)
            {
                currentString = IncrementPassword(currentString);
                if (currentString.IndexOfAny(new char[] { 'i', 'o', 'l' }) == -1 &&
                    TestStraight(currentString) &&
                    TestPairs(currentString))
                {
                    return currentString; ;
                }
            }
        }

        public static void Run()
        {
            var input = "hxbxwxba";
            var newPassword = NextPassword(input);
            Console.WriteLine(newPassword);
            newPassword = NextPassword(newPassword);
            Console.WriteLine(newPassword);
        } 
    }
}
