using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Adventofcode
{
    class Day17
    {
        public static void Run()
        {
            var input = System.IO.File.ReadAllLines("day17input.txt");
            //string[] input = new string[] { "1", "2", "3", "4", "5" };
            var containers = new int[input.Length];

            for (int i = 0; i < input.Length; i++)
            {
                containers[i] = int.Parse(input[i]);
            }

            //var combination = Enumerable.Range(0, 3).ToArray();
            /*do
            {
                foreach (int c in combination)
                {
                    Console.Write(c + " ");
                }
                Console.WriteLine("");
            } while (nextCombination(combination, containers.Length));
            */

            int numValidCombinations = 0;
            for(int numContainers = 1; numContainers < containers.Length; numContainers++)
            {
                //Console.WriteLine("numContainers: " + numContainers);
                var combination = Enumerable.Range(0, numContainers).ToArray();
                do
                {
                    if(testCombination(combination, containers))
                    {
                        numValidCombinations++;
                    }
                } while (nextCombination(combination, containers.Length));
            }

            Console.WriteLine("Number of combinations: " + numValidCombinations);

            numValidCombinations = 0;
            for (int numContainers = 1; numContainers < containers.Length; numContainers++)
            {
                //Console.WriteLine("numContainers: " + numContainers);
                var combination = Enumerable.Range(0, numContainers).ToArray();
                do
                {
                    if (testCombination(combination, containers))
                    {
                        numValidCombinations++;
                    }
                } while (nextCombination(combination, containers.Length));
                if (numValidCombinations > 0) break;
            }
            Console.WriteLine("Part two, number of combinations: " + numValidCombinations);
        }


        public static bool testCombination(int[] combination, int[] containers, int desiredSum = 150)
        {
            int sum = 0;
            foreach (int c in combination)
            {
                sum += containers[c];
            }
            if (sum == desiredSum) return true;
            else return false;
        }


        // Gå igenom alla kombinationer
        public static bool nextCombination(int[] combination, int numContainers)
        {
            if(combination[0] == numContainers - combination.Length)
            {
                return false;
            }

            int indexToIncrease = combination.Length - 1;
            int toChange = numContainers - 1;
            // Gå bakifrån och hitta vilka som ska ändras
            while(combination[indexToIncrease] == toChange)
            {
                indexToIncrease--;
                toChange--;
            }

            combination[indexToIncrease]++;
            for(int i = indexToIncrease + 1, j = combination[indexToIncrease] + 1; i < combination.Length; i++)
            {
                combination[i] = j++;
            }
                        
            return true;
        }
    }
}
