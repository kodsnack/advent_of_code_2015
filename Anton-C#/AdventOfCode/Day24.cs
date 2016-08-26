using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day24
    {
        public static void Run()
        {
            var lines = System.IO.File.ReadAllLines("day24input.txt");
            var presents = new int[lines.Length];
            for (int i = 0; i < lines.Length; i++)
            {
                presents[i] = int.Parse(lines[i]);
            }
            //presents = new int[] { 1, 2, 3, 4, 5, 7, 8, 9, 10, 11 };
            int totalWeight = presents.Sum();
            int numPresents = presents.Length;
            /*
             * PART 1
             */
            int groupWeight = totalWeight / 3;
            List<long> solutions = new List<long>();
            bool solutionFound = false;
            int groupSize1 = 1;
            while (!solutionFound)
            {
                List<int[]> testGroups1 = findGroups(presents, groupSize1, groupWeight);
                foreach (var group1 in testGroups1)
                {
                    var remainingPresents = new int[numPresents - group1.Length];
                    for (int i = 0, presentsAdded = 0; i < numPresents; i++)
                    {
                        if (!group1.Contains(i))
                        {
                            remainingPresents[presentsAdded] = presents[i];
                            presentsAdded++;
                        }
                    }
                    for (int groupSize2 = group1.Length; groupSize2 <= remainingPresents.Length / 2; groupSize2++)
                    {
                        List<int[]> testGroups2 = findGroups(remainingPresents, groupSize2, groupWeight, true);
                        if (testGroups2.Count > 0)
                        {
                            solutions.Add(getQE(presents, group1));

                            //PrintCombination(presents, group1);
                            //PrintCombination(remainingPresents, testGroups2[0]);
                            //Console.WriteLine("");
                            solutionFound = true;
                            goto PART1_SOLUTION_FOUND;
                        }
                    }
                }
                groupSize1++;
            }
            PART1_SOLUTION_FOUND:
            solutions.Sort();
            Console.WriteLine("Solution, part 1: " + solutions.First());

            /*
             * PART 2, klippt och klistrat från part 1, inte så vackert!
             */
            groupWeight = totalWeight / 4;
            solutions = new List<long>();
            solutionFound = false;
            groupSize1 = 1;
            while (!solutionFound)
            {
                List<int[]> testGroups1 = findGroups(presents, groupSize1, groupWeight);
                foreach (var group1 in testGroups1)
                {
                    var remainingPresents = new int[numPresents - group1.Length];
                    for (int i = 0, presentsAdded = 0; i < numPresents; i++)
                    {
                        if (!group1.Contains(i))
                        {
                            remainingPresents[presentsAdded] = presents[i];
                            presentsAdded++;
                        }
                    }
                    for (int groupSize2 = group1.Length; groupSize2 <= remainingPresents.Length / 3; groupSize2++)
                    {
                        List<int[]> testGroups2 = findGroups(remainingPresents, groupSize2, groupWeight, false);
                        if (testGroups2.Count > 0)
                        {
                            foreach (var group2 in testGroups2)
                            {
                                var remainingPresents2 = new int[numPresents - group1.Length - group2.Length];
                                for (int i = 0, presentsAdded = 0; i < numPresents; i++)
                                {
                                    if (!(group1.Contains(i) || group2.Contains(i)))
                                    {
                                        remainingPresents[presentsAdded] = presents[i];
                                        presentsAdded++;
                                    }
                                }
                                for (int groupSize3 = group1.Length; groupSize2 <= remainingPresents.Length / 3; groupSize2++)
                                {
                                    List<int[]> testGroups3 = findGroups(remainingPresents, groupSize2, groupWeight, true);
                                    if (testGroups2.Count > 0)
                                    {
                                        solutions.Add(getQE(presents, group1));
                                        //PrintCombination(presents, group1);
                                        //PrintCombination(remainingPresents, testGroups2[0]);
                                        //Console.WriteLine("");
                                        solutionFound = true;
                                        goto PART2_SOLUTION_FOUND;
                                    }
                                }
                            }
                        }
                    }
                }
                groupSize1++;
            }
            PART2_SOLUTION_FOUND:
            Console.WriteLine("Solution, part 2: " + solutions.First());
        }
        private static bool checkWeight(int[] presents, int[] combination, int weight)
        {
            int sum = 0;
            for (int i = 0; i < combination.Length; i++)
            {
                sum += presents[combination[i]];
            }
            if (sum == weight)
                return true;
            else return false;
        }

        // Hitta grupper med en viss sammanlagd massa
        private static List<int[]> findGroups(int[] presents, int numPresents, int groupWeight, bool returnOnFirst = false)
        {
            var groups = new List<int[]>();
            var combination = firstCombination(numPresents);
            if (checkWeight(presents, combination, groupWeight))
            {
                groups.Add((int[])combination.Clone());
                if (returnOnFirst) return groups;
            }
            while(nextCombination(combination, presents.Length))
            {
                if (checkWeight(presents, combination, groupWeight))
                {
                    groups.Add((int[])combination.Clone());
                    if (returnOnFirst) return groups;
                }
            }
            return groups;
        }
        
        // Första kombinationen
        private static int[] firstCombination(int numElements)
        {
            var combination = new int[numElements];
            for(int i = 0; i < numElements; i++)
            {
                combination[i] = i;
            }
            return combination;
        }

        private static void PrintCombinations(int[] presents, List<int[]> combinations)
        {
            foreach (var combination in combinations)
            {
                PrintCombination(presents, combination);
            }
        }

        private static void PrintCombination(int[] presents, int[] combination)
        {
            long quantumEntanglement = 1;
            foreach (int i in combination)
            {
                quantumEntanglement *= presents[i];
                Console.Write(presents[i] + " ");
            }
            Console.WriteLine(", QE: " + quantumEntanglement);
        }

        static long getQE(int[] presents, int[] combination)
        {
            long quantumEntanglement = 1;
            foreach (int i in combination)
            {
                quantumEntanglement *= presents[i];
            }
            return quantumEntanglement;
        }

        // Gå igenom alla kombinationer, från Day17
        public static bool nextCombination(int[] combination, int numContainers)
        {
            if (combination[0] == numContainers - combination.Length)
            {
                return false;
            }

            int indexToIncrease = combination.Length - 1;
            int toChange = numContainers - 1;
            // Gå bakifrån och hitta vilka som ska ändras
            while (combination[indexToIncrease] == toChange)
            {
                indexToIncrease--;
                toChange--;
            }

            combination[indexToIncrease]++;
            for (int i = indexToIncrease + 1, j = combination[indexToIncrease] + 1; i < combination.Length; i++)
            {
                combination[i] = j++;
            }

            return true;
        }
    }
}

