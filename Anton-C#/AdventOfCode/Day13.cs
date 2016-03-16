using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Adventofcode
{
    class Day13
    {
        static Dictionary<string, int> happinessModifiers;
        static HashSet<string> guests;
        public static void Run()
        {
            guests = new HashSet<string>();
            happinessModifiers = new Dictionary<string, int>();
            string[] input = System.IO.File.ReadAllLines("day13input.txt");
            foreach (var line in input)
            {
                var parts = line.Split();
                var guest1 = parts[0];
                var guest2 = parts[10].Trim(new char[] { '.' });
                int change;
                if (parts[2] == "gain")
                {
                    change = int.Parse(parts[3]);
                }
                else
                {
                    change = -int.Parse(parts[3]);
                }
                guests.Add(guest1);
                guests.Add(guest2);
                happinessModifiers[guest1 + " " + guest2] = change;
            }
            List<string> seating = new List<string>(guests);


            int maxHappiness = computeMaxHappiness(seating);
            System.Console.WriteLine("Max happiness modification: " + maxHappiness);

            seating.Insert(0, "Me");
            maxHappiness = computeMaxHappiness(seating);
            System.Console.WriteLine("Max happiness modification, with me: " + maxHappiness);
        }

        private static int computeMaxHappiness(List<string> seating)
        {
            seating.Sort();
            int maxHappiness = computeHappiness(seating);

            string firstGuest = seating[0];
            seating.RemoveAt(0);
            while (nextPermutation(seating))
            {
                seating.Insert(0, firstGuest);
                int happiness = computeHappiness(seating);
                if (happiness > maxHappiness) maxHappiness = happiness;
                seating.RemoveAt(0);
            }
            seating.Insert(0, firstGuest);
            return maxHappiness;
        }


        static int computeHappiness(List<string> seating)
        {
            int totalHappiness = 0;
            for(int i = 0; i < seating.Count - 1; i++)
            {
                totalHappiness += getHappinessModifier(seating[i], seating[i + 1]);
            }
            totalHappiness += getHappinessModifier(seating[0], seating.Last());

            return totalHappiness;
        }

        static int getHappinessModifier(string guest1, string guest2) 
        {
            if (guest1 == "Me" || guest2 == "Me") return 0;
            return (happinessModifiers[guest1 + " " + guest2] + happinessModifiers[guest2 + " " + guest1]);
        }

        // Recycled from Day9
        static bool nextPermutation(List<string> route)
        {
            int k = route.Count - 2;
            for (; k >= 0; k--)
            {
                if (String.Compare(route[k], route[k + 1]) == -1)
                {
                    break;
                }
            }
            if (k == -1) return false;
            int i = route.Count - 1;
            for (; i >= 0; i--)
            {
                if (String.Compare(route[k], route[i]) == -1)
                {
                    break;
                }
            }
            var temp = route[k];
            route[k] = route[i];
            route[i] = temp;
            var reversedPart = route.GetRange(k + 1, route.Count - k - 1);
            reversedPart.Reverse();
            route.RemoveRange(k + 1, route.Count - k - 1);
            route.AddRange(reversedPart);

            return true;
        }
    }
}
