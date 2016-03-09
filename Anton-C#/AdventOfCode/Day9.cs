using System;
using System.Collections.Generic;
using System.Linq;

using System.Text;
using System.Threading.Tasks;

namespace Adventofcode
{
    class Day9
    {
        static Dictionary<string, int> distances;
        static HashSet<String> cities;

        static bool nextPermutation(List<string> route)
        {
            int k = route.Count - 2;
            for (; k >= 0; k--)
            {
                if(String.Compare(route[k], route[k + 1]) == -1)
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
        static int computeDistance(List<string> route)
        {
            int distance = 0;
            for(int i = 0; i < route.Count - 1; i++)
            {
                distance += distances[route[i] + " to " + route[i + 1]];
            }
            return distance;
        }

        public static void Run()
        {
            string[] input = System.IO.File.ReadAllLines("day9input.txt");
            /*string[] input = {"London to Dublin = 464",
                    "London to Belfast = 518",
                    "Dublin to Belfast = 141"};*/
            cities = new HashSet<String>();
            distances = new Dictionary<string, int>();
            foreach (var line in input)
            {
                var parts = line.Split(new string[] { " to ", " = " }, StringSplitOptions.None);
                string city1 = parts[0];
                string city2 = parts[1];
                int distance = int.Parse(parts[2]);
                cities.Add(city1);
                cities.Add(city2);
                distances.Add(city1 + " to " + city2, distance);
                distances.Add(city2 + " to " + city1, distance);
            }

            var testRoute = new List<string>(cities);
            testRoute.Sort();
            int minDistance = computeDistance(testRoute);
            int maxDistance = minDistance;
            while (nextPermutation(testRoute))
            {
                int distance = computeDistance(testRoute);
                if (distance < minDistance) minDistance = distance;
                if (distance > maxDistance) maxDistance = distance;
            }

            Console.WriteLine("Min distance: " + minDistance);
            Console.WriteLine("Max distance: " + maxDistance);

        }
    }
}
