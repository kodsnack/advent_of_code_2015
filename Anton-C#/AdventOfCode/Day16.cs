using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Adventofcode
{
    partial class Program
    {
        public static void Day16()
        {
            var input = System.IO.File.ReadAllLines("day16input.txt");
            var tickerTape = System.IO.File.ReadAllLines("day16ticker.txt");

            var auntsMap = new Dictionary<string, int>[input.Length];
            for (int i = 0; i < input.Length; i++)
            {
                var line = input[i];
                var compounds = line.Substring(line.IndexOf(':') + 2);
                auntsMap[i] = new Dictionary<string, int>();
                foreach (var compound in compounds.Split(new char[] { ',' }))
                {
                    var parts = compound.Split(new char[] { ':' });
                    auntsMap[i][parts[0].Trim()] = int.Parse(parts[1]);
                }
            }

            var tickerMap = new Dictionary<string, int>();
            foreach (var line in tickerTape)
            {
                var parts = line.Split(new char[] { ':' });
                tickerMap[parts[0].Trim()] = int.Parse(parts[1]);
            }

            bool matchFound = false;
            int auntNumber = 0;
            while (!matchFound)
            {
                matchFound = true;
                foreach (var pair in tickerMap)
                {
                    string key = pair.Key;
                    int value = pair.Value;

                    if (auntsMap[auntNumber].ContainsKey(key))
                    {
                        if (auntsMap[auntNumber][key] != value)
                        {
                            matchFound = false;
                            break;
                        }
                    }
                }
                auntNumber++;
            }
            Console.WriteLine("Aunt number " + auntNumber);

            matchFound = false;
            auntNumber = 0;
            while (!matchFound)
            {
                matchFound = true;
                foreach (var pair in tickerMap)
                {
                    string key = pair.Key;
                    int value = pair.Value;

                    if (auntsMap[auntNumber].ContainsKey(key))
                    {
                        if (key == "cats" || key == "trees")
                        {
                            if (auntsMap[auntNumber][key] <= value)
                            {
                                matchFound = false;
                                break;
                            }
                        }
                        else if (key == "pomeranians" || key == "goldfish")
                        {
                            if (auntsMap[auntNumber][key] >= value)
                            {
                                matchFound = false;
                                break;
                            }
                        }
                        else if (auntsMap[auntNumber][key] != value)
                        {
                            matchFound = false;
                            break;
                        }
                    }
                }
                auntNumber++;
            }
            Console.WriteLine("Part two, aunt number " + auntNumber);

        }
    }
}
