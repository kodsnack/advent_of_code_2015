using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day14
    {
        class Reindeer
        {
            string name;
            int speed;
            int flyDuration, restDuration;
            public int distance, score;
            int flyingCounter, restingCounter;

            public Reindeer(string line)
            {
                var parts = line.Split();
                name = parts[0];
                speed = int.Parse(parts[3]);
                flyDuration = int.Parse(parts[6]);
                restDuration = int.Parse(parts[13]);
                distance = 0;
                flyingCounter = flyDuration;
                restingCounter = 0;

            }
            public void tick()
            {
                if(restingCounter > 0)
                {
                    restingCounter--;
                    if (restingCounter == 0)
                    {
                        flyingCounter = flyDuration;
                    }
                } else if(flyingCounter > 0)
                {
                    flyingCounter--;
                    distance += speed;
                    if (flyingCounter == 0)
                    {
                        restingCounter = restDuration;
                    }
                }
            }
        }

        public static void Run()
        {
            string[] input = System.IO.File.ReadAllLines("day14input.txt");
            /*
            string[] input = { "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.",
                            "Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds."};
            */
            List<Reindeer> reindeers = new List<Reindeer>();
            foreach(var line in input)
            {
                reindeers.Add(new Reindeer(line));
            }
            const int numSeconds = 2503;
            for(int i = 0; i < numSeconds; i++)
            {
                foreach(var r in reindeers)
                {
                    r.tick();
                }
                int currentMaxDistance = reindeers.Max(r => r.distance);
                foreach (var r in reindeers)
                {
                    if (r.distance == currentMaxDistance) r.score++;
                }
            }
            int maxDistance = reindeers.Max(r => r.distance);
            Console.WriteLine("Max distance: " + maxDistance);

            int maxScore = reindeers.Max(r => r.score);
            Console.WriteLine("Max score: " + maxScore);
        }
    }
}
