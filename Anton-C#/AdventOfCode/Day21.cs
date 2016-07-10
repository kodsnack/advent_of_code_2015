using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day21
    {
        class Item
        {
            public string name;
            public int cost, damage, armor;
            override public string ToString()
            {
                return name;
            }
        }

        static List<List<Item>> Combine(List<List<Item>> combinations, List<Item> items)
        {
            var newCombinations = new List<List<Item>>();
            foreach(var combination in combinations)
            {
                foreach(var item in items)
                {
                    var newCombination = new List<Item>(combination);
                    newCombination.Add(item);
                    newCombinations.Add(newCombination);
                }
            }


            return newCombinations;
        }

        public static void Run()
        {
            var weapons = new List<Item>()
            {
                new Item {name = "Dagger",      cost = 8,   damage = 4 },
                new Item {name = "Shortsword",  cost = 10,  damage = 5 },
                new Item {name = "Warhammer",   cost = 25,  damage = 6 },
                new Item {name = "Longsword",   cost = 40,  damage = 7 },
                new Item {name = "Dagger",      cost = 74,  damage = 8 },
            };

            var armorList = new List<Item>()
            {
                new Item {name = "No armor" },
                new Item {name = "Leather",     cost = 13,  armor = 1 },
                new Item {name = "Chainmail",   cost = 31,  armor = 2 },
                new Item {name = "Splintmail",  cost = 53,  armor = 3 },
                new Item {name = "Bandemail",   cost = 75,  armor = 4 },
                new Item {name = "Platemail",   cost = 102, armor = 5 },
            };

            var rings = new List<Item>()
            {
                new Item {name = "No ring" },
                new Item {name = "Dam +1",  cost = 25,  damage = 1 },
                new Item {name = "Dam +2",  cost = 50,  damage = 2 },
                new Item {name = "Dam +3",  cost = 100, damage = 3 },
                new Item {name = "Def +1",  cost = 20,  armor = 1 },
                new Item {name = "Def +2",  cost = 40,  armor = 2 },
                new Item {name = "Def +3",  cost = 80,  armor = 3 }
            };

            var combinations = new List<List<Item>>();
            combinations.Add(new List<Item>());
            
            combinations = Combine(combinations, rings);
            combinations = Combine(combinations, rings);

            // Remove combinations with duplicate ring
            combinations.RemoveAll(c => c[0] == c[1] && c[0].name != "No ring");
            combinations = Combine(combinations, weapons);
            combinations = Combine(combinations, armorList);

            // puzzle input
            int bossStartingHP = 103;
            int bossDamage = 9;
            int bossArmor = 2;

            int lowestCost = 10000;
            int highestCost = 0;
            foreach(var combination in combinations)
            {
                int damage = combination.Sum(c => c.damage);
                int armor = combination.Sum(c => c.armor);
                int cost = combination.Sum(c => c.cost);

                int bossHP = bossStartingHP;
                int playerHP = 100;
                while (true)
                {
                    bossHP -= Math.Max(damage - bossArmor, 1);
                    if(bossHP <= 0)
                    {
                        lowestCost = Math.Min(lowestCost, cost);
                        break;
                    }
                    playerHP -= Math.Max(bossDamage - armor, 1);
                    if(playerHP <= 0)
                    {
                        highestCost = Math.Max(highestCost, cost);
                        break;
                    }
                }

            }
            Console.WriteLine("Lowest cost: " + lowestCost);
            Console.WriteLine("Highest cost: " + highestCost);

        }
    }
}
