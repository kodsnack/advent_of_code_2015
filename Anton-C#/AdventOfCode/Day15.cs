using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day15
    {
        class Ingredient
        {
            public int capacity, durability, flavor, texture, calories;
            public Ingredient(string line)
            {
                var parts = line.Split(new char[] { ' ', ',' }, StringSplitOptions.RemoveEmptyEntries);
                capacity = int.Parse(parts[2]);
                durability = int.Parse(parts[4]);
                flavor = int.Parse(parts[6]);
                texture = int.Parse(parts[8]);
                calories = int.Parse(parts[10]);
            }
        }
        // Gå igenom alla kombinationer
        public static bool nextRecipe(int[] recipe)
        {
            if (recipe.Last() == recipe.Sum())
            {
                return false;
            }

            int lastNonZero = recipe.Length - 1;
            while(recipe[lastNonZero] == 0)
            {
                lastNonZero--;
            }
            if(lastNonZero != recipe.Length - 1)
            {
                recipe[lastNonZero]--;
                recipe[lastNonZero + 1]++;
            } else
            {
                int nextLastNonZero = lastNonZero - 1;
                while(recipe[nextLastNonZero] == 0)
                {
                    nextLastNonZero--;
                }
                int numAtLastNonZero = recipe[lastNonZero];
                recipe[lastNonZero] = 0;
                recipe[nextLastNonZero + 1] = numAtLastNonZero + 1;
                recipe[nextLastNonZero]--;
            }
            return true;
        }

        static int computeScore(Ingredient[] ingredients, int[] recipe, bool countCalories = false)
        {
            int capacity = 0, durability = 0, flavor = 0, texture = 0, calories = 0;
            for(int i = 0; i < recipe.Length; i++)
            {
                capacity += ingredients[i].capacity * recipe[i];
                durability += ingredients[i].durability * recipe[i];
                flavor += ingredients[i].flavor * recipe[i];
                texture += ingredients[i].texture * recipe[i];
                calories += ingredients[i].calories * recipe[i];
            }
            if (countCalories && calories != 500) return 0;

            if (capacity <= 0) return 0;
            if (durability <= 0) return 0;
            if (flavor <= 0) return 0;
            if (texture <= 0) return 0;

            return capacity * durability * flavor * texture;
        }

        public static void Run()
        {
            string[] input = System.IO.File.ReadAllLines("day15input.txt");
            
            /*string[] input = { "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8",
                               "Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3"};
                               */
            Ingredient[] ingredients = new Ingredient[input.Length];
            for(int i = 0; i < input.Length; i++)
            {
                ingredients[i] = new Ingredient(input[i]);
            }

            int[] recipe = new int[ingredients.Length];
            recipe[0] = 100;
            int highestScore = computeScore(ingredients, recipe);
            while (nextRecipe(recipe))
            {
                int score = computeScore(ingredients, recipe);
                if (score > highestScore) highestScore = score;
            }
            Console.WriteLine("Highest score: " + highestScore);

            recipe = new int[ingredients.Length];
            recipe[0] = 100;
            highestScore = computeScore(ingredients, recipe, true);
            while (nextRecipe(recipe))
            {
                int score = computeScore(ingredients, recipe, true);
                if (score > highestScore) highestScore = score;
            }
            Console.WriteLine("Highest score, counting calories: " + highestScore);

        }
    }
}
