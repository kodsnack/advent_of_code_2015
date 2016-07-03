using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Adventofcode
{
    class Day19
    {
        /*
         * CRAPPY SOLUTION, DON'T LOOK!
         */
        static List<Tuple<string,string>> replacements;
        static int SPECIAL_VARIABLE = 8;
        static int maxMatching;
        static string medicineMolecule = "";

        // Generate new molecules
        static HashSet<string> GenerateMolecules(HashSet<string> currentMolecules, bool partTwo = false)
        {
            var newMolecules = new HashSet<string>();
            foreach (var r in replacements)
            {
                foreach (var molecule in currentMolecules)
                {
                    int index = 0;
                    int numMatching = GetNumMatching(molecule, medicineMolecule);

                    if (partTwo) { // don't search from the beginning
                        index = Math.Max(numMatching - SPECIAL_VARIABLE, 0);
                    }
                    
                    while (index != -1)
                    {
                        index = molecule.IndexOf(r.Item1, index);

                        // no point in doing replacements after the first character that doesn't match
                        if (partTwo && index > numMatching) 
                        {
                            index = -1;
                        }
                        if(index != -1)
                        {
                            // replace
                            string newMolecule = molecule.Substring(0, index) + 
                                                 r.Item2 + 
                                                 molecule.Substring(index + r.Item1.Length);
                            newMolecules.Add(newMolecule);
                            
                            index += r.Item1.Length;
                        }
                    }
                }
            }
            return newMolecules;
        }

        // For part 2, only keep the molecules with most matching characters
        static HashSet<string> filterMolecules(HashSet<string> currentMolecules, string target)
        {
            var newMolecules = new HashSet<string>();
            int maxMatching = 0;
            foreach(var m in currentMolecules)
            {
                int num = GetNumMatching(m, target);
                if (num > maxMatching) maxMatching = num;
            }

            foreach (var m in currentMolecules)
            {
                int num = GetNumMatching(m, target);
                if (maxMatching - num < SPECIAL_VARIABLE && m.Length <= target.Length) newMolecules.Add(m);
            }
            Console.WriteLine("Num matching " + maxMatching);
            Day19.maxMatching = maxMatching;
            return newMolecules;
        }

        // Number of matching characters
        static int GetNumMatching(string molecule, string target)
        {
            int numMatching = 0;
            while (numMatching < molecule.Length &&
                  numMatching < target.Length &&
                  molecule[numMatching] == target[numMatching])
            {
                numMatching++;
            }
            return numMatching;
        }

        public static void Run()
        {
            var lines = System.IO.File.ReadAllLines("day19input.txt");
            replacements = new List<Tuple<string, string>>();
            
            foreach (var line in lines)
            {
                if (line.Contains("=>"))
                {
                    var parts = line.Split(new string[] { " => " }, StringSplitOptions.None);
                    replacements.Add(new Tuple<string, string>(parts[0], parts[1]));
                } else if (line.Length > 0)
                {
                    medicineMolecule = line;
                }
            }
            var calibrationMolecules = new HashSet<string>();
            calibrationMolecules.Add(medicineMolecule);

            var newMolecules = GenerateMolecules(calibrationMolecules);
            Console.WriteLine("Number of different molecules: " + newMolecules.Count);

            int counter = 0;
            var currentMolecules = new HashSet<string>();
            currentMolecules.Add("e"); // initial molecule
            while (!currentMolecules.Contains(medicineMolecule))
            {
                counter++;
                currentMolecules = GenerateMolecules(currentMolecules, true);
                currentMolecules = filterMolecules(currentMolecules, medicineMolecule);
                Console.WriteLine("Step " + counter + ", number of molecules: " + currentMolecules.Count);
            }
            Console.WriteLine("Number of steps: " + counter);
        }
    }
   
}
