using System;
using System.Collections.Generic;

namespace AdventOfCode
{
    class Day7
    {
        static Dictionary<string, ushort> wires;

        static bool parseValue(string identifier, out ushort value)
        {
            value = 0;
            if (ushort.TryParse(identifier, out value))
            {
                return true;
            }
            else if (wires.ContainsKey(identifier))
            {
                value = wires[identifier];
                return true;
            }
            else
            {
                return false;
            }
        }


        public static void Run()
        {
            string[] input = System.IO.File.ReadAllLines("day7input.txt");
            /*string[] input = {"123 -> x",
                            "456 -> y",
                            "x AND y -> d",
                            "x OR y -> e",
                            "x LSHIFT 2 -> f",
                            "y RSHIFT 2 -> g",
                            "NOT x -> h",
                            "NOT y -> i"};*/
            wires = new Dictionary<string, ushort>();
            ComputeCircuit(input);
            Console.WriteLine("a: " + wires["a"]);

            for (int i = 0; i < input.Length; i++)
            {
                if (input[i].EndsWith("-> b"))
                {
                    input[i] = wires["a"] + " -> b";
                }
            }

            wires.Clear();
            ComputeCircuit(input);
            Console.WriteLine("a: " + wires["a"]);
        }

        private static void ComputeCircuit(string[] input)
        {
            bool allDone = false;
            while (!allDone)
            {
                allDone = true;
                foreach (var line in input)
                {
                    var parts = line.Split(new string[] { " -> " }, StringSplitOptions.None);
                    string outWire = parts[1];
                    ushort inWire1, inWire2;
                    if (parts[0].Contains("AND"))
                    {
                        var inputWires = parts[0].Split(new string[] { " AND " }, StringSplitOptions.None);
                        if (parseValue(inputWires[0], out inWire1) && parseValue(inputWires[1], out inWire2))
                        {
                            wires[outWire] = (ushort)(inWire1 & inWire2);
                        }
                        else allDone = false;
                    }
                    else if (parts[0].Contains("OR"))
                    {
                        var inputWires = parts[0].Split(new string[] { " OR " }, StringSplitOptions.None);
                        if (parseValue(inputWires[0], out inWire1) && parseValue(inputWires[1], out inWire2))
                        {
                            wires[outWire] = (ushort)(inWire1 | inWire2);
                        }
                        else allDone = false;
                    }
                    else if (parts[0].Contains("LSHIFT"))
                    {
                        var inputs = parts[0].Split(new string[] { " LSHIFT " }, StringSplitOptions.None);
                        if (parseValue(inputs[0], out inWire1))
                        {
                            wires[outWire] = (ushort)(inWire1 << ushort.Parse(inputs[1]));
                        }
                        else allDone = false;
                    }
                    else if (parts[0].Contains("RSHIFT"))
                    {
                        var inputs = parts[0].Split(new string[] { " RSHIFT " }, StringSplitOptions.None);
                        if (parseValue(inputs[0], out inWire1))
                        {
                            wires[outWire] = (ushort)(inWire1 >> ushort.Parse(inputs[1]));
                        }
                        else allDone = false;
                    }
                    else if (parts[0].Contains("NOT"))
                    {
                        var inputs = parts[0].Split(' ');
                        if (parseValue(inputs[1], out inWire1))
                        {
                            wires[outWire] = (ushort)(~inWire1);
                        }
                        else allDone = false;
                    }
                    else
                    {
                        if (parseValue(parts[0], out inWire1))
                        {
                            wires[outWire] = inWire1;
                        }
                        else allDone = false;
                    }
                } //foreach
            }
        }
    }
}
