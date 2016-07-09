using System;
using System.Collections.Generic;

namespace Adventofcode
{
    class Day23
    {
        public static void Run()
        {
            var program = System.IO.File.ReadAllLines("day23input.txt");
            var registers = new Dictionary<string, uint>();

            // Part 1
            registers["a"] = 0;
            registers["b"] = 0;
            RunProgram(program, registers);
            Console.WriteLine("Part 1, value of register b: " + registers["b"]);

            // Part 2
            registers["a"] = 1;
            registers["b"] = 0;
            RunProgram(program, registers);
            Console.WriteLine("Part 2, value of register b: " + registers["b"]);
        }

        private static void RunProgram(string[] program, Dictionary<string, uint> registers)
        {
            uint PC = 0; // program counter
            bool running = true;
            while (running)
            {
                if (PC > program.Length - 1)
                {
                    running = false;
                }
                else
                {
                    var instruction = program[PC];
                    var parts = instruction.Split();
                    switch (parts[0])
                    {
                        case "hlf":
                            registers[parts[1]] = registers[parts[1]] / 2;
                            PC++;
                            break;
                        case "tpl":
                            registers[parts[1]] = registers[parts[1]] * 3;
                            PC++;
                            break;
                        case "inc":
                            registers[parts[1]]++;
                            PC++;
                            break;
                        case "jmp":
                            PC = (uint)((int)PC + int.Parse(parts[1]));
                            break;
                        case "jie":
                            string reg = new string(parts[1][0], 1);
                            if (registers[reg] % 2 == 0)
                            {
                                PC = (uint)((int)PC + int.Parse(parts[2]));
                            }
                            else
                            {
                                PC++;
                            }
                            break;
                        case "jio":
                            string reg2 = new string(parts[1][0], 1);
                            if (registers[reg2] == 1)
                            {
                                PC = (uint)((int)PC + int.Parse(parts[2]));
                            }
                            else
                            {
                                PC++;
                            }
                            break;
                    }
                }
            } // while (running)
        }
    }
}
