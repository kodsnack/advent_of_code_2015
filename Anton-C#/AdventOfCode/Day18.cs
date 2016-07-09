using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day18
    {
        public static bool isOn(bool[,] currentField, int row, int col)
        {
            if (row < 0 ||
                row >= currentField.GetLength(0) ||
                col < 0 ||
                col >= currentField.GetLength(1))
            {
                return false;
            }
            else return currentField[row, col];
            
        }

        public static int countNeighbors(bool[,] currentField, int row, int col)
        {
            int numNeighbors = 0;
            for(int r = row - 1; r <= row + 1; r++)
            {
                for (int c = col - 1; c <= col + 1; c++)
                {
                    if (!(r == row && c == col) && isOn(currentField, r, c))
                    {
                        numNeighbors++;
                    }  
                }
            }
            return numNeighbors;
        }

        public static void printField(bool[,] field)
        {

            for (int row = 0; row < field.GetLength(0); row++)
            {
                for (int col = 0; col < field.GetLength(1); col++)
                {
                    Console.Write(field[row, col] ? '#' : '.' );
                }
                Console.WriteLine();
            }
        }

        public static void Run()
        {
            var input = System.IO.File.ReadAllLines("day18input.txt");
            //string[] input = new string[] { ".#.#.#", "...##.", "#....#", "..#...", "#.#..#", "####.." };
            int numRows = input.Length;
            int numCols = input[0].Length;
            const int numSteps = 100;

            bool[,] currentField = new bool[numRows, numCols];
            bool[,] updatedField = new bool[numRows, numCols];

            for (int row = 0; row < numRows; row++)
            {
                for(int col = 0; col < numCols; col++)
                {
                    currentField[row, col] = (input[row][col] == '#');
                }
            }

            bool[,] initialField = (bool[,]) currentField.Clone();
            //printField(currentField);
            for (int i = 0; i < numSteps; i++)
            {
                //Console.WriteLine("step: " + i);
                for (int row = 0; row < numRows; row++)
                {
                    for (int col = 0; col < numCols; col++)
                    {
                        int numLit = countNeighbors(currentField, row, col);
                        if (numLit == 3)
                        {
                            updatedField[row, col] = true;
                        } else if (numLit == 2 || numLit == 3) 
                        {
                            updatedField[row, col] = currentField[row, col];
                        } else
                        {
                            updatedField[row, col] = false;
                        }
                    }
                }
                var temp = currentField;
                currentField = updatedField;
                updatedField = temp;
                //printField(currentField);
            }
            Console.WriteLine("Num lit: " + currentField.Cast<bool>().Sum(b => Convert.ToInt32(b)));

            /*
             * Part 2
             */
            currentField = initialField;
            for (int i = 0; i < numSteps; i++)
            {
                // Tänd lamporna i hörnen
                currentField[0, 0] = true;
                currentField[0, numCols - 1] = true;
                currentField[numRows - 1, 0] = true;
                currentField[numRows - 1, numCols - 1] = true;

                for (int row = 0; row < numRows; row++)
                {
                    for (int col = 0; col < numCols; col++)
                    {
                        int numLit = countNeighbors(currentField, row, col);
                        if (numLit == 3)
                        {
                            updatedField[row, col] = true;
                        }
                        else if (numLit == 2 || numLit == 3)
                        {
                            updatedField[row, col] = currentField[row, col];
                        }
                        else
                        {
                            updatedField[row, col] = false;
                        }
                    }
                }
                var temp = currentField;
                currentField = updatedField;
                updatedField = temp;
            }
            currentField[0, 0] = true;
            currentField[0, numCols - 1] = true;
            currentField[numRows - 1, 0] = true;
            currentField[numRows - 1, numCols - 1] = true;
            Console.WriteLine("Num lit: " + currentField.Cast<bool>().Sum(b => Convert.ToInt32(b)));
        }
    }
}
