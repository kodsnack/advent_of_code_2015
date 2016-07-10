using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AdventOfCode
{
    class Day25
    {
        public static void Run()
        {
            int code = 20151125; // First code
            int row = 1, col = 1;

            int targetRow = 2981;
            int targetCol = 3075;

           // targetRow = targetCol = 5;

            while(!(row == targetRow && col == targetCol))
            {
                if(row == 1)
                {
                    row = col + 1;
                    col = 1;
                } else
                {
                    row--;
                    col++;
                }
                code = NextCode(code);
            }
            Console.WriteLine("Code at row " + row + ", col " + col + ": " + code);
        }

        static int NextCode(int previousCode)
        {
            return checked((int) ((previousCode * 252533L) % 33554393));
        }
    }
}
