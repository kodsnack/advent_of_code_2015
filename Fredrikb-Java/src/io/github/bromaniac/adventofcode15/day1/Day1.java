package io.github.bromaniac.adventofcode15.day1;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URL;
import java.util.Scanner;

public class Day1 {
    public static void main (String[] args) {
        long floor = 0;
        String line = "";

        try {
            URL path = Day1.class.getResource("input.txt");
            Scanner input = new Scanner(new File(path.getFile()));

            while (input.hasNextLine()) {
                line += input.nextLine();
            }

            char[] charArray = line.toCharArray();

            for (char c : charArray) {
                if (c == '(') {
                    floor++;
                } else if (c == ')') {
                    floor--;
                }  else {
                    throw new IllegalArgumentException() ;
                }
            }

        } catch (FileNotFoundException|IllegalArgumentException e) {
            e.printStackTrace();
        }

        System.out.println("Floor: " + floor);

    }
}