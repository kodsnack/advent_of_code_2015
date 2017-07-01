package io.github.bromaniac.adventofcode15.day2;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URL;
import java.util.Arrays;
import java.util.Scanner;

public class Day2 {
    public static void main (String[] args) {
        int squareFeetNeeded = 0;

        try {
            URL path = Day2.class.getResource("input.txt");
            Scanner input = new Scanner(new File(path.getFile()));

            while (input.hasNextLine()) {
                String[] nums = input.nextLine().split("x");

                int l = Integer.parseInt(nums[0]);
                int w = Integer.parseInt(nums[1]);
                int h = Integer.parseInt(nums[2]);

                int[] ack = new int[3];
                ack[0] = l * w;
                ack[1] = w * h;
                ack[2] = h * l;
                int min = Arrays.stream(ack).min().getAsInt();

                squareFeetNeeded += ((2 * ack[0]) + (2 * ack[1]) + (2 * ack[2])) + min;
            }

        } catch (FileNotFoundException|IllegalArgumentException e) {
            e.printStackTrace();
        }

        System.out.println("Square feet needed: " + squareFeetNeeded);

    }
}