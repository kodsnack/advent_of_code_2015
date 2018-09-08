package se.droogies.bromaniac.adventofcode15.day3;

import java.io.File;
import java.io.FileNotFoundException;
import java.net.URL;
import java.util.HashMap;
import java.util.Scanner;

public class App {
    public static void main (String[] args) {
        char[] charArray = new char[0];
        Point p = new Point();
        HashMap<String, Integer> result = new HashMap<>();

        result.put("0,0", 1);

        try {
            URL path = App.class.getResource("/input.txt");
            Scanner input = new Scanner(new File(path.getFile()));

            while (input.hasNextLine()) {
               charArray = input.nextLine().toCharArray();
            }

            for (char c : charArray) {
                switch (c) {
                    case '<':
                        p.x--;
                        break;
                    case '>':
                        p.x++;
                        break;
                    case '^':
                        p.y++;
                        break;
                    case 'v':
                        p.y--;
                        break;
                    default:
                        throw new IllegalArgumentException("Unknown char " + c);
                }

                result.put(p.toString(), 1);
            }
        } catch (FileNotFoundException|IllegalArgumentException e) {
            System.out.println(e.toString());
            e.printStackTrace();
        }

        System.out.println(result.size());
    }
}
