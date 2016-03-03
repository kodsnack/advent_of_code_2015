package com.lennell;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

public class Day1 {

    public static void main(String[] args) throws IOException{
        if (args.length!=1){
            System.out.println("Program takes one String parameter. Filename to input file!");
            return;
        }

        String fileName = args[0];

        List<String> lines = Files.readAllLines(Paths.get(fileName));

        List<Character> charList = new ArrayList<>();
        for (char c:lines.get(0).toCharArray()){
            charList.add(c);
        }
        long left = charList.stream().filter(p->p.equals('(')).count();
        long right = charList.stream().filter(p->p.equals(')')).count();
        System.out.println("Ending up att floor: " + (left-right));
    }
}
