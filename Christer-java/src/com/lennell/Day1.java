package com.lennell;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Day1 {

    public static void main(String[] args) {
        if (args.length!=1){
            System.out.println("Program takes one String parameter!");
            return;
        }

        List<Character> charList = new ArrayList<>();
        for (char c:args[0].toCharArray()){
            charList.add(c);
        }
        long left = charList.stream().filter(p->p.equals('(')).count();
        long right = charList.stream().filter(p->p.equals(')')).count();
        System.out.println("Ending up att floor: " + (left-right));
    }
}
