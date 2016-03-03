package com.lennell;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Stream;

public class Day2 {



    protected static int countRow(String[] s){
        int l = Integer.valueOf(s[0]);
        int w = Integer.valueOf(s[1]);
        int h = Integer.valueOf(s[2]);
        int sa = (2*l*w) + (2*w*h) + (2*h*l);
        List<Integer> iList = Arrays.asList(l*w, w*h, h*l);
        int min = iList.stream().min(Integer::compare).get();
        return sa+min;
    }

    public static void main(String[] args) throws Exception{
        if (args.length!=1){
            System.out.println("Program takes one String parameter. Filename to input file!");
            return;
        }

        String fileName = args[0];
        //read file into stream
        Stream<String> stream = Files.lines(Paths.get(fileName));

        int result = 0;

        for (String s:stream.toArray(String[]::new)){
            result+=countRow(s.split("x"));
        }

        System.out.println("Total square feet: " +result);
    }
}
