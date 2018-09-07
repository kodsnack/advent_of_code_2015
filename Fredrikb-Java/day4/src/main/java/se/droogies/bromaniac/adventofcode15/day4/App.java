package se.droogies.bromaniac.adventofcode15.day4;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

// http://adventofcode.com/2015/day/4

public class App {
    public static void main(String[] args) {
        String password = "";

        try {
            password = args[0];
        } catch(ArrayIndexOutOfBoundsException ex) {
            System.err.println("Usage: java App <password>");
            System.exit(1);
        }

        int counter = 0;

        while (true) {
            counter++;
            String answer = toHash(password + counter);

            if (answer.startsWith("00000")) {
                System.out.println(answer);
                System.out.println(counter);
                break;
            }
        }
    }

    public static String toHash(String str) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            md.update(str.getBytes());
            byte[] digest = md.digest();

            StringBuffer sb = new StringBuffer();
            for (byte b : digest) {
                sb.append(String.format("%02x", b & 0xff));
            }
            return sb.toString();

        } catch(NoSuchAlgorithmException ex) {
            System.err.println(ex.toString());
        }
        return "";
    }
}
