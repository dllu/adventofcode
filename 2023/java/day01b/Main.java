import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.HashMap;

public class Main {
    static void usage() {
        System.out.println("usage: java Main <file>");
        System.exit(1);
    }

    static int process(String filename) {
        int sum = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            HashMap<String, String> digitMap = new HashMap<String, String>() {{
                put("0", "0");
                put("1", "1");
                put("2", "2");
                put("3", "3");
                put("4", "4");
                put("5", "5");
                put("6", "6");
                put("7", "7");
                put("8", "8");
                put("9", "9");
                put("zero", "0");
                put("one", "1");
                put("two", "2");
                put("three", "3");
                put("four", "4");
                put("five", "5");
                put("six", "6");
                put("seven", "7");
                put("eight", "8");
                put("nine", "9");
            }};
            String line;
            while ( (line = reader.readLine()) != null ) {
                int minIndex = line.length() + 1;
                int maxIndex = -1;
                String leftDigit = "";
                String rightDigit = "";
                for ( String digit : digitMap.keySet() ) {
                    int leftIndex = line.indexOf(digit);
                    if ( leftIndex != -1 && leftIndex < minIndex ) {
                        minIndex = leftIndex;
                        leftDigit = digitMap.get(digit);
                    }
                    int rightIndex = line.lastIndexOf(digit);
                    if ( rightIndex != -1 && rightIndex > maxIndex ) {
                        maxIndex = rightIndex;
                        rightDigit = digitMap.get(digit);
                    }
                }
                sum += Integer.parseInt(leftDigit + rightDigit);
            }
        } catch ( IOException e ) {
            e.printStackTrace();
        }
        return sum;
    }

    public static final void main(String[] args) {
        if ( args.length < 1 ) {
            usage();
        }
        String filename = args[0];
        int result = process(filename);
        System.out.println("result = " + Integer.toString(result));
    }
}
