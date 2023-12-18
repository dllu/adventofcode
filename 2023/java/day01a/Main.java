import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class Main {
    static void usage() {
        System.out.println("usage: java Main <file>");
        System.exit(1);
    }

    static int process(String filename) {
        int sum = 0;
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String[] digits = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
            String line;
            while ( (line = reader.readLine()) != null ) {
                int minIndex = line.length() + 1;
                int maxIndex = -1;
                String leftDigit = "";
                String rightDigit = "";
                for ( String digit : digits ) {
                    int leftIndex = line.indexOf(digit);
                    if ( leftIndex != -1 && leftIndex < minIndex ) {
                        minIndex = leftIndex;
                        leftDigit = digit;
                    }
                    int rightIndex = line.lastIndexOf(digit);
                    if ( rightIndex != -1 && rightIndex > maxIndex ) {
                        maxIndex = rightIndex;
                        rightDigit = digit;
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
