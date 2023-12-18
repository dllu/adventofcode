import core.stdc.stdlib;
import std.conv;
import std.file;
import std.stdio;
import std.string;

void usage(string progname) {
    stderr.writef("usage: %s <file>\n", progname);
    exit(1);
}

int process(string contents) {
    int sum = 0;
    int[string] digits =
        ["0": 0, "1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7,
         "8": 8, "9": 9, "zero": 0, "one": 1, "two": 2, "three": 3,
	 "four": 4, "five": 5, "six": 6, "seven": 7, "eight": 8, "nine": 9];
    int[] nums = [];
    string[] lines = splitLines(strip(contents));
    foreach ( line ; lines ) {
        int minIndex = to!int(line.length) + 1;
        int maxIndex = -1;
        int leftDigit = 0;
        int rightDigit = 0;
        foreach ( digit ; digits.keys ) {
            int leftIndex = to!int(indexOf(line, digit));
            if ( leftIndex != -1 ) {
                if ( leftIndex < minIndex ) {
                    minIndex = leftIndex;
                    leftDigit = digits[digit];
                }
            }
            int rightIndex = to!int(lastIndexOf(line, digit));
            if ( rightIndex != -1 ) {
                if ( rightIndex > maxIndex ) {
                    maxIndex = rightIndex;
                    rightDigit = digits[digit];
                }
            }
        }
        sum += leftDigit * 10 + rightDigit;
    }
    return sum;
}

void main(string[] args) {
    if ( args.length < 2 ) {
        usage(args[0]);
    }
    string filename = args[1];
    string contents = readText(filename);
    int result = process(contents);
    writef("result = %d\n", result);
}
