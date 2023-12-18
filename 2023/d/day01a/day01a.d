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
    string[] digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"];
    int[] nums = [];
    string[] lines = splitLines(strip(contents));
    foreach ( line ; lines ) {
        int minIndex = to!int(line.length) + 1;
        int maxIndex = -1;
        string leftDigit = "";
        string rightDigit = "";
        foreach ( digit ; digits ) {
            int leftIndex = to!int(indexOf(line, digit));
            if ( leftIndex != -1 ) {
                if ( leftIndex < minIndex ) {
                    minIndex = leftIndex;
                    leftDigit = digit;
                }
            }
            int rightIndex = to!int(lastIndexOf(line, digit));
            if ( rightIndex != -1 ) {
                if ( rightIndex > maxIndex ) {
                    maxIndex = rightIndex;
                    rightDigit = digit;
                }
            }
        }
	string s = leftDigit ~ rightDigit;
        sum += parse!int(s);
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
