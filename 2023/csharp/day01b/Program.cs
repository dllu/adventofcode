using System.IO;

class Program
{
    static void Usage()
    {
        string? progPath = Environment.ProcessPath;
	string? progName = Path.GetFileName(progPath);
        Console.WriteLine("usage: " + progName + " <file>");
        System.Environment.Exit(1);
    }

    static string WordToNumber(string word) => word switch
    {
        "zero"  => "0",
        "one"   => "1",
        "two"   => "2",
        "three" => "3",
        "four"  => "4",
        "five"  => "5",
        "six"   => "6",
        "seven" => "7",
        "eight" => "8",
        "nine"  => "9",
        _       => throw new ArgumentOutOfRangeException("unexpected value: " + word),
    };

    static int Process(string contents)
    {
        int sum = 0;
        string[] digits =
            { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
              "zero", "one", "two", "three", "four", "five", "six", "seven",
              "eight", "nine" };
        string[] lines = contents.Split(
            new string[] { Environment.NewLine },
            StringSplitOptions.None
        );
        foreach ( string line in lines )
        {
            int minLeft = Int32.MaxValue;
            int maxRight = Int32.MinValue;
            string minDigit = "";
            string maxDigit = "";
            foreach ( string digit in digits )
            {
                if ( line.IndexOf(digit) != -1 )
                {
                    int leftIndex = line.IndexOf(digit);
		    int rightIndex = 0;
                    int tempIndex = 0;
                    while ( true )
                    {
                        tempIndex = line.IndexOf(digit, tempIndex + 1);
                        if ( tempIndex == -1 )
                        {
                            break;
                        }
                        rightIndex = tempIndex;
                    }
                    if ( leftIndex < minLeft )
                    {
                        minLeft = leftIndex;
                        if ( digit.Length > 1 )
                        {
                            minDigit = WordToNumber(digit);
                        } else {
                            minDigit = digit;
                        }
                    }
                    if ( rightIndex > maxRight )
                    {
                        maxRight = rightIndex;
                        if ( digit.Length > 1 )
                        {
                            maxDigit = WordToNumber(digit);
                        } else {
                            maxDigit = digit;
                        }
                    }
                }
            }
            int num = Int32.Parse(minDigit + maxDigit);
            sum += num;
        }
        return sum;
    }

    public static void Main(string[] args)
    {
        if ( args.Length < 1 )
        {
            Program.Usage();
        }
	string fileName = args[0];
	string contents = File.ReadAllText(fileName);
	contents = contents.Remove(contents.Length - 1, 1);
        int result = Process(contents);
	Console.WriteLine("result = " + result);
    }
}
