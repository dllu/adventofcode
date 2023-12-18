#include <cstdlib>
#include <fstream>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

void usage(char* progname)
{
    std::cout << progname << " <file>" << std::endl;
    std::exit(1);
}

int process(std::ifstream& infile)
{
    std::string line;
    std::unordered_map<std::string, std::string> digits = {
        {"0", "0"}, {"1", "1"}, {"2", "2"}, {"3", "3"}, {"4", "4"},
	{"5", "5"}, {"6", "6"}, {"7", "7"}, {"8", "8"}, {"9", "9"},
	{"zero", "0"}, {"one", "1"}, {"two", "2"}, {"three", "3"},
	{"four", "4"}, {"five", "5"}, {"six", "6"}, {"seven", "7"},
	{"eight", "8"}, {"nine", "9"},
    };
    std::vector<int> nums;
    while ( std::getline(infile, line) )
    {
        std::unordered_map<std::string, int> left_indices;
        std::unordered_map<std::string, int> right_indices;
        for ( auto digitp = digits.begin(); digitp != digits.end(); digitp++ )
        {
            if ( line.find(digitp->first) != std::string::npos )
            {
                left_indices.insert({digitp->first, (int)line.find(digitp->first)});
                right_indices.insert({digitp->first, (int)line.rfind(digitp->first)});
            }
        }
        int min_index = (int)line.size() + 1;
        std::string left_digit;
        for ( auto leftp = left_indices.begin(); leftp != left_indices.end(); leftp++ )
        {
            if ( leftp->second < min_index )
            {
                min_index = leftp->second;
                left_digit = digits[leftp->first];
            }
        }
        int max_index = -1;
        std::string right_digit;
        for ( auto rightp = right_indices.begin(); rightp != right_indices.end(); rightp++ )
        {
            if ( rightp->second > max_index )
            {
                max_index = rightp->second;
                right_digit = digits[rightp->first];
            }
        }
        std::string num_str = left_digit + right_digit;
        nums.push_back(std::atoi(num_str.c_str()));
    }
    int sum = 0;
    for ( auto nump = nums.begin(); nump != nums.end(); nump++ )
    {
        sum += *nump;
    }
    return sum;
}

int main(int argc, char* argv[])
{
    if ( argc < 2 ) {
        usage(argv[0]);
    }
    std::ifstream infile(argv[1]);
    int result = process(infile);
    std::cout << "result = " << result << std::endl;
    return 0;
}
