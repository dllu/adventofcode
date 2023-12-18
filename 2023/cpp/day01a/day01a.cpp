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
    std::vector<std::string> digits = { "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" };
    std::vector<int> nums;
    while ( std::getline(infile, line) )
    {
        std::unordered_map<std::string, int> left_indices;
        std::unordered_map<std::string, int> right_indices;
        for ( auto digitp = digits.begin(); digitp != digits.end(); digitp++ )
        {
            if ( line.find(*digitp) != std::string::npos )
            {
                left_indices.insert({*digitp, (int)line.find(*digitp)});
                right_indices.insert({*digitp, (int)line.rfind(*digitp)});
            }
        }
        int min_index = (int)line.size() + 1;
        std::string left_digit;
        for ( auto leftp = left_indices.begin(); leftp != left_indices.end(); leftp++ )
        {
            if ( leftp->second < min_index )
            {
                min_index = leftp->second;
                left_digit = leftp->first;
            }
        }
        int max_index = -1;
        std::string right_digit;
        for ( auto rightp = right_indices.begin(); rightp != right_indices.end(); rightp++ )
        {
            if ( rightp->second > max_index )
            {
                max_index = rightp->second;
                right_digit = rightp->first;
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
