#include <iostream>
#include <cstring>
#include <fstream>
#include <vector>
#include <algorithm>
#include <array>

using pair = std::pair<std::vector<std::string>, std::string>;

void print_three(pair &&v1, pair &&v2, pair &&v3) {
    size_t i = 0;
    size_t j = 0;
    size_t k = 0;
    while (i < v1.first.size() || j < v2.first.size() || k < v3.first.size()) {
        if (v1.first.size() > i && (v2.first.size() == j || v1.first[i] < v2.first[j]) &&
            (k == v3.first.size() || v1.first[i] < v3.first[k])) {
            std::cout << v1.first[i] << std::endl;
            i++;
        } else if (v2.first.size() > j && (k == v3.first.size() || v2.first[j] < v3.first[k])) {
            std::cout << v2.second << v2.first[j] << std::endl;
            j++;
        } else {
            std::cout << v3.second << v3.first[k] << std::endl;
            k++;
        }
    }
}

void comm_stream(std::istream &input1, std::istream &input2, std::array<bool, 3> &sup) {
    std::vector<std::string> v1, v2, diff1, diff2, inter;
    std::string line;
    std::array<std::string, 3> sep{"", "\t", "\t\t"};
    while (std::getline(input1, line)) {
        v1.push_back(line);
    }
    while (std::getline(input2, line)) {
        v2.push_back(line);
    }
    if (!sup[0]) {
        std::set_difference(v1.begin(), v1.end(), v2.begin(), v2.end(),
                            std::inserter(diff1, diff1.begin()));
    } else {
        sep[1] = "";
        sep[2] = "\t";
    }
    if (!sup[1]) {
        std::set_difference(v2.begin(), v2.end(), v1.begin(), v1.end(),
                            std::inserter(diff2, diff2.begin()));
    } else {
        sep[2] = sup[0] ? "" : "\t";
    }
    if (!sup[2]) {
        std::set_intersection(v1.begin(), v1.end(), v2.begin(), v2.end(),
                              std::inserter(inter, inter.begin()));
    }
    print_three(std::make_pair(diff1, sep[0]), std::make_pair(diff2, sep[1]), std::make_pair(inter, sep[2]));
}

int main(int argc, char **argv) {
    std::array<bool, 3> sup{false, false, false};
    std::array<std::string, 2> input_name;
    for (int i = 1; i < argc; i++) {
        std::string str = std::string(argv[i]);
        if (argv[i][0] == '-' && str.length() > 1) {
            for (size_t j = 1; j < str.length(); j++) {
                int ind = static_cast<int>(static_cast<unsigned char>(argv[i][j])) - 49;
                if (ind >= 0 && ind <= 2) {
                    sup[ind] = true;
                }
            }
        } else {
            if (input_name[0].empty()) {
                input_name[0] = str;
            }
            input_name[1] = str;
        }
    }
    std::ifstream f1, f2;
    if (input_name[0] != "-") {
        f1 = std::ifstream(input_name[0]);
    }
    if (input_name[1] != "-") {
        f2 = std::ifstream(input_name[1]);
    }
    comm_stream(f1.is_open() ? f1 : std::cin, f2.is_open() ? f2 : std::cin, sup);
    return 0;
}