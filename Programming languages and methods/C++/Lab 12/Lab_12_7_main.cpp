/*
Найти все файлы с расширением «txt» в указанном каталоге, для каждого файла разбить его текст 
на слова, случайным образом перемешать буквы в середине каждого слова (оставить на своих 
местах первую и последнюю буквы) и сохранить полученный текст в файле с тем же именем, но в 
текущем каталоге.
Работоспособность программы нужно проверить на наборе текстовых файлов, содержащих текст на 
английском языке.
*/

#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <filesystem>
#include <algorithm>
#include <random>

namespace fs = std::filesystem;

std::vector<std::string> getWordsFromFile(const std::string& fileName) {
    std::vector<std::string> words;
    std::ifstream file(fileName);
    
    std::string word;
    while (file >> word) {
        words.push_back(word);
    }
    return words;
}

std::string shuffleMiddleLetters(const std::string& word) {
    if (word.length() <= 3) {
        return word;
    }
    std::string middle = word.substr(1, word.length() - 2);
    std::shuffle(middle.begin(), middle.end(), std::mt19937{std::random_device{}()});
    return word[0] + middle + word[word.length() - 1];
}

bool writeWordsToFile(const std::string& fileName, const std::vector<std::string>& words) {
    std::ofstream file(fileName);
    
    for (const auto& word : words) {
        file << word << " ";
    }
    return true;
}

int main(int argc, char* argv[]) {

    std::string directory = argv[1];
    for (const auto& entry : fs::directory_iterator(directory)) {
        if (entry.is_regular_file() && entry.path().extension() == ".txt") {
            std::string fileName = entry.path().string();
            std::vector<std::string> words = getWordsFromFile(fileName);
            for (std::string& word : words) {
                word = shuffleMiddleLetters(word);
            }
            std::string outputFile = fs::current_path().string() + "/" + entry.path().filename().string();
            if (!writeWordsToFile(outputFile, words)) {
                std::cerr << "Failed to write to file: " << outputFile << std::endl;
            } else {
                std::cout << "Processed file: " << fileName << ", saved to: " << outputFile << std::endl;
            }
        }
    }

    return 0;
}
