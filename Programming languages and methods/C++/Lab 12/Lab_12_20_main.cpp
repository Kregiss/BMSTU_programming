/*
Найти все файлы с расширением «html» в указанном каталоге, для каждого файла 
определить множество гиперссылок и сохранить объединение полученных множеств в 
файле links.txt в текущем каталоге. Каждая гиперссылка в сформированном файле 
должна располагаться в отдельной строке. Гиперссылки должны быть отсортированы 
лексикографически.

Гиперссылка в HTML-файле задаётся тегом «a»:
<a href=гиперссылка>

Работоспособность программы нужно проверить на наборе HTML-файлов, загруженных 
из интернета.
*/
#include <iostream>
#include <fstream>
#include <vector>
#include <set>
#include <string>
#include <filesystem>
#include <regex>

namespace fs = std::filesystem;

std::vector<std::string> getHtmlFiles(const std::string& directory) {
    std::vector<std::string> htmlFiles;
    for (const auto& entry : fs::directory_iterator(directory)) {
        if (entry.is_regular_file() && entry.path().extension() == ".html") {
	    std::ifstream file(entry.path().string());
	    std::string line;
            while (std::getline(file, line)) {
                std::smatch match;
                std::regex linkRegex("<a\\s+href=\"(https://[^\"]+)\"");
                if (std::regex_search(line, match, linkRegex)) {
                    htmlFiles.push_back(entry.path().string());
                    break;
                }
            }
        }
    }
    return htmlFiles;
}

std::set<std::string> getHyperlinksFromFile(const std::string& fileName) {
    std::set<std::string> hyperlinks;
    std::ifstream file(fileName);
    std::string line;
    std::regex linkRegex("<a\\s+href=\"([^\"]+)\"");
    while (std::getline(file, line)) {
        std::smatch match;
        while (std::regex_search(line, match, linkRegex)) {
            hyperlinks.insert(match[1].str());
            line = match.suffix().str();
        }
    }
    return hyperlinks;
}

bool writeHyperlinksToFile(const std::string& fileName, const std::set<std::string>& hyperlinks) {
    std::ofstream file(fileName);

    for (const auto& hyperlink : hyperlinks) {
        file << hyperlink << std::endl;
    }
    return true;
}

int main(int argc, char* argv[]) {

    std::string directory = argv[1];
    std::vector<std::string> htmlFiles = getHtmlFiles(directory);


    std::set<std::string> allHyperlinks;

    for (const auto& fileName : htmlFiles) {
        std::set<std::string> hyperlinks = getHyperlinksFromFile(fileName);
        allHyperlinks.insert(hyperlinks.begin(), hyperlinks.end());
    }

   if (!writeHyperlinksToFile("links.txt", allHyperlinks)) {
        std::cerr << "Failed to write hyperlinks to links.txt." << std::endl;
        return 1;
    }

    std::cout << "All hyperlinks from HTML files have been saved to links.txt." << std::endl;
    return 0;
}
