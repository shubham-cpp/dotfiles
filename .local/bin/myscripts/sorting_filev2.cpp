/// g++ -std=c++20 -O3 -march=native -flto -fopenmp -funroll-loops -o sort_files
/// sort_files.cpp An alternative to `git ls-files --exclude-standard --cached
/// --others
/// --deduplicate | xargs -I{} stat -f "%m %N" {} | sort -rn | cut -d' ' -f2-`
/// (except for git ls-files part)
/// The reason was
/**
 * hyperfine --warmup 3 "git ls-files --exclude-standard --cached --others
--deduplicate | xargs -I{} stat -f '%m %N' {} | sort -rn | cut -d' ' -f2-" 'git
ls-files --exclude-standard
 --cached --others --deduplicate | ./sorting_filev2'

 * Benchmark 1: git ls-files --exclude-standard --cached --others --deduplicate
| xargs -I{} stat -f '%m %N' {} | sort -rn | cut -d' ' -f2- Time (mean ± σ):
277.3 ms ±  11.3 ms    [User: 22.3 ms, System: 134.9 ms] Range (min … max):
260.4 ms … 290.1 ms    10 runs

* Benchmark 2: git ls-files --exclude-standard --cached --others --deduplicate |
 ./sorting_filev2 Time (mean ± σ):      10.0 ms ±   1.3 ms    [User: 2.9 ms,
System: 5.2 ms] Range (min … max):     7.7 ms …  15.1 ms    173 runs

* Summary
  git ls-files --exclude-standard --cached --others --deduplicate |
./sorting_filev2 ran 27.77 ± 3.78 times faster than git ls-files
--exclude-standard --cached --others --deduplicate | xargs -I{} stat -f '%m %N'
{} | sort -rn | cut -d' ' -f2-
 */
#include <algorithm>
#include <execution>
#include <fcntl.h>
#include <future>
#include <iostream>
#include <optional>
#include <ranges>
#include <sstream>
#include <string>
#include <sys/stat.h>
#include <unistd.h>
#include <vector>

struct FileInfo {
  time_t modTime;
  std::string name;
};

std::optional<FileInfo> getFileInfo(const std::string &fileName) {
  struct stat fileStat;
  if (stat(fileName.c_str(), &fileStat) != 0) {
    // std::cerr << "Error getting file info for " << fileName << std::endl;
    return std::nullopt;
  }
  return FileInfo{fileStat.st_mtime, fileName};
}

int main() {
  std::vector<std::string> fileNames;
  std::string fileName;

  // Read file names from stdin
  while (std::getline(std::cin, fileName)) {
    fileNames.push_back(fileName);
  }

  std::vector<std::future<std::optional<FileInfo>>> futures;
  for (const auto &file : fileNames) {
    futures.emplace_back(std::async(std::launch::async, getFileInfo, file));
  }

  std::vector<FileInfo> files;
  for (auto &future : futures) {
    auto fileInfo = future.get();
    if (fileInfo) {
      files.push_back(*fileInfo);
    }
  }

  std::ranges::sort(files, std::ranges::greater{}, &FileInfo::modTime);

  for (const auto &file : files) {
    std::cout << file.name << std::endl;
  }

  return 0;
}
