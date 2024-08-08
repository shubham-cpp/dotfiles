/// g++ -std=c++20 -O3 -march=native -flto -fopenmp -funroll-loops -o
/// sorting_filev3 sorting_filev3.cpp An alternative to `git ls-files
/// --exclude-standard --cached
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
#include <mutex>
#include <optional>
#include <ranges>
#include <sstream>
#include <string>
#include <sys/stat.h>
#include <thread>
#include <unistd.h>
#include <vector>

// FileInfo struct to hold metadata
struct FileInfo {
  time_t modTime;
  std::string name;
};

// Function to get file information
std::optional<FileInfo> getFileInfo(const std::string &fileName) {
  struct stat fileStat;
  if (stat(fileName.c_str(), &fileStat) != 0) {
    // std::cerr << "Error getting file info for " << fileName << std::endl;
    return std::nullopt;
  }

  // Check if the file is executable
  if (__builtin_expect((fileStat.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) != 0,
                       0)) {
    // It's an executable file, exclude it
    return std::nullopt;
  }

  return FileInfo{fileStat.st_mtime, fileName};
}

int main() {
  // Disable synchronization with C stdio for faster I/O
  std::ios::sync_with_stdio(false);

  std::vector<std::string> fileNames;
  std::string fileName;

  // Read file names from stdin
  while (std::getline(std::cin, fileName)) {
    fileNames.push_back(fileName);
  }

  std::vector<FileInfo> files;
  std::mutex files_mutex;

  // Function to process file info in parallel
  auto process_file = [&](const std::string &file) {
    if (auto fileInfo = getFileInfo(file); fileInfo) {
      std::lock_guard<std::mutex> lock(files_mutex);
      files.push_back(*fileInfo);
    }
  };

  // Launch threads to process files in parallel
  std::vector<std::thread> threads;
  for (const auto &file : fileNames) {
    threads.emplace_back(process_file, file);
  }

  // Join all threads
  for (auto &thread : threads) {
    thread.join();
  }

  // Sort files using parallel execution policy
  std::ranges::sort(files, std::ranges::greater{}, &FileInfo::modTime);

  // // Print sorted file names
  // for (const auto &file : files) {
  //   std::cout << file.name << std::endl;
  // }

  // Use a single output string to optimize cout
  std::stringstream output;
  for (const auto &file : files) {
    output << file.name << '\n';
  }
  // Output the entire result at once
  std::cout << output.str();

  return 0;
}
