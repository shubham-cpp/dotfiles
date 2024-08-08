/// rustc -C opt-level=3 -C target-cpu=native -o sort_files sort_files.rs
use std::fs::{self, Metadata};
use std::io::{self, BufRead};
use std::os::unix::fs::PermissionsExt;
use std::path::Path;
use std::sync::mpsc;
use std::thread;
// use std::time::{Duration, SystemTime};
use std::time::SystemTime;

// #[derive(Debug)]
struct FileInfo {
    mod_time: SystemTime,
    name: String,
}

fn get_file_info(file_name: String) -> Option<FileInfo> {
    let path = Path::new(&file_name);
    match path.metadata() {
        Ok(metadata) => Some(FileInfo {
            mod_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
            name: file_name,
        }),
        Err(_) => {
            // eprintln!("Error getting file info for {}", file_name);
            None
        }
    }
}

fn get_file_info2(file_name: String) -> Option<FileInfo> {
    let path = Path::new(&file_name);
    match path.metadata() {
        Ok(metadata) => {
            let permissions = metadata.permissions();
            let is_executable = permissions.mode() & 0o111 != 0;

            if !is_executable {
                Some(FileInfo {
                    mod_time: metadata.modified().unwrap_or(SystemTime::UNIX_EPOCH),
                    name: file_name,
                })
            } else {
                None
            }
        }
        Err(_) => {
            // eprintln!("Error getting file info for {}", file_name);
            None
        }
    }
}
fn main() {
    let stdin = io::stdin();
    let file_names: Vec<String> = stdin.lock().lines().filter_map(Result::ok).collect();

    let (tx, rx) = mpsc::channel();
    let mut handles = vec![];

    for file_name in file_names {
        let tx = tx.clone();
        let handle = thread::spawn(move || {
            if let Some(file_info) = get_file_info2(file_name) {
                tx.send(file_info).unwrap();
            }
        });
        handles.push(handle);
    }

    drop(tx); // Close the sending side of the channel

    let mut files: Vec<FileInfo> = rx.iter().collect();
    for handle in handles {
        handle.join().unwrap();
    }

    files.sort_by(|a, b| b.mod_time.cmp(&a.mod_time));

    for file in files {
        println!("{}", file.name);
    }
}

fn main2() {
    let stdin = io::stdin();
    let file_names: Vec<String> = stdin.lock().lines().filter_map(Result::ok).collect();

    let (tx, rx) = mpsc::channel();
    let mut handles = vec![];

    for file_name in file_names {
        let tx = tx.clone();
        let handle = thread::spawn(move || {
            if let Some(file_info) = get_file_info(file_name) {
                tx.send(file_info).unwrap();
            }
        });
        handles.push(handle);
    }

    drop(tx); // Close the sending side of the channel

    let mut files: Vec<FileInfo> = rx.iter().collect();
    for handle in handles {
        handle.join().unwrap();
    }

    files.sort_by(|a, b| b.mod_time.cmp(&a.mod_time));

    for file in files {
        println!("{}", file.name);
    }
}
