#!/bin/python3

from argparse import ArgumentParser
from os.path import relpath
from sys import argv, stderr
from sys import exit as sys_exit
from typing import TypedDict


class ReturnParseArgs(TypedDict):
    from_file: str
    to_file: str


def parse_arguments() -> ReturnParseArgs:
    """
    Parse command-line arguments with multiple flexible input methods.

    Supports three argument parsing styles:
    1. Positional arguments: python main.py file1 file2
    2. Long-form arguments: python main.py --from file1 --to file2
    3. Short-form arguments: python main.py -f file1 -t file2
    """
    # Create the main parser
    parser = ArgumentParser(
        prog="Calculate Relative Path",
        description="Calculate relative path between two files or process two file arguments.",
        usage="%(prog)s [OPTIONS] [FILE1 FILE2]",
    )

    # Method 1: Positional arguments
    if len(argv) == 3 and not argv[1].startswith("-"):
        return {"from_file": argv[1], "to_file": argv[2]}

    # Method 2 & 3: Use argparse for named arguments
    parser.add_argument(
        "-f", "--from", dest="from_file", required=True, help="Source file path"
    )
    parser.add_argument(
        "-t", "--to", dest="to_file", required=True, help="Destination file path"
    )

    # Parse the arguments
    args = parser.parse_args()

    return {"from_file": args.from_file, "to_file": args.to_file}


# Example usage
if __name__ == "__main__":
    try:
        paths = parse_arguments()
        print(relpath(paths["from_file"], paths["to_file"]))
    except Exception as e:
        print(f"Error: {e}", file=stderr)
        sys_exit(1)
