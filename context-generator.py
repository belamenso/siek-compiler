#!/usr/bin/env python3

import sys
import os

def collect_files(path, collected_files=None):
    """Recursively collect files from path into a set of (filepath, relative_path, filename) tuples."""
    if collected_files is None:
        collected_files = set()

    try:
        if os.path.isfile(path):
            collected_files.add((
                os.path.abspath(path),  # full path for opening files
                path,  # relative path as given in arguments
                os.path.basename(path)
            ))
        elif os.path.isdir(path):
            for entry in os.scandir(path):
                # Construct relative path based on the original path argument
                rel_path = os.path.relpath(entry.path, os.path.dirname(path))
                rel_path = os.path.join(os.path.basename(path), rel_path)
                if entry.is_file():
                    collected_files.add((
                        os.path.abspath(entry.path),
                        rel_path,
                        os.path.basename(entry.path)
                    ))
                elif entry.is_dir():
                    collect_files(entry.path, collected_files)
        else:
            print(f"Warning: '{path}' is neither a file nor a directory. Skipping.", 
                  file=sys.stderr)
    except Exception as e:
        print(f"Error accessing '{path}': {str(e)}", file=sys.stderr)

    return collected_files

def main():
    # Check if any paths were provided as arguments
    if len(sys.argv) < 2:
        print("Usage: script.py path1 path2 ... (paths can be files or directories)")
        sys.exit(1)

    # First pass: collect all unique files
    files_to_process = set()
    for path in sys.argv[1:]:
        if os.path.exists(path):
            files_to_process.update(collect_files(path))
        else:
            print(f"Warning: Path '{path}' does not exist. Skipping.", file=sys.stderr)

    # Convert to sorted list for consistent output
    files_to_process = sorted(files_to_process)

    # Start context
    print('<context>')

    # Print file listing
    print('<files>')
    for _, rel_path, _ in files_to_process:
        print(f'  {rel_path}')
    print('</files>\n')

    # Process each file
    for abs_path, _, filename in files_to_process:
        try:
            # Print the opening tag
            print(f'<file name="{filename}">')

            # Read and print the file contents
            with open(abs_path, 'r', encoding='utf-8') as f:
                print(f.read().rstrip())

            # Print the closing tag
            print('</file>\n')

        except Exception as e:
            print(f"Error processing file '{abs_path}': {str(e)}", file=sys.stderr)

    # End context
    print('</context>')

if __name__ == "__main__":
    main()

