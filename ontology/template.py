#!/usr/bin/env python3

# Convert a TSV table into a HOWL file
# consisting of lines
# that append the header to the cell value.

import argparse, csv


def main():
  # Parse arguments
  parser = argparse.ArgumentParser(
      description='Convert table to HOWL stanzas')
  parser.add_argument('table',
      type=argparse.FileType('r'),
      help='a TSV file')
  args = parser.parse_args()

  rows = csv.reader(args.table, delimiter='\t')
  headers = next(rows)
  for row in rows:
    if len(row) < 1:
      continue
    for i in range(0, min(len(headers), len(row))):
      if headers[i] == 'SUBJECT':
        print(row[i])
      else:
        print(headers[i] +' '+ row[i])
    print()


# Run the main() function.
if __name__ == "__main__":
  main()
