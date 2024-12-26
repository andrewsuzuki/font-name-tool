# font-name-tool

CLI tool for reading and writing the `name` table in TTF and OTF fonts (family, subfamily, copyright, etc).

## Usage

Commands: `print`, `set`, `replace`, `remove`

Quick examples

```sh
# Print all records (table output)
font-name-tool print myfont.ttf
# Print all records (json output)
font-name-tool print --print-json myfont.ttf
# Add/update record
# (record argument order: platform, encoding, language, name, string)
font-name-tool set --record 3 1 0x0409 1 "My Font" --in-place myfont.ttf
# Add/update record (json input string)
font-name-tool set --json-input-string "[[3,1,1033,1,\"My Font\"]]" --in-place myfont.ttf
# Remove record(s) matching all filters (boolean AND)
font-name-tool remove \
    --platform-id 3 \
    --encoding-id 1 \
    --language-id 0x0409 \
    --name-id 1 \
    --in-place \
    myfont.ttf
```

## Installation

Requires Python 3.8+

```sh
# With pip
pip install font-name-tool
# With pipx
pipx install font-name-tool
# With pipx (run without installing)
pipx run font-name-tool ...
```

## `name` Table Reference

- Apple (TTF) https://developer.apple.com/fonts/TrueType-Reference-Manual/RM06/Chap6name.html
- Microsoft (OTF) https://learn.microsoft.com/en-us/typography/opentype/spec/name

## Commands

### print

Print all records. Prints as table by default, or json with `--print-json`.

### set

Add or update records. Prints results after saving.

```
Options
--print-json          Print JSON instead of table
--quiet               Don't print anything
--dry-run             Show changes without applying them
--output OUTPUT       Output font file
--in-place            Modify the input file in-place
--no-validation       Don't validate ids or combinations (platform, encoding, language, name)
--record PLATFORM ENCODING LANGUAGE NAME STRING
                      Record as: platform encoding language name string (supports multiple)
--json-input-string JSON_INPUT_STRING
                      JSON string containing records (e.g. [[1, 0, 0, 1, 'My Font'], ...])
--json-input-file JSON_INPUT_FILE
                      JSON file containing records (e.g. [[1, 0, 0, 1, 'My Font'], ...])
```

The `platform`, `encoding`, `language`, and `name` for `--record` can be given as integers (base 10 or base 16 with 0x prefix), or as string codes, which are translated to integers.

Examples (all equivalent):

- `--record 3 1 0x0409 1 "Comic Sans"`.
- `--record 3 1 1033 1 "Comic Sans"`.
- `--record windows unicode_bmp en font_family_name "Comic Sans"`.
- `--record win unicode_bmp en font_family "Comic Sans"`.
- `--record win 1 1033 family "Comic Sans"`.

See `codes.txt` for a list of supported ids and codes. Note, there are a couple of edge cases with the IETF BCP 47 language codes. Notably, `es` isn't supported because there are multiple Spanish language IDs that refer to `es`. Similarly, `ga` (Irish Gaelic) isn't supported on the macintosh platform and `sms` (Skolt Sámi) isn't supported on the windows platform.

Multiple `--record` can be supplied.

Input json from a string or file should be an array of records, which are represented as an array of the four whole number IDs + string:

```json
[
  [3, 1, 1033, 1, "Comic Sans"],
  [3, 1, 1033, 2, "Regular"]
]
```

Use `--dry-run` to print the changes that would be made.

### replace

Same as `set`, but replaces the entire table with the given records.

### remove

Remove records that match all given filters.

```
Options
--print-json          Print JSON instead of table
--quiet               Don't print anything
--dry-run             Show changes without applying them
--output OUTPUT       Output font file
--in-place            Modify the input file in-place
--no-validation       Don't validate name IDs that may be referenced in other tables
--platform-id PLATFORM_ID
                      Platform ID filter
--encoding-id ENCODING_ID
                      Encoding ID filter
--language-id LANGUAGE_ID
                      Language ID filter
--name-id NAME_ID     Name ID filter
```

At least one of the four filters should be supplied. Does not support string codes.

## Full Example

```sh
# Replace the entire name table with new records
font-name-tool replace \
--record win 1 1033 0 "Copyright 2020 The JetBrains Mono Project Authors (https://github.com/JetBrains/JetBrainsMono)" \
--record win 1 1033 1 "JetBrains Mono" \
--record win 1 1033 2 "Bold" \
--record win 1 1033 3 "2.304;JB;JetBrainsMono-Bold" \
--record win 1 1033 4 "JetBrains Mono Bold" \
--record win 1 1033 5 "Version 2.304; ttfautohint (v1.8.4.7-5d5b)" \
--record win 1 1033 6 "JetBrainsMono-Bold" \
--record win 1 1033 7 "JetBrains Mono is a trademark of JetBrains s.r.o." \
--record win 1 1033 8 "JetBrains" \
--record win 1 1033 9 "Philipp Nurullin, Konstantin Bulenkov" \
--record win 1 1033 11 "https://www.jetbrains.com" \
--record win 1 1033 12 "https://www.jetbrains.com" \
--record win 1 1033 13 "This Font Software is licensed under the SIL Open Font License, Version 1.1. This license is available with a FAQ at: https://scripts.sil.org/OFL" \
--record win 1 1033 14 "https://scripts.sil.org/OFL" \
--output JetBrainsMono-Bold_modified.ttf \
JetBrainsMono-Bold.ttf
```

## TODO

- Allow reading/writing from ltag table?
- Remove guards on names attribute if my PR is merged
  - https://github.com/fonttools/fonttools/pull/3732
- Remove special handling for duplicate language codes if resolved (es, ga / es, sms)
  - https://github.com/fonttools/fonttools/issues/3733

## Author

Andrew Suzuki [andrewsuzuki.com](https://andrewsuzuki.com)
