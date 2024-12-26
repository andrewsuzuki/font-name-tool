import argparse
import json
import os
import re
import sys
from importlib import metadata
from typing import Any, Dict, List, Optional, Tuple, Union

from fontTools.ttLib import TTFont  # type: ignore[import-untyped]
from fontTools.ttLib.tables._n_a_m_e import (  # type: ignore[import-untyped]
    _MAC_LANGUAGES,
    _WINDOWS_LANGUAGES,
    NameRecord,
    NameRecordVisitor,
    table__n_a_m_e,
)

PLATFORM_ID_UNICODE: int = 0
PLATFORM_ID_MACINTOSH: int = 1
PLATFORM_ID_WINDOWS: int = 3

# NOTE codes must be unique within platform
PLATFORM_ID_TO_CODES: Dict[int, Tuple[str, ...]] = {
    PLATFORM_ID_UNICODE: ("unicode",),
    PLATFORM_ID_MACINTOSH: ("macintosh", "mac"),
    PLATFORM_ID_WINDOWS: ("windows", "win"),
}
PLATFORM_CODE_TO_ID: Dict[str, int] = {
    code: platform_id
    for platform_id, codes in PLATFORM_ID_TO_CODES.items()
    for code in codes
}

# NOTE codes must be unique within platform
ENCODING_ID_TO_CODE_FOR_PLATFORM: Dict[int, Dict[int, str]] = {
    PLATFORM_ID_UNICODE: {
        0: "unicode_1_0",  # deprecated
        1: "unicode_1_1",  # deprecated
        2: "iso_10646",  # deprecated
        3: "unicode_bmp",
        4: "unicode_full",
    },
    PLATFORM_ID_MACINTOSH: {
        0: "roman",
        1: "japanese",
        2: "chinese_traditional",
        3: "korean",
        4: "arabic",
        5: "hebrew",
        6: "greek",
        7: "russian",
        8: "rsymbol",
        9: "devanagari",
        10: "gurmukhi",
        11: "gujarati",
        12: "oriya",
        13: "bengali",
        14: "tamil",
        15: "telugu",
        16: "kannada",
        17: "malayalam",
        18: "sinhalese",
        19: "burmese",
        20: "khmer",
        21: "thai",
        22: "laotian",
        23: "georgian",
        24: "armenian",
        25: "chinese_simplified",
        26: "tibetan",
        27: "mongolian",
        28: "geez",
        29: "slavic",
        30: "vietnamese",
        31: "sindhi",
        32: "uninterpreted",  # special name (no special handling)
    },
    PLATFORM_ID_WINDOWS: {
        0: "symbol",
        1: "unicode_bmp",
        2: "shift_jis",  # japanese
        3: "gb2312",  # chinese (simplified)
        4: "big5",  # chinese (traditional)
        5: "euc_kr",  # korean
        6: "johab",  # korean
        # 7, 8, 9 are reserved
        10: "unicode_full",
    },
}
ENCODING_CODE_TO_ID_FOR_PLATFORM: Dict[int, Dict[str, int]] = {
    platform_id: {
        code: encoding_id for encoding_id, code in platform_encoding_id_to_code.items()
    }
    for (
        platform_id,
        platform_encoding_id_to_code,
    ) in ENCODING_ID_TO_CODE_FOR_PLATFORM.items()
}

# Macintosh/windows codes are lowercased IETF BCP 47.
# Doesn't include languages >= 0x8000 (LangTagRecord), those are handled separately.
# fonttools calls the string "lang" and the int "code",
# but we call the string "code" and the int "language_id".
# NOTE codes CAN have duplicates within platform, but they must be
# removed when reversed below
LANGUAGE_ID_TO_CODE_FOR_PLATFORM: Dict[int, Dict[int, str]] = {
    PLATFORM_ID_UNICODE: {
        0: "default",
    },
    PLATFORM_ID_MACINTOSH: {
        language_id: code.lower() for language_id, code in _MAC_LANGUAGES.items()
    },
    PLATFORM_ID_WINDOWS: {
        language_id: code.lower() for language_id, code in _WINDOWS_LANGUAGES.items()
    },
}
LANGUAGE_CODE_TO_ID_FOR_PLATFORM: Dict[int, Dict[str, int]] = {
    platform_id: {
        code: language_id
        for language_id, code in platform_language_id_to_code.items()
        # Remove codes with multiple ids
        if not (
            (platform_id == PLATFORM_ID_MACINTOSH and code in {"es", "ga"})
            or (platform_id == PLATFORM_ID_WINDOWS and code in {"es", "sms"})
        )
    }
    for (
        platform_id,
        platform_language_id_to_code,
    ) in LANGUAGE_ID_TO_CODE_FOR_PLATFORM.items()
}

# NOTE codes must be unique across all name ids
NAME_ID_TO_CODES: Dict[int, Tuple[str, ...]] = {
    0: ("copyright_notice", "copyright"),
    1: ("font_family_name", "font_family", "family"),
    2: ("font_subfamily_name", "font_subfamily", "subfamily"),
    3: (
        "unique_font_identifier",
        "unique_identifier",
        "unique_subfamily_identification",
        "unique",
    ),
    4: ("full_font_name", "full_name"),
    5: ("version_string", "version"),
    6: ("postscript_name",),
    7: ("trademark_notice", "trademark"),
    8: ("manufacturer_name", "manufacturer"),
    9: ("designer_name", "designer"),
    10: ("description",),
    11: ("vendor_url",),
    12: ("designer_url",),
    13: ("license_description",),
    14: ("license_information_url", "license_info_url", "license_url"),
    # 15 is reserved
    16: (
        "typographic_family_name",
        "typographic_family",
        "preferred_family_name",
        "preferred_family",
    ),
    17: (
        "typographic_subfamily_name",
        "typographic_subfamily",
        "preferred_subfamily_name",
        "preferred_subfamily",
    ),
    18: ("compatible_full_name", "compatible_full"),
    19: ("sample_text", "sample"),
    20: ("postscript_cid_findfont_name",),
    21: ("wws_family_name", "wws_family"),
    22: ("wws_subfamily_name", "wws_subfamily"),
    23: ("light_background_palette_id", "light_background_palette"),
    24: ("dark_background_palette_id", "dark_background_palette"),
    25: ("variations_postscript_name_prefix",),
}
NAME_CODE_TO_ID: Dict[str, int] = {
    code: name_id for name_id, codes in NAME_ID_TO_CODES.items() for code in codes
}


# NOTE font["name"] might not exist.
# font["name"]["names"] might not exist either (bug?), see my pull request:
# https://github.com/fonttools/fonttools/pull/3732
class FontNameTool:
    def __init__(self, font: Union[TTFont, str]):
        self.font = font if isinstance(font, TTFont) else TTFont(file=font)

        if "name" not in self.font:
            self.font["name"] = table__n_a_m_e()
        if not hasattr(self.font["name"], "names"):
            self.font["name"].names = []

    @staticmethod
    def record_to_tuple(record: NameRecord) -> Tuple[int, int, int, int, str]:
        return (
            record.platformID,
            record.platEncID,
            record.langID,
            record.nameID,
            record.toStr(),
        )

    def is_otf(self) -> bool:
        return str(self.font.sfntVersion) == "OTTO"

    def is_name_id_referenced(self, name_id: int) -> bool:
        if name_id >= 0x100 and name_id < 0x8000:
            visitor = NameRecordVisitor()
            visitor.visit(self.font)
            return name_id in visitor.seen
        return False

    def resolve_and_validate_record_parts(
        self,
        platform: Union[int, str],
        encoding: Union[int, str],
        language: Union[int, str],
        name: Union[int, str],
        validate_ids: bool = True,
    ) -> NameRecord:
        MAX_ID = 0xFFFF  # H, unsigned short, two bytes

        def resolve_part(
            label: str,
            value: Union[int, str],
            id_lookup: Dict[int, Any],
            code_lookup: Dict[str, int],
        ) -> int:
            if isinstance(value, int):
                if validate_ids and value not in id_lookup:
                    raise ValueError(f"Invalid {label} ID ({value})")
                # even with validate_ids=False,
                # it still must be able to fit in two bytes
                if value > MAX_ID:
                    raise ValueError(
                        f"Invalid {label} ID ({value}), must not exceed 0xFFFF"
                    )
                return value
            else:
                if value not in code_lookup:
                    for_platform_suffix = "" if label == "platform" else " for platform"
                    raise ValueError(f"Invalid {label} code{for_platform_suffix}")
                return code_lookup[value]

        platform_id = resolve_part(
            "platform", platform, PLATFORM_ID_TO_CODES, PLATFORM_CODE_TO_ID
        )

        encoding_id = resolve_part(
            "encoding",
            encoding,
            ENCODING_ID_TO_CODE_FOR_PLATFORM.get(platform_id, {}),
            ENCODING_CODE_TO_ID_FOR_PLATFORM.get(platform_id, {}),
        )

        # Special case for language IDs trying to reference the ltag table
        if (
            validate_ids
            and isinstance(language, int)
            and language >= 0x8000
            and language <= MAX_ID
        ):
            # OTF with version 1 name table
            language_id = language
            if not self.is_otf():
                raise ValueError(
                    f"Language ID {language_id} greater than or equal to 0x8000 "
                    + "can only be used with OTF fonts"
                )
            # Only works with table version 1 (OTF-only), not 0
            # TODO the format (version) attribute doesn't actually exist yet.
            # https://github.com/fonttools/fonttools/issues/3051
            # UNTESTED
            if getattr(self.font["name"], "format", None) != 1:
                raise ValueError(
                    f"Language ID {language_id} greater than or equal to 0x8000 "
                    + "can only be used with a version 1 'name' table (UNSUPPORTED)"
                )
            if "ltag" not in self.font:
                raise ValueError(
                    f"Language ID {language_id} greater than or equal to 0x8000 "
                    + "can only be used with an existing 'ltag' table"
                )
            if (language_id - 0x8000) >= len(self.font["ltag"].tags):
                raise ValueError(
                    f"Language ID {language_id} greater than or equal to 0x8000 "
                    + "is not present in the 'ltag' table"
                )
        elif (
            validate_ids
            and isinstance(language, int)
            and platform_id == PLATFORM_ID_UNICODE
            and language > 0  # language=0 may or may not refer to an ltag
            and language <= MAX_ID
        ):
            # TTF
            # Apple's documentation on this is a bit unclear.
            # The page for the name table says language_id must be set to 0. But on
            # the page for the ltag table page, it says "The numeric codes are used
            # in language field of the 'name' table for strings with a Unicode
            # platform", which is what fonttools seems to do in _makeMacName(),
            # so we'll allow it too.
            language_id = language
            if self.is_otf():
                raise ValueError(
                    f"Language ID {language_id} greater than 0 with the Unicode "
                    + "platform can only be used with TTF fonts"
                )
            if "ltag" not in self.font:
                raise ValueError(
                    f"Language ID {language_id} greater than 0 with the Unicode "
                    + "platform can only be used with an existing 'ltag' table"
                )
            if language_id >= len(self.font["ltag"].tags):
                raise ValueError(
                    f"Language ID {language_id} greater than 0 with the Unicode "
                    + "platform is not present in the 'ltag' table"
                )
        else:
            language_id = resolve_part(
                "language",
                language,
                LANGUAGE_ID_TO_CODE_FOR_PLATFORM.get(platform_id, {}),
                LANGUAGE_CODE_TO_ID_FOR_PLATFORM.get(platform_id, {}),
            )

        # Special case for name IDs referenced by other tables
        # Range is 256 to 32767 inclusive => [256, 32768) => [0x100, 0x8000)
        # Use NameRecordVisitor to check if it's referenced
        # (borrowed from table__n_a_m_e.removeUnusedNames())
        if validate_ids and isinstance(name, int) and name >= 0x100 and name < 0x8000:
            name_id = name
            if not self.is_name_id_referenced(name_id):
                raise ValueError(
                    f"Name ID {name_id} is between 0xFF and 0x8000 (exclusive)"
                    + " but is not referenced in other font tables"
                )
        else:
            name_id = resolve_part("name", name, NAME_ID_TO_CODES, NAME_CODE_TO_ID)

        # Special cases for standard name IDs
        if validate_ids:
            # compatible_full_name is macintosh-only
            if name_id == 18 and platform_id != PLATFORM_ID_MACINTOSH:
                raise ValueError(
                    f"Name ID 18 ({NAME_ID_TO_CODES[18][0]}) is"
                    + f" {PLATFORM_ID_TO_CODES[PLATFORM_ID_MACINTOSH][0]}-only"
                )

        return (platform_id, encoding_id, language_id, name_id)

    def get_records(
        self,
        platform_id: Optional[int] = None,
        encoding_id: Optional[int] = None,
        language_id: Optional[int] = None,
        name_id: Optional[int] = None,
    ) -> List[NameRecord]:
        records = self.font["name"].names
        filtered = []
        for record in records:
            if (
                (platform_id is None or record.platformID == platform_id)
                and (encoding_id is None or record.platEncID == encoding_id)
                and (language_id is None or record.langID == language_id)
                and (name_id is None or record.nameID == name_id)
            ):
                filtered.append(record)
        return filtered

    def set_record(
        self,
        platform_id: str,
        encoding_id: str,
        language_id: str,
        name_id: str,
        string: str,
    ) -> None:
        """Add or update record"""
        self.font["name"].setName(
            string,
            name_id,
            platform_id,
            encoding_id,
            language_id,
        )
        self.font["name"].names.sort()  # sort every time

    def remove_records(
        self,
        platform_id: Optional[int] = None,
        encoding_id: Optional[int] = None,
        language_id: Optional[int] = None,
        name_id: Optional[int] = None,
        validate_name_id_usage: bool = True,
    ) -> None:
        if validate_name_id_usage and name_id is not None:
            # Check if any other table depends on this name_id
            if self.is_name_id_referenced(name_id):
                raise ValueError(
                    f"Attempting to remove name ID {name_id}, but it is "
                    + "referenced in other tables"
                )
        self.font["name"].removeNames(name_id, platform_id, encoding_id, language_id)

    def clear_records(self, validate_name_id_usage: bool = True) -> None:
        if validate_name_id_usage:
            for record in self.get_records():
                if self.is_name_id_referenced(record.nameID):
                    raise ValueError(
                        f"Attempting to remove record with name ID {record.nameID}, "
                        + "but it is referenced in other tables"
                    )
        self.font["name"].names = []

    def save(self, output_path: str) -> None:
        self.font.save(output_path)


def records_to_rows(records: List[NameRecord]) -> List[List[str]]:
    def format_cell(the_id: int, code: Optional[str]) -> str:
        return f"{the_id}{f' ({code})' if code else ''}"

    rows = []
    for record in records:
        (
            platform_id,
            encoding_id,
            language_id,
            name_id,
            string,
        ) = FontNameTool.record_to_tuple(record)
        # Get preferred codes if they exist
        platform_code = None
        encoding_code = None
        language_code = None
        name_code = None
        if platform_id in PLATFORM_ID_TO_CODES:
            platform_code = PLATFORM_ID_TO_CODES[platform_id][0]
            encoding_code = ENCODING_ID_TO_CODE_FOR_PLATFORM[platform_id].get(
                encoding_id, None
            )
            if platform_id != PLATFORM_ID_UNICODE:
                # Don't display the language code with Unicode ("default")
                # because there's a slight chance it could refer
                # to an ltag rather than "default".
                language_code = LANGUAGE_ID_TO_CODE_FOR_PLATFORM[platform_id].get(
                    language_id, None
                )
        if name_id in NAME_ID_TO_CODES:
            name_code = NAME_ID_TO_CODES[name_id][0]
        rows.append(
            [
                format_cell(platform_id, platform_code),
                format_cell(encoding_id, encoding_code),
                format_cell(language_id, language_code),
                format_cell(name_id, name_code),
                string,
            ]
        )
    return rows


def format_table_rows(rows: List[List[str]], padding: int = 2) -> str:
    """Space-separated table"""
    string_rows = [[str(val) for val in row] for row in rows]

    # Get col widths
    col_widths = []
    for col_idx in range(len(rows[0])):
        width = max(len(row[col_idx]) for row in string_rows)
        col_widths.append(width)

    output = []
    for row in string_rows:
        line = ""
        for i, val in enumerate(row):
            if i < len(row) - 1:
                line += val.ljust(col_widths[i] + padding)
            else:
                line += val  # last col
        output.append(line)
    return "\n".join(output)


def format_text_output(records: List[NameRecord]) -> str:
    return format_table_rows(
        [
            ["PLATFORM", "ENCODING", "LANGUAGE", "NAME", "STRING"],
            *records_to_rows(records),
        ]
    )


def format_json_output(records: List[NameRecord]) -> str:
    # custom outer array stringification to keep it readable yet compact
    INDENT = 2
    inner_lines = []
    for record in records:
        inner_json_string = json.dumps(
            FontNameTool.record_to_tuple(record), indent=None
        )
        inner_lines.append(f"{' ' * INDENT}{inner_json_string}")
    return "[\n" + ",\n".join(inner_lines) + "\n]"


class CliArgumentParser:
    COMMAND_PRINT: str = "print"
    COMMAND_SET: str = "set"
    COMMAND_REPLACE: str = "replace"
    COMMAND_REMOVE: str = "remove"

    @staticmethod
    def add_common_args(parser: argparse.ArgumentParser) -> None:
        parser.add_argument("font_file", help="Input font file")

    @staticmethod
    def add_printing_args(
        parser: argparse.ArgumentParser, allow_quiet: bool = True
    ) -> None:
        pg = (
            parser.add_mutually_exclusive_group(required=False)
            if allow_quiet
            else parser
        )
        pg.add_argument(
            "--print-json",
            action="store_true",
            help="Print JSON instead of table",
        )
        if allow_quiet:
            pg.add_argument("--quiet", action="store_true", help="Don't print anything")

    @staticmethod
    def add_filter_args(parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--platform-id",
            type=CliArgumentParser.whole_base10_or_base16_int,
            help="Platform ID filter",
        )
        parser.add_argument(
            "--encoding-id",
            type=CliArgumentParser.whole_base10_or_base16_int,
            help="Encoding ID filter",
        )
        parser.add_argument(
            "--language-id",
            type=CliArgumentParser.whole_base10_or_base16_int,
            help="Language ID filter",
        )
        parser.add_argument(
            "--name-id",
            type=CliArgumentParser.whole_base10_or_base16_int,
            help="Name ID filter",
        )

    @staticmethod
    def add_output_args(parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--dry-run", action="store_true", help="Show changes without applying them"
        )
        # NOTE one of these is still required, unless --dry-run is set (checked later)
        output_group = parser.add_mutually_exclusive_group(required=False)
        output_group.add_argument("--output", help="Output font file")
        output_group.add_argument(
            "--in-place", action="store_true", help="Modify the input file in-place"
        )

    @staticmethod
    def add_input_args(parser: argparse.ArgumentParser) -> None:
        parser.add_argument(
            "--no-validation",
            action="store_true",
            help="Don't validate ids or their combinations",
        )
        input_data_group = parser.add_mutually_exclusive_group(required=True)
        input_data_group.add_argument(
            "--record",
            action="append",
            nargs=5,
            metavar=("PLATFORM", "ENCODING", "LANGUAGE", "NAME", "STRING"),
            help="Record as: platform encoding language name string "
            + "(supports multiple records)",
        )
        input_data_group.add_argument(
            "--json-input-string",
            help="JSON string containing records (e.g. [[1, 0, 0, 1, 'My Font'], ...])",
        )
        input_data_group.add_argument(
            "--json-input-file",
            help="JSON file containing records (e.g. [[1, 0, 0, 1, 'My Font'], ...])",
        )

    @staticmethod
    def whole_base10_or_base16_int(string: str) -> int:
        """Parse base10 or base16 whole integer number to an int"""
        result = CliArgumentParser.whole_base10_or_base16_int_or_code(string)
        if isinstance(result, str):
            raise ValueError(f"Invalid whole integer: {string}")
        return result

    @staticmethod
    def whole_base10_or_base16_int_or_code(string: str) -> Union[int, str]:
        """
        Try to parse base10 or base16 whole number to an int.
        If not, return normalized string as a code (if not empty)
        """
        string_stripped_lowered = string.strip().lower()
        if not string_stripped_lowered:
            raise ValueError("Empty string")

        # Start looks like hex?
        if re.search(r"(?m)\A-?0x", string_stripped_lowered):
            if not re.search(r"(?m)\A0x[0-9a-f]+\Z", string_stripped_lowered):
                raise ValueError(
                    f"Invalid base 16 whole number: {string}"
                )  # e.g. '-0x1', 0x 1', '0x-1'
            return int(string_stripped_lowered[2:], 16)
        # Start looks like decimal?
        elif re.search(r"(?m)\A-?[0-9]", string_stripped_lowered):
            if not re.search(r"(?m)\A[0-9]+\Z", string_stripped_lowered):
                raise ValueError(
                    f"Invalid base 10 whole number: {string}"
                )  # e.g. '-1', '1a'
            return int(string_stripped_lowered)
        elif re.search(r"(?m)\A[a-z][a-z0-9_-]+\Z", string_stripped_lowered):
            return string_stripped_lowered  # likely code
        raise ValueError(f"Invalid whole number or code: {string}")

    @staticmethod
    def parse_args() -> argparse.Namespace:
        parser = argparse.ArgumentParser(
            description="Read and write font name tables",
            allow_abbrev=False,
        )
        parser.add_argument(
            "--version",
            action="version",
            version=metadata.version("font-name-tool"),
            help="Print the program version",
        )
        subparsers = parser.add_subparsers(dest="command", required=True)

        # Print command
        print_parser = subparsers.add_parser(
            CliArgumentParser.COMMAND_PRINT, help="Print all name table records"
        )
        CliArgumentParser.add_common_args(print_parser)
        CliArgumentParser.add_printing_args(print_parser, allow_quiet=False)

        # Set command
        set_parser = subparsers.add_parser(
            CliArgumentParser.COMMAND_SET,
            help="Set (add or update) name table records",
        )
        CliArgumentParser.add_common_args(set_parser)
        CliArgumentParser.add_printing_args(set_parser, allow_quiet=True)
        CliArgumentParser.add_output_args(set_parser)
        CliArgumentParser.add_input_args(set_parser)

        # Replace command
        replace_parser = subparsers.add_parser(
            CliArgumentParser.COMMAND_REPLACE, help="Replace entire name table"
        )
        CliArgumentParser.add_common_args(replace_parser)
        CliArgumentParser.add_printing_args(replace_parser, allow_quiet=True)
        CliArgumentParser.add_output_args(replace_parser)
        CliArgumentParser.add_input_args(replace_parser)

        # Remove command
        remove_parser = subparsers.add_parser(
            CliArgumentParser.COMMAND_REMOVE,
            help="Remove name table records matching all filters",
        )
        CliArgumentParser.add_common_args(remove_parser)
        CliArgumentParser.add_printing_args(remove_parser, allow_quiet=True)
        CliArgumentParser.add_output_args(remove_parser)
        remove_parser.add_argument(
            "--no-validation",
            action="store_true",
            help="Don't validate name IDs that may be referenced in other tables",
        )
        CliArgumentParser.add_filter_args(remove_parser)

        args = parser.parse_args()

        if args.command in {
            CliArgumentParser.COMMAND_SET,
            CliArgumentParser.COMMAND_REPLACE,
            CliArgumentParser.COMMAND_REMOVE,
        }:
            # prevent --quiet with --dry-run
            if args.dry_run and args.quiet:
                parser.error("argument --dry-run: not allowed with argument --quiet")
            # if not --dry-run, ensure either --in-place or --output are set
            if not args.dry_run and not args.in_place and not args.output:
                parser.error("must set [--output OUTPUT | --in-place] or --dry-run")

        if args.command == CliArgumentParser.COMMAND_REMOVE:
            # Must have at least one filter
            if not any(
                (args.platform_id, args.encoding_id, args.language_id, args.name_id)
            ):
                parser.error(
                    "at least one of the arguments --platform-id --encoding-id "
                    + "--language-id --name-id is required"
                )

        return args


def get_input_data(args: argparse.Namespace) -> List[List[Union[int, str]]]:
    def validate_json_data(data: Any) -> None:
        # Validate structure (NOT including ID validation at this stage)
        if not isinstance(data, list):
            raise ValueError("JSON outer structure is not an array")
        for i, raw_record in enumerate(data):
            if not isinstance(raw_record, list):
                raise ValueError(f"JSON record at index {i} is not an array")
            if len(raw_record) != 5:
                raise ValueError(
                    f"JSON record at index {i} does not have exactly five elements"
                )
            if not isinstance(raw_record[4], str):
                raise ValueError(
                    f"JSON record at index {i} does not have a string at index 4"
                )
            for j, x in enumerate(raw_record[:4]):
                # Only allow literal integer IDs for now (no hex strings or codes)
                if not isinstance(x, int):
                    raise ValueError(
                        f"JSON record at index {i} does not have an "
                        + f"integer ID at index {j}"
                    )

    if args.record:
        data = []
        for record_args in args.record:
            raw_record = [
                CliArgumentParser.whole_base10_or_base16_int_or_code(arg)
                for arg in record_args[:4]
            ]
            raw_record.append(record_args[4])
            data.append(raw_record)
        return data
    elif args.json_input_string:
        data = json.loads(args.json_input_string)
        validate_json_data(data)
        return data
    elif args.json_input_file:
        if args.json_input_file == "-":
            data = json.load(sys.stdin)
        else:
            with open(args.json_input_file) as f:
                data = json.load(f)
        validate_json_data(data)
        return data
    raise Exception("couldn't find input data in args")


def handle_exception(exception: BaseException) -> None:
    """
    High-level exception handler. Print a user-friendly message,
    unless we're in debug mode, then re-raise exception and
    let Python handle it normally.
    """
    debug_mode = os.environ.get("FONT_NAME_TOOL_DEBUG", "").lower() in {
        "1",
        "true",
        "yes",
    }

    if debug_mode:
        raise exception
    elif isinstance(exception, SystemExit):
        raise exception
    else:
        message = None
        if isinstance(exception, OSError):
            if exception.filename:
                message = f"{exception.strerror}: '{exception.filename}'"
            else:
                message = exception.strerror
        else:
            raw_exception_string = str(exception)
            if raw_exception_string:
                message = raw_exception_string
        full_message = f"[{type(exception).__name__}]"
        if message:
            full_message += f" {message}"
        print(full_message, file=sys.stderr)
        sys.exit(1)


def main() -> None:
    try:
        args = CliArgumentParser.parse_args()
        tool = FontNameTool(args.font_file)

        if args.command == CliArgumentParser.COMMAND_PRINT:
            pass  # print at end
        elif args.command in {
            CliArgumentParser.COMMAND_SET,
            CliArgumentParser.COMMAND_REPLACE,
        }:
            if args.command == CliArgumentParser.COMMAND_REPLACE:
                tool.clear_records(validate_name_id_usage=not args.no_validation)
            for raw_record in get_input_data(args):
                (
                    platform_id,
                    encoding_id,
                    language_id,
                    name_id,
                ) = tool.resolve_and_validate_record_parts(
                    raw_record[0],
                    raw_record[1],
                    raw_record[2],
                    raw_record[3],
                    validate_ids=not args.no_validation,
                )
                tool.set_record(
                    platform_id, encoding_id, language_id, name_id, str(raw_record[4])
                )
        elif args.command == CliArgumentParser.COMMAND_REMOVE:
            tool.remove_records(
                args.platform_id,
                args.encoding_id,
                args.language_id,
                args.name_id,
                validate_name_id_usage=not args.no_validation,
            )

        # Save
        if args.command in {
            CliArgumentParser.COMMAND_SET,
            CliArgumentParser.COMMAND_REPLACE,
            CliArgumentParser.COMMAND_REMOVE,
        }:
            if args.dry_run:
                print("=========DRY RUN=========")
            else:
                output_path = args.font_file if args.in_place else args.output
                tool.save(output_path)

        # Print
        if args.command == CliArgumentParser.COMMAND_PRINT or not args.quiet:
            records = tool.get_records()
            print(
                format_json_output(records)
                if args.print_json
                else format_text_output(records)
            )
    except BaseException as exception:
        handle_exception(exception)


if __name__ == "__main__":
    main()
