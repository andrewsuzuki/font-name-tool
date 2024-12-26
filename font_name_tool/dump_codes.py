from typing import Dict

from font_name_tool.cli import (
    ENCODING_ID_TO_CODE_FOR_PLATFORM,
    LANGUAGE_ID_TO_CODE_FOR_PLATFORM,
    NAME_ID_TO_CODES,
    PLATFORM_ID_TO_CODES,
)


def print_codes_tuples(prefix: str, d: Dict) -> None:
    for i, t in d.items():
        print(f"{prefix} {i}: ({', '.join(t)})")


def print_codes_for_platform(prefix: str, d_by_platform: Dict) -> None:
    for platform_id, d in d_by_platform.items():
        for i, code in d.items():
            print(f"{prefix} ({PLATFORM_ID_TO_CODES[platform_id][0]}) {i}: {code}")


print_codes_tuples("PLATFORM", PLATFORM_ID_TO_CODES)
print_codes_for_platform("ENCODING", ENCODING_ID_TO_CODE_FOR_PLATFORM)
print_codes_for_platform("LANGUAGE", LANGUAGE_ID_TO_CODE_FOR_PLATFORM)
print_codes_tuples("NAME", NAME_ID_TO_CODES)
