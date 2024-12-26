import json
import os
import subprocess
import sys
import tempfile
import unittest
from io import StringIO
from pathlib import Path
from unittest.mock import patch  # MagicMock

from fontTools.ttLib import TTFont
from fontTools.ttLib.tables._f_v_a_r import Axis, table__f_v_a_r
from fontTools.ttLib.tables._l_t_a_g import table__l_t_a_g
from fontTools.ttLib.tables._n_a_m_e import NameRecord, table__n_a_m_e

from font_name_tool.cli import (
    ENCODING_CODE_TO_ID_FOR_PLATFORM,
    ENCODING_ID_TO_CODE_FOR_PLATFORM,
    LANGUAGE_CODE_TO_ID_FOR_PLATFORM,
    LANGUAGE_ID_TO_CODE_FOR_PLATFORM,
    NAME_CODE_TO_ID,
    NAME_ID_TO_CODES,
    PLATFORM_CODE_TO_ID,
    PLATFORM_ID_MACINTOSH,
    PLATFORM_ID_TO_CODES,
    PLATFORM_ID_UNICODE,
    PLATFORM_ID_WINDOWS,
    CliArgumentParser,
    FontNameTool,
    format_json_output,
    format_table_rows,
    format_text_output,
    handle_exception,
    records_to_rows,
)


class TestConstants(unittest.TestCase):
    platform_ids = {
        PLATFORM_ID_UNICODE,
        PLATFORM_ID_MACINTOSH,
        PLATFORM_ID_WINDOWS,
    }

    @staticmethod
    def has_all_platforms(d):
        return set(d.keys()) == TestConstants.platform_ids

    def assert_ids_and_codes_for_platform(
        self, ids_to_codes, codes_to_ids, duplicate_codes_ok=False
    ):
        """helper for encodings and languages"""
        self.assertTrue(TestConstants.has_all_platforms(ids_to_codes))
        self.assertTrue(TestConstants.has_all_platforms(codes_to_ids))
        for platform_id in TestConstants.platform_ids:
            self.assertIsInstance(ids_to_codes[platform_id], dict)
            for code in ids_to_codes[platform_id].values():
                is_str = isinstance(code, str)
                self.assertTrue(is_str)
                if is_str:
                    self.assertTrue(code)
                    self.assertEqual(code.lower(), code)
                    self.assertRegex(code, r"^[a-z][a-z0-9_-]+$")
                    # implied by the above regex now:
                    # self.assertNotRegex(code, r"^\d+$")  # not only digits
                    # self.assertNotRegex(code, r"^0[xX]")  # not starting with 0x or 0X
                    # self.assertNotRegex(code, r"^[-_]")  # not starting with - or _
            for id in ids_to_codes[platform_id].keys():
                is_int = isinstance(id, int)
                self.assertTrue(is_int)
                if is_int:
                    self.assertGreaterEqual(id, 0)
                    self.assertLess(id, 0x8000)

            self.assertIsInstance(codes_to_ids[platform_id], dict)

            self.assertEqual(
                len(set(codes_to_ids[platform_id].values())),
                len(codes_to_ids[platform_id].values()),
                "ids in code->id dict must be unique for platform",
            )
            if duplicate_codes_ok:
                self.assertTrue(
                    set(ids_to_codes[platform_id].values()).issuperset(
                        set(codes_to_ids[platform_id].keys())
                    ),
                    "codes in id->code dict must be superset of codes in code->id dict",
                )
                self.assertTrue(
                    set(ids_to_codes[platform_id].keys()).issuperset(
                        set(codes_to_ids[platform_id].values())
                    ),
                    "ids in id->code dict must be superset of ids in code->id dict",
                )
            else:
                self.assertEqual(
                    len(set(ids_to_codes[platform_id].values())),
                    len(ids_to_codes[platform_id].values()),
                    "codes in id->code dict must be unique for platform",
                )
                self.assertEqual(
                    len(set(codes_to_ids[platform_id].values())),
                    len(codes_to_ids[platform_id].values()),
                    "ids in code->id dict must be unique for platform",
                )
                self.assertEqual(
                    set(ids_to_codes[platform_id].values()),
                    set(codes_to_ids[platform_id].keys()),
                    "codes in id->code dict must match codes in code->id dict",
                )
                self.assertEqual(
                    set(ids_to_codes[platform_id].keys()),
                    set(codes_to_ids[platform_id].values()),
                    "ids in id->code dict must match ids in code->id dict",
                )

    def test_platforms(self):
        self.assertEqual(PLATFORM_ID_UNICODE, 0)
        self.assertEqual(PLATFORM_ID_MACINTOSH, 1)
        self.assertEqual(PLATFORM_ID_WINDOWS, 3)

        self.assertTrue(TestConstants.has_all_platforms(PLATFORM_ID_TO_CODES))
        all_codes = set()
        for platform_id in TestConstants.platform_ids:
            self.assertIsInstance(PLATFORM_ID_TO_CODES[platform_id], tuple)
            self.assertIsInstance(PLATFORM_ID_TO_CODES[platform_id][0], str)
            for code in PLATFORM_ID_TO_CODES[platform_id]:
                self.assertRegex(code, r"^[a-z_]+$")
                all_codes.add(code)
        self.assertEqual(all_codes, set(PLATFORM_CODE_TO_ID.keys()))
        self.assertEqual(PLATFORM_CODE_TO_ID["macintosh"], PLATFORM_ID_MACINTOSH)
        self.assertEqual(PLATFORM_CODE_TO_ID["mac"], PLATFORM_ID_MACINTOSH)

    def test_encodings(self):
        self.assert_ids_and_codes_for_platform(
            ENCODING_ID_TO_CODE_FOR_PLATFORM,
            ENCODING_CODE_TO_ID_FOR_PLATFORM,
            duplicate_codes_ok=False,
        )
        self.assertEqual(
            ENCODING_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_UNICODE][3], "unicode_bmp"
        )
        self.assertEqual(
            ENCODING_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_MACINTOSH][0], "roman"
        )
        self.assertEqual(
            ENCODING_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_WINDOWS][1], "unicode_bmp"
        )
        self.assertEqual(
            ENCODING_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_UNICODE]["unicode_bmp"], 3
        )
        self.assertEqual(
            ENCODING_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_MACINTOSH]["roman"], 0
        )
        self.assertEqual(
            ENCODING_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_WINDOWS]["unicode_bmp"], 1
        )

    def test_languages(self):
        self.assert_ids_and_codes_for_platform(
            LANGUAGE_ID_TO_CODE_FOR_PLATFORM,
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM,
            duplicate_codes_ok=True,  # check specifics below
        )

        # Since these rely on imported private constants from fonttools,
        # track the exact number
        LANGUAGE_ID_COUNT_MACINTOSH = 119
        LANGUAGE_ID_COUNT_WINDOWS = 205
        self.assertEqual(
            len(LANGUAGE_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_MACINTOSH]),
            LANGUAGE_ID_COUNT_MACINTOSH,
        )
        self.assertEqual(
            len(LANGUAGE_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_WINDOWS]),
            LANGUAGE_ID_COUNT_WINDOWS,
        )

        # Removed duplicates
        # (es and ga for macintosh, and es and sms for windows)
        self.assertEqual(
            len(LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_MACINTOSH]),
            LANGUAGE_ID_COUNT_MACINTOSH - (2 * 2),
        )
        self.assertEqual(
            len(LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_WINDOWS]),
            LANGUAGE_ID_COUNT_WINDOWS - (2 * 2),
        )
        self.assertIsNone(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_MACINTOSH].get("es")
        )
        self.assertIsNone(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_MACINTOSH].get("ga")
        )
        self.assertIsNone(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_WINDOWS].get("es")
        )
        self.assertIsNone(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_WINDOWS].get("sms")
        )

        self.assertEqual(
            LANGUAGE_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_UNICODE][0], "default"
        )
        self.assertEqual(
            LANGUAGE_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_MACINTOSH][0], "en"
        )
        self.assertEqual(
            LANGUAGE_ID_TO_CODE_FOR_PLATFORM[PLATFORM_ID_WINDOWS][0x0409], "en"
        )
        self.assertEqual(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_UNICODE]["default"], 0
        )
        self.assertEqual(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_MACINTOSH]["en"], 0
        )
        self.assertEqual(
            LANGUAGE_CODE_TO_ID_FOR_PLATFORM[PLATFORM_ID_WINDOWS]["en"], 0x0409
        )

    def test_names(self):
        all_codes = set()
        for name_id, codes in NAME_ID_TO_CODES.items():
            self.assertIsInstance(name_id, int)
            self.assertIsInstance(codes, tuple)
            self.assertIsInstance(codes[0], str)
            for code in codes:
                self.assertRegex(code, r"^[a-z_]+$")
                all_codes.add(code)
        self.assertEqual(len(set(all_codes)), len(all_codes))
        self.assertEqual(set(all_codes), set(NAME_CODE_TO_ID.keys()))
        self.assertEqual(set(NAME_ID_TO_CODES.keys()), set(NAME_CODE_TO_ID.values()))


class TestFontNameTool(unittest.TestCase):
    def test_record_to_tuple_conversion(self):
        """Test conversion of NameRecord to tuple format"""
        record = NameRecord()
        record.platformID = 1
        record.platEncID = 0
        record.langID = 0
        record.nameID = 1
        record.string = "Test Font"
        expected = (1, 0, 0, 1, "Test Font")
        result = FontNameTool.record_to_tuple(record)
        self.assertEqual(result, expected)

    def test_init_new_font_without_existing_name_table(self):
        font = TTFont()
        tool = FontNameTool(font)
        self.assertEqual(tool.font, font)
        self.assertIsInstance(tool.font["name"], table__n_a_m_e)
        self.assertEqual(tool.font["name"].names, [])

    def test_init_new_font_with_existing_name_table(self):
        font = TTFont()
        table = table__n_a_m_e()
        font["name"] = table
        font["name"].names = []
        font["name"].setName("test font", 1, 1, 0, 0)
        tool = FontNameTool(font)
        self.assertEqual(tool.font, font)
        self.assertEqual(tool.font["name"], table)
        self.assertEqual(tool.font["name"].names[0].string, "test font")

    def test_record_crud_methods(self):
        tool = FontNameTool(TTFont())
        self.assertEqual(tool.get_records(), [])

        tool.set_record(1, 0, 0, 1, "test font")
        self.assertIsInstance(tool.get_records(), list)
        self.assertEqual(len(tool.get_records()), 1)

        mac_family_record = tool.get_records()[0]
        self.assertIsInstance(mac_family_record, NameRecord)
        self.assertEqual(mac_family_record.platformID, 1)
        self.assertEqual(mac_family_record.platEncID, 0)
        self.assertEqual(mac_family_record.langID, 0)
        self.assertEqual(mac_family_record.nameID, 1)
        self.assertEqual(mac_family_record.string, "test font")

        # mutates same underlying NameRecord
        tool.set_record(1, 0, 0, 1, "test font 2")
        self.assertEqual(len(tool.get_records()), 1)
        self.assertEqual(tool.get_records()[0], mac_family_record)
        self.assertEqual(mac_family_record.string, "test font 2")

        # new record with lower sort order
        tool.set_record(1, 0, 0, 0, "test copyright")
        self.assertEqual(len(tool.get_records()), 2)
        mac_copyright_family_record = tool.get_records()[0]
        self.assertEqual(mac_copyright_family_record.string, "test copyright")
        self.assertEqual(tool.get_records()[1], mac_family_record)
        self.assertEqual(mac_family_record.string, "test font 2")  # unchanged

        # get records works with filters
        self.assertEqual(len(tool.get_records(name_id=0)), 1)
        self.assertEqual(len(tool.get_records(name_id=1)), 1)
        self.assertEqual(len(tool.get_records(name_id=2)), 0)
        self.assertEqual(len(tool.get_records(platform_id=1)), 2)
        self.assertEqual(len(tool.get_records(platform_id=2)), 0)
        self.assertEqual(len(tool.get_records(encoding_id=0)), 2)
        self.assertEqual(len(tool.get_records(encoding_id=1)), 0)
        self.assertEqual(len(tool.get_records(language_id=0)), 2)
        self.assertEqual(len(tool.get_records(language_id=1)), 0)
        self.assertEqual(len(tool.get_records(1, 0, 0)), 2)
        self.assertEqual(len(tool.get_records(1, 0, 0, 0)), 1)

        # remove records does nothing with no args
        tool.remove_records()
        self.assertEqual(len(tool.get_records()), 2)
        self.assertIn(mac_copyright_family_record, tool.get_records())
        self.assertIn(mac_family_record, tool.get_records())

        # remove records works with matching name id
        tool.remove_records(name_id=0)
        self.assertEqual(len(tool.get_records()), 1)
        self.assertNotIn(mac_copyright_family_record, tool.get_records())
        self.assertIn(mac_family_record, tool.get_records())

        # remove records fails if name id is referenced in other table
        tool.font["fvar"] = table__f_v_a_r()
        axis = Axis()
        axis.axisNameID = 0x100
        tool.font["fvar"].axes.append(axis)
        tool.set_record(1, 0, 0, 0x100, "test")
        with self.assertRaises(ValueError):
            tool.remove_records(name_id=0x100)

        # clear records fails if a name id is referenced in other table
        with self.assertRaises(ValueError):
            tool.clear_records()
        # but works if we disable the id check
        tool.clear_records(validate_name_id_usage=False)
        self.assertEqual(len(tool.get_records()), 0)

        # clear records works (normal usage)
        tool.set_record(1, 0, 0, 1, "test")
        self.assertEqual(len(tool.get_records()), 1)
        tool.clear_records()
        self.assertEqual(len(tool.get_records()), 0)

    def test_resolve_and_validate_record_parts_valid(self):
        tool = FontNameTool(TTFont())

        # Test with numeric IDs
        result = tool.resolve_and_validate_record_parts(PLATFORM_ID_MACINTOSH, 0, 0, 1)
        self.assertEqual(result, (PLATFORM_ID_MACINTOSH, 0, 0, 1))

        # validate_ids=False
        result = tool.resolve_and_validate_record_parts(2, 0, 0, 0, validate_ids=False)
        self.assertEqual(result, (2, 0, 0, 0))
        result = tool.resolve_and_validate_record_parts(
            PLATFORM_ID_WINDOWS, 3, 1, 18, validate_ids=False
        )
        self.assertEqual(result, (PLATFORM_ID_WINDOWS, 3, 1, 18))
        result = tool.resolve_and_validate_record_parts(
            PLATFORM_ID_MACINTOSH, 0, 0, 0xFFFF, validate_ids=False
        )
        self.assertEqual(result, (PLATFORM_ID_MACINTOSH, 0, 0, 0xFFFF))

        # Test with string codes
        result = tool.resolve_and_validate_record_parts(
            "macintosh", "roman", "en", "font_family"
        )
        self.assertEqual(result, (PLATFORM_ID_MACINTOSH, 0, 0, 1))
        result = tool.resolve_and_validate_record_parts(
            "unicode", "unicode_bmp", "default", "font_family"
        )
        self.assertEqual(result, (PLATFORM_ID_UNICODE, 3, 0, 1))

        tool.resolve_and_validate_record_parts(PLATFORM_ID_MACINTOSH, 1, 0, 18)

        # test language ID referencing ltag table with Unicode/TTF
        tool.font["ltag"] = table__l_t_a_g()
        foo_ltag_index = tool.font["ltag"].addTag("foo")  # 0
        bar_ltag_index = tool.font["ltag"].addTag("bar")  # 1
        result = tool.resolve_and_validate_record_parts(
            PLATFORM_ID_UNICODE, 3, foo_ltag_index, 1
        )
        self.assertEqual(result, (PLATFORM_ID_UNICODE, 3, foo_ltag_index, 1))
        result = tool.resolve_and_validate_record_parts(
            PLATFORM_ID_UNICODE, 3, bar_ltag_index, 1
        )
        self.assertEqual(result, (PLATFORM_ID_UNICODE, 3, bar_ltag_index, 1))

        # Test name ID >= 0x100, which checks if it"s referenced in fvar (among others)
        tool.font["fvar"] = table__f_v_a_r()
        axis = Axis()
        axis.axisNameID = 0x100
        tool.font["fvar"].axes.append(axis)
        result = tool.resolve_and_validate_record_parts(
            PLATFORM_ID_UNICODE, 3, 0, 0x100
        )
        self.assertEqual(result, (PLATFORM_ID_UNICODE, 3, 0, 0x100))

    def test_resolve_and_validate_record_parts_invalid(self):
        tool = FontNameTool(TTFont())

        # Test invalid platform ID
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(2, 0, 0, 1)
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts("foobar", 0, 0, 1)

        # Test invalid encoding ID
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(PLATFORM_ID_MACINTOSH, 99, 0, 1)
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(
                PLATFORM_ID_MACINTOSH, "foobar", 0, 1
            )
        with self.assertRaises(ValueError):
            # roman is macintosh, not unicode
            tool.resolve_and_validate_record_parts(PLATFORM_ID_UNICODE, "roman", 0, 1)

        # Test invalid language ID
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(PLATFORM_ID_MACINTOSH, 0, 99999, 1)
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(
                PLATFORM_ID_UNICODE, "unicode_bmp", "en", 1
            )
        # test language ID trying to reference ltag table with Unicode/TTF
        # (ltag table doesn't exist)
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(PLATFORM_ID_UNICODE, 3, 1, 1)

        # Test invalid name ID
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(PLATFORM_ID_MACINTOSH, 0, 0, 0xFFFF)
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(PLATFORM_ID_MACINTOSH, 0, 0, 0x10000)
        with self.assertRaises(ValueError):
            tool.resolve_and_validate_record_parts(
                PLATFORM_ID_MACINTOSH, 0, 0, 0x10000, validate_ids=True
            )
        with self.assertRaises(ValueError):
            # in [0x100, 0x8000) but not referenced in other table
            tool.resolve_and_validate_record_parts(PLATFORM_ID_UNICODE, 3, 0, 0x100)
        with self.assertRaises(ValueError):
            # 18 only allowed with macintosh
            tool.resolve_and_validate_record_parts(PLATFORM_ID_WINDOWS, 3, 0x0409, 18)

    def test_save(self):
        font = TTFont()
        tool_write = FontNameTool(font)
        tool_write.set_record(1, 0, 0, 1, "test font")

        temp_dir = tempfile.mkdtemp()
        output_path = Path(temp_dir) / "foo.ttf"
        tool_write.save(output_path)
        self.assertTrue(output_path.exists())

        # read it back into a new tool to verify record was set and saved
        tool_read = FontNameTool(output_path)
        self.assertEqual(tool_read.get_records()[0].toStr(), "test font")

        # Clean up
        output_path.unlink(missing_ok=True)
        os.rmdir(temp_dir)


class TestCliArgumentParser(unittest.TestCase):
    # helper
    def assert_whole_base10_or_base16_int_valid(self, f):
        # Test base 10
        self.assertEqual(f("0"), 0)
        self.assertEqual(f("123"), 123)
        self.assertEqual(f("123 "), 123)
        self.assertEqual(f(" 123 "), 123)
        # Test base 16
        self.assertEqual(f("0x0"), 0x0)
        self.assertEqual(f("0xFF"), 0xFF)
        self.assertEqual(f("0xFf"), 0xFF)
        self.assertEqual(f("0xff"), 0xFF)
        self.assertEqual(f("0x0409"), 0x0409)
        self.assertEqual(f("0X0"), 0x0)
        self.assertEqual(f("0XFF"), 0xFF)
        self.assertEqual(f("0XFf"), 0xFF)
        self.assertEqual(f("0Xff"), 0xFF)
        self.assertEqual(f("0X0409"), 0x0409)
        self.assertEqual(f("0X0409 "), 0x0409)
        self.assertEqual(f(" 0X0409 "), 0x0409)

    def test_whole_base10_or_base16_int(self):
        # Valid
        self.assert_whole_base10_or_base16_int_valid(
            CliArgumentParser.whole_base10_or_base16_int
        )
        # Invalid
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int(" ")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("\t")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("abc")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("-0")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("-1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("0x")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("0x-1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("0x 1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("0x -1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("-0x1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int("0b0")

    def test_whole_base10_or_base16_int_or_code(self):
        # Valid
        self.assert_whole_base10_or_base16_int_valid(
            CliArgumentParser.whole_base10_or_base16_int_or_code
        )
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("FOO"), "foo"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo"), "foo"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo0"), "foo0"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo-bar"), "foo-bar"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo-0"), "foo-0"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo_bar"), "foo_bar"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo_0"), "foo_0"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("foo_0 "), "foo_0"
        ),
        self.assertEqual(
            CliArgumentParser.whole_base10_or_base16_int_or_code("\tfOO_0 "), "foo_0"
        ),
        # Invalid
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code(" ")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("\t")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("-0")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("-1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("0x")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("0x-1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("0x 1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("0x -1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("-0x1")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("0b0")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("0aaa")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("-0aaa")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("-foo")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code("_foo")
        with self.assertRaises(ValueError):
            CliArgumentParser.whole_base10_or_base16_int_or_code(" _foo")

    # helper
    def assert_parser_output(
        self,
        argv,
        expect_exit=False,
        expected_exit_code=None,
        expect_stdout=False,
        expect_stderr=False,
    ):
        class ParserExit(Exception):
            pass

        def exit_func(*_):
            raise ParserExit()

        with patch("sys.argv", argv):
            with patch("sys.exit") as mock_exit:
                # Make sys.exit() raise our custom exception
                mock_exit.side_effect = exit_func
                with patch("sys.stderr", new=StringIO()) as fake_err:
                    with patch("sys.stdout", new=StringIO()) as fake_out:
                        args = None
                        try:
                            args = CliArgumentParser.parse_args()
                        except ParserExit:
                            if expect_exit:
                                mock_exit.assert_called_once()
                                if expected_exit_code is not None:
                                    mock_exit.assert_called_with(expected_exit_code)
                            else:
                                self.fail("Parser called sys.exit() unexpectedly")

                        stdout = fake_out.getvalue()
                        stderr = fake_err.getvalue()

                        if expect_stdout:
                            self.assertGreater(len(stdout), 0)
                        else:
                            self.assertEqual(stdout, "")
                        if expect_stderr:
                            self.assertGreater(len(stderr), 0)
                        else:
                            self.assertEqual(stderr, "")

                        return args, stdout, stderr

    def test_help(self):
        tool_name = "foo-tool-name"
        _, stdout, _ = self.assert_parser_output(
            [tool_name, "-h"],
            expect_exit=True,
            expected_exit_code=0,
            expect_stdout=True,
        )
        self.assertIn("positional arguments:", stdout)
        # "options:" for python 3.10+, "optional arguments:" for 3.8/3.9
        self.assertTrue("options:" in stdout or "optional arguments:" in stdout)
        self.assertTrue(stdout.startswith(f"usage: {tool_name} [-h]"))

    def test_print_command_args(self):
        tool_name = "foo-tool-name"

        args, _, _ = self.assert_parser_output([tool_name, "print", "test.ttf"])
        self.assertEqual(args.command, "print")
        self.assertEqual(args.font_file, "test.ttf")
        self.assertFalse(args.print_json)

        args, _, _ = self.assert_parser_output(
            [tool_name, "print", "--print-json", "test.ttf"]
        )
        self.assertEqual(args.command, "print")
        self.assertEqual(args.font_file, "test.ttf")
        self.assertTrue(args.print_json)

        # --quiet shouldn't work with print
        _, _, stderr = self.assert_parser_output(
            [tool_name, "print", "--quiet", "test.ttf"],
            expect_exit=True,
            expected_exit_code=2,
            expect_stderr=True,
        )
        self.assertTrue(stderr.startswith(f"usage: {tool_name} [-h]"))

    def test_set_and_replace_command_args(self):
        tool_name = "foo-tool-name"

        for command in ["set", "replace"]:
            args, _, _ = self.assert_parser_output(
                [
                    tool_name,
                    command,
                    "--record",
                    "mac",
                    "roman",
                    "en",
                    "1",
                    "Foo",
                    "--output",
                    "out.ttf",
                    "in.ttf",
                ]
            )
            self.assertEqual(args.command, command)
            self.assertEqual(args.record, [["mac", "roman", "en", "1", "Foo"]])
            self.assertEqual(args.output, "out.ttf")
            self.assertEqual(args.font_file, "in.ttf")

            # no options
            _, _, stderr = self.assert_parser_output(
                f"{tool_name} {command} in.ttf".split(" "),
                expect_exit=True,
                expected_exit_code=2,
                expect_stderr=True,
            )
            self.assertIn(
                "one of the arguments --record --json-input-string "
                + "--json-input-file is required",
                stderr,
            )

            # needs --output or --in-place
            _, _, stderr = self.assert_parser_output(
                f"{tool_name} {command} --record mac roman en 1 Foo in.ttf".split(" "),
                expect_exit=True,
                expected_exit_code=2,
                expect_stderr=True,
            )
            self.assertIn(
                "must set [--output OUTPUT | --in-place] or --dry-run", stderr
            )

            # both --quiet and --dry-run not allowed
            _, _, stderr = self.assert_parser_output(
                [
                    tool_name,
                    command,
                    "--record",
                    "mac",
                    "roman",
                    "en",
                    "1",
                    "Foo",
                    "--quiet",
                    "--dry-run",
                    "in.ttf",
                ],
                expect_exit=True,
                expected_exit_code=2,
                expect_stderr=True,
            )
            self.assertIn("not allowed with argument", stderr)

            # both --in-place and --output not allowed
            _, _, stderr = self.assert_parser_output(
                [
                    tool_name,
                    command,
                    "--record",
                    "mac",
                    "roman",
                    "en",
                    "1",
                    "Foo",
                    "--output",
                    "out.ttf",
                    "--in-place",
                    "in.ttf",
                ],
                expect_exit=True,
                expected_exit_code=2,
                expect_stderr=True,
            )
            self.assertIn("not allowed with argument", stderr)

    def test_remove_args(self):
        tool_name = "foo-tool-name"

        args, _, _ = self.assert_parser_output(
            f"{tool_name} remove --name-id 1 --output out.ttf in.ttf".split(" "),
        )
        self.assertEqual(args.command, "remove")
        self.assertEqual(args.output, "out.ttf")
        self.assertEqual(args.font_file, "in.ttf")

        # no filters
        args, _, stderr = self.assert_parser_output(
            f"{tool_name} remove --output out.ttf in.ttf".split(" "),
            expect_exit=True,
            expected_exit_code=2,
            expect_stderr=True,
        )
        self.assertIn(
            "at least one of the arguments --platform-id --encoding-id "
            + "--language-id --name-id is required",
            stderr,
        )


class TestFormatting(unittest.TestCase):
    @staticmethod
    def make_test_records():
        record1 = NameRecord()
        record1.platformID = 1
        record1.platEncID = 0
        record1.langID = 0
        record1.nameID = 1
        record1.string = "test font"

        record2 = NameRecord()
        record2.platformID = 3
        record2.platEncID = 1
        record2.langID = 0x0409
        record2.nameID = 0
        record2.string = "test copyright"

        return [record1, record2]

    def test_records_to_rows(self):
        """Test conversion of records to row format"""
        rows = records_to_rows(TestFormatting.make_test_records())
        self.assertEqual(len(rows), 2)
        self.assertEqual(len(rows[0]), 5)
        self.assertEqual(len(rows[1]), 5)
        self.assertEqual(rows[0][0], "1 (macintosh)")
        self.assertEqual(rows[0][1], "0 (roman)")
        self.assertEqual(rows[0][2], "0 (en)")
        self.assertEqual(rows[0][3], "1 (font_family_name)")
        self.assertEqual(rows[0][4], "test font")
        self.assertEqual(rows[1][0], "3 (windows)")
        self.assertEqual(rows[1][1], "1 (unicode_bmp)")
        self.assertEqual(rows[1][2], "1033 (en)")
        self.assertEqual(rows[1][3], "0 (copyright_notice)")
        self.assertEqual(rows[1][4], "test copyright")

    def test_format_table_rows(self):
        rows = [
            ["foo", "bar", "baz"],
            [
                "aaaaaaaaaaa",
                "bb",
                "ccc",
            ],
        ]

        result = format_table_rows(rows)
        self.assertIsInstance(result, str)
        lines = result.split("\n")
        self.assertEqual(len(lines), 2)
        self.assertEqual(lines[0], "foo          bar  baz")
        self.assertEqual(lines[1], "aaaaaaaaaaa  bb   ccc")

        result2 = format_table_rows(rows, padding=3)
        self.assertIsInstance(result2, str)
        lines2 = result2.split("\n")
        self.assertEqual(len(lines2), 2)
        self.assertEqual(lines2[0], "foo           bar   baz")
        self.assertEqual(lines2[1], "aaaaaaaaaaa   bb    ccc")

    def test_format_text_output(self):
        result = format_text_output(TestFormatting.make_test_records())
        lines = result.split("\n")
        self.assertTrue(lines[0].startswith("PLATFORM"))
        self.assertTrue(lines[1].startswith("1"))

    def test_format_json_output(self):
        """Test JSON formatting of records"""
        result = format_json_output(TestFormatting.make_test_records())
        parsed = json.loads(result)
        self.assertIsInstance(parsed, list)
        self.assertEqual(len(parsed), 2)
        self.assertEqual(parsed[0][4], "test font")
        self.assertEqual(parsed[1][0], 3)
        self.assertEqual(parsed[1][1], 1)
        self.assertEqual(parsed[1][2], 1033)
        self.assertEqual(parsed[1][3], 0)
        self.assertEqual(parsed[1][4], "test copyright")


# Test as part of TestMain instead
class TestGetInputData(unittest.TestCase):
    pass


class TestHandleException(unittest.TestCase):
    # not a dict, but we only use the get method
    @patch.dict(os.environ, {"FONT_NAME_TOOL_DEBUG": "1"})
    def test_handle_exception_raises_in_debug(self):
        with self.assertRaises(FileExistsError):
            handle_exception(FileExistsError())
        with self.assertRaises(FileExistsError):
            try:
                raise FileExistsError()
            except BaseException as exception:
                handle_exception(exception)

    def test_handle_exception_raises_system_exit(self):
        with self.assertRaises(SystemExit):
            handle_exception(SystemExit())
        with self.assertRaises(SystemExit):
            try:
                sys.exit(0)
            except BaseException as exception:
                handle_exception(exception)

    @patch("builtins.print")
    @patch("sys.exit")
    def test_handle_exception_prints_error_and_exits_with_filename(
        self, mock_exit, mock_print
    ):
        exception = FileExistsError()
        exception.filename = "test.ttf"
        handle_exception(exception)
        print_args = mock_print.call_args[0]
        self.assertEqual(print_args[0], "[FileExistsError] None: 'test.ttf'")
        mock_exit.assert_called_once_with(1)

    @patch("builtins.print")
    @patch("sys.exit")
    def test_handle_exception_prints_error_and_exits_without_filename(
        self, mock_exit, mock_print
    ):
        handle_exception(FileExistsError())
        print_args = mock_print.call_args[0]
        self.assertEqual(print_args[0], "[FileExistsError]")
        mock_exit.assert_called_once_with(1)

    @patch("builtins.print")
    @patch("sys.exit")
    def test_handle_exception_prints_error_and_exits_non_oserror(
        self, mock_exit, mock_print
    ):
        handle_exception(KeyError("foo"))
        print_args = mock_print.call_args[0]
        self.assertEqual(print_args[0], "[KeyError] 'foo'")
        mock_exit.assert_called_once_with(1)


class TestIntegration(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.temp_dir = tempfile.mkdtemp()

    @classmethod
    def tearDownClass(cls):
        for file in Path(cls.temp_dir).glob("*"):
            file.unlink()
        os.rmdir(cls.temp_dir)

    def setUp(self):
        self.font_path = Path(self.temp_dir) / "test.ttf"
        self.font = TTFont()
        self.font.save(self.font_path)

    def tearDown(self):
        if self.font_path.exists():
            self.font_path.unlink()

    def run_tool(self, *args):
        result = subprocess.run(
            ["poetry", "run", "python", "-m", "font_name_tool.cli"] + list(args),
            capture_output=True,
            text=True,
        )
        return result

    def test_print_empty_font(self):
        result = self.run_tool("print", self.font_path)
        self.assertEqual(result.returncode, 0)
        self.assertEqual("PLATFORM  ENCODING  LANGUAGE  NAME  STRING\n", result.stdout)

    def test_print_json_format(self):
        result = self.run_tool("print", self.font_path, "--print-json")
        self.assertEqual(result.returncode, 0)
        records = json.loads(result.stdout)
        self.assertIsInstance(records, list)

    def test_set_single_record(self):
        result = self.run_tool(
            "set",
            self.font_path,
            "--in-place",
            "--record",
            "windows",
            "unicode_bmp",
            "en",
            "font_family",
            "Test Font",
        )
        self.assertEqual(result.returncode, 0)

        result = self.run_tool("print", self.font_path, "--print-json")
        records = json.loads(result.stdout)
        self.assertEqual(len(records), 1)
        self.assertEqual(records[0][4], "Test Font")

    def test_replace_multiple_records(self):
        records = [[3, 1, 0x409, 1, "Test Font"], [3, 1, 0x409, 2, "Regular"]]
        json_file = Path(self.temp_dir) / "test_records.json"
        with open(json_file, "w") as f:
            json.dump(records, f)

        result = self.run_tool(
            "replace", self.font_path, "--in-place", "--json-input-file", json_file
        )
        self.assertEqual("", result.stderr)
        self.assertEqual(result.returncode, 0)

        result = self.run_tool("print", self.font_path, "--print-json")
        saved_records = json.loads(result.stdout)
        self.assertEqual(len(saved_records), 2)

        os.remove(json_file)

    def test_json_input_string(self):
        records = [[3, 1, 0x409, 1, "Test Font"], [3, 1, 0x409, 2, "Regular"]]
        json_string = json.dumps(records)
        result = self.run_tool(
            "replace",
            self.font_path,
            "--print-json",
            "--in-place",
            "--json-input-string",
            json_string,
        )
        self.assertEqual(
            records, json.loads(result.stdout)
        )  # might as well test --print-json too
        self.assertEqual("", result.stderr)
        self.assertEqual(result.returncode, 0)
        result = self.run_tool("print", self.font_path, "--print-json")
        saved_records = json.loads(result.stdout)
        self.assertEqual(len(saved_records), 2)

    def test_remove_filtered_records(self):
        self.run_tool(
            "replace",
            self.font_path,
            "--in-place",
            "--record",
            "windows",
            "unicode_bmp",
            "en",
            "font_family",
            "Test Font",
            "--record",
            "macintosh",
            "roman",
            "en",
            "font_family",
            "Test Font Mac",
        )

        result = self.run_tool(
            "remove", self.font_path, "--in-place", "--platform-id", "3"
        )
        self.assertEqual(result.returncode, 0)

        result = self.run_tool("print", self.font_path, "--print-json")
        records = json.loads(result.stdout)
        self.assertEqual(len(records), 1)
        self.assertEqual(records[0][0], 1)  # macintosh platform ID

    def test_invalid_platform(self):
        result = self.run_tool(
            "set",
            self.font_path,
            "--in-place",
            "--record",
            "invalid",
            "unicode_bmp",
            "en",
            "font_family",
            "Test Font",
        )
        self.assertEqual(result.returncode, 1)
        self.assertIn("Invalid platform code", result.stderr)

    def test_dry_run(self):
        original_size = os.path.getsize(self.font_path)

        result = self.run_tool(
            "set",
            self.font_path,
            "--dry-run",
            "--record",
            "windows",
            "unicode_bmp",
            "en",
            "font_family",
            "Test Font",
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(os.path.getsize(self.font_path), original_size)
        self.assertIn("DRY RUN", result.stdout)

    def test_output_file(self):
        output_font = "output.ttf"

        result = self.run_tool(
            "set",
            self.font_path,
            "--output",
            output_font,
            "--record",
            "windows",
            "unicode_bmp",
            "en",
            "font_family",
            "Test Font",
        )
        self.assertEqual(result.returncode, 0)
        self.assertTrue(os.path.exists(output_font))

        os.remove(output_font)


if __name__ == "__main__":
    unittest.main()
