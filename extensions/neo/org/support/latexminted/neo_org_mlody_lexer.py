"""Pygments lexer for Haskell-style Mlody sources."""

from __future__ import annotations

import re

from pygments.lexer import RegexLexer, bygroups, words
from pygments.token import Comment, Keyword, Name, Number, Operator, Punctuation, String, Text

__all__ = ["MlodyLexer"]


class MlodyLexer(RegexLexer):
    """Highlight Mlody sources with categories that mirror `neo-mlody-mode`."""

    name = "Mlody"
    aliases = ["mlody"]
    filenames = ["*.mlody"]
    mimetypes = ["text/x-mlody"]
    flags = re.MULTILINE | re.UNICODE

    _TOP_LEVEL_KEYWORDS = ("from", "type", "value", "task", "layout", "location", "config")
    _SECTION_KEYWORDS = ("inputs", "outputs", "config")
    _PROPERTY_KEYWORDS = (
        "where",
        "abstract",
        "base",
        "source",
        "type",
        "representation",
        "layout",
        "location",
        "freshness",
        "choices",
        "canonical",
        "validate",
        "predicate",
        "min",
        "max",
        "default",
        "description",
        "attrs",
        "methods",
        "injectable",
        "duration",
        "schedule",
        "unit",
        "group",
        "constraint",
    )
    _BUILTIN_CONSTANTS = ("true", "false")

    tokens = {
        "root": [
            (r"^\s*--.*?$", Comment.Single),
            (
                r"^(\s*)(from)(\s+)(//[^\s\"]+)(\s+)(import)\b",
                bygroups(Text, Keyword, Text, Name.Constant, Text, Keyword),
            ),
            (
                r"^(\s*)(type|value|layout|location)(\s+)([A-Za-z0-9_-]+)\b",
                bygroups(Text, Keyword, Text, Name.Class),
            ),
            (
                r"^(\s*)(task)(\s+)([A-Za-z0-9_-]+)\b",
                bygroups(Text, Keyword, Text, Name.Function),
            ),
            (
                r"^(\s*)(config)(\s+)([A-Za-z0-9_-]+)\b",
                bygroups(Text, Keyword, Text, Name.Variable),
            ),
            (
                words(_SECTION_KEYWORDS, prefix=r"^(\s*)(", suffix=r")\b"),
                bygroups(Text, Keyword),
            ),
            (
                words(_TOP_LEVEL_KEYWORDS, prefix=r"^(\s*)(", suffix=r")\b"),
                bygroups(Text, Keyword),
            ),
            (
                r"^(\s*)([A-Za-z0-9_./-]+)(\s*)(::|:|=)",
                bygroups(Text, Name.Variable, Text, Operator),
            ),
            (r"--.*?$", Comment.Single),
            (r"//[^\s\")\]}]+", Name.Constant),
            (r":[A-Za-z0-9_/-]+(?:\.[A-Za-z0-9_/-]+)*", Name.Constant),
            (r'"(?:\\.|[^"\\])*"', String.Double),
            (r"'(?:\\.|[^'\\])*'", String.Single),
            (words(_PROPERTY_KEYWORDS, prefix=r"\b", suffix=r"\b"), Keyword),
            (r"\b[A-Za-z0-9_-]+:[A-Za-z0-9_/-]+\b", Name.Builtin),
            (words(_BUILTIN_CONSTANTS, prefix=r"\b", suffix=r"\b"), Keyword.Constant),
            (r"\b[0-9]+(?:\.[0-9]+)?\b", Number),
            (r"::|->|<-|=>|=|:|\|", Operator),
            (r"[\(\)\[\]\{\},\.]", Punctuation),
            (r"[ \t]+", Text),
            (r"\n", Text),
            (r"[A-Za-z_][A-Za-z0-9_'-]*", Text),
            (r".", Text),
        ]
    }
