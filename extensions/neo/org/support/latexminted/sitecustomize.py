"""Register repo-local Pygments extensions for notebook PDF builds."""

from pygments.lexers import LEXERS


LEXERS.setdefault(
    "MlodyLexer",
    (
        "neo_org_mlody_lexer",
        "Mlody",
        ("mlody",),
        ("*.mlody",),
        ("text/x-mlody",),
    ),
)
