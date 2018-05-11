tests
testString
  | string x |
  self assert: (self materialize: '''foo''') = 'foo'.
  self assert: (self materialize: '''FOO''') = 'FOO'.
  self
    assert:
      (self materialize: '''\u00C3\u00A9l\u00C3\u00A8ve en Fran\u00C3\u00A7ais''')
        = 'élève en Français'.
  self
    assert:
      (x := self
        materialize:
          '''\u00D0\u00AF \u00D0\u00BC\u00D0\u00BE\u00D0\u00B6\u00D1\u0083 \u00D1\u0097\u00D1\u0081\u00D1\u0082\u00D0\u00B8 \u00D1\u0081\u00D0\u00BA\u00D0\u00BB\u00D0\u00BE, \u00D1\u0096 \u00D0\u00B2\u00D0\u00BE\u00D0\u00BD\u00D0\u00BE \u00D0\u00BC\u00D0\u00B5\u00D0\u00BD\u00D1\u0096 \u00D0\u00BD\u00D0\u00B5 \u00D0\u00B7\u00D0\u00B0\u00D1\u0088\u00D0\u00BA\u00D0\u00BE\u00D0\u00B4\u00D0\u00B8\u00D1\u0082\u00D1\u008C.''')
        = self unicode16TestString.
  self
    assert:
      (x := self materialize: '''\u00F0\u00A3\u008E\u008F''')
        = self unicode32TestString.
  string := String
    withAll:
      {$".
      $'.
      $\.
      (Character tab).
      (Character cr).
      (Character lf).
      (Character newPage).
      (Character backspace)}.
  self assert: (self materialize: '''\"\''\\\t\r\n\f\b''') = string