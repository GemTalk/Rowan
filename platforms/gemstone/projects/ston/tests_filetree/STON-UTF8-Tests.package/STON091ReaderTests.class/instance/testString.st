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
          '''Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.''')
        = self unicode16TestString.
  self assert: (x := self materialize: '''ð£''') = self unicode32TestString.
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