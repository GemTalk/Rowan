tests
testString
  | x string |
  self assert: (self serialize: 'foo') = '''foo'''.
  self assert: (self serialize: 'FOO') = '''FOO'''.
  self
    assert:
      (x := self serialize: 'élève en Français') = '''Ã©lÃ¨ve en FranÃ§ais'''.
  self
    assert:
      (x := self serialize: self unicode16TestString)
        =
          '''Ð¯ Ð¼Ð¾Ð¶Ñ ÑÑÑÐ¸ ÑÐºÐ»Ð¾, Ñ Ð²Ð¾Ð½Ð¾ Ð¼ÐµÐ½Ñ Ð½Ðµ Ð·Ð°ÑÐºÐ¾Ð´Ð¸ÑÑ.'''.
  self assert: (x := self serialize: self unicode32TestString) = '''ð£'''.
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
  self assert: (self serialize: string) = '''\"\''\\\t\r\n\f\b'''