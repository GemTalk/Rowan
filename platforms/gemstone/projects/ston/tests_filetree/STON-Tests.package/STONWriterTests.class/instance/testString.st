tests
testString
  | x string |
  self assert: (self serialize: 'foo') = '''foo'''.
  self assert: (self serialize: 'FOO') = '''FOO'''.
  self
    assert:
      (x := self serialize: 'élève en Français')
        = '''\u00E9l\u00E8ve en Fran\u00E7ais'''.
  self
    assert:
      (x := self serialize: self unicode16TestString)
        =
          '''\u042F \u043C\u043E\u0436\u0443 \u0457\u0441\u0442\u0438 \u0441\u043A\u043B\u043E, \u0456 \u0432\u043E\u043D\u043E \u043C\u0435\u043D\u0456 \u043D\u0435 \u0437\u0430\u0448\u043A\u043E\u0434\u0438\u0442\u044C.'''.
  false
    ifTrue: [ 
      "ambiguous encoding for 32-bit Unicode characters: https://github.com/svenvc/ston/issues/11"
      self
        assert: (x := self serialize: self unicode32TestString) = '''\u2338F''' ].
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