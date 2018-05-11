tests
testDoubleQuotedString
  | string x |
  self assert: (self serializeJson: 'foo') = '"foo"'.
  self assert: (self serializeJson: 'FOO') = '"FOO"'.
  self
    assert:
      (x := self serializeJson: 'élève en Français') = '"Ã©lÃ¨ve en FranÃ§ais"'.
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
  self assert: (self serializeJson: string) = '"\"\''\\\t\r\n\f\b"'