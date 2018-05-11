tests
testJsonString
  "Allow double quotes for backwards JSON compatibility"

  | string |
  self assert: (self materialize: '"foo"') = 'foo'.
  self assert: (self materialize: '"FOO"') = 'FOO'.
  self
    assert: (self materialize: '''Ã©lÃ¨ve en FranÃ§ais''') = 'élève en Français'.
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
  self assert: (self materialize: '"\"\''\\\t\r\n\f\b"') = string