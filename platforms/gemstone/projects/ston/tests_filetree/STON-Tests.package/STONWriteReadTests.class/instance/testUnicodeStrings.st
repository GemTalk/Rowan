tests
testUnicodeStrings
  | strings |
  strings := {(String
    withAll:
      {(Character codePoint: 0).
      (Character codePoint: 255).
      (Character codePoint: 256)}).
  (self unicode16TestString).
  (self unicode32TestString).
  'élève en Français'}.
  self serializeAndMaterialize: strings