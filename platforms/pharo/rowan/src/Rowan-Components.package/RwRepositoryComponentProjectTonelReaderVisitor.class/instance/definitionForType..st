tonel parser
definitionForType: aString
  aString = TonelWriter classLabel
    ifTrue: [ ^ #class ].
  aString = TonelWriter extensionLabel
    ifTrue: [ ^ #classExtension ].
  TonelParseError signal: 'Unknown type declaration.'