instance creation
fromString: aString
  "Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

  "RwUrl fromString: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'"

  "RwUrl fromString: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"

  "RwUrl fromString: 'telnet:chaos.resnet.gatech.edu'"

  "RwUrl fromString: 'file:/etc/passwd'"

  | remainder index scheme fragment newUrl |
  remainder := aString trimSeparators.
  index := remainder indexOf: $#.
  index > 0
    ifTrue: [ 
      "extract the fragment, if any"
      fragment := remainder copyFrom: index + 1 to: remainder size.
      remainder := remainder copyFrom: 1 to: index - 1 ].
  scheme := self schemeNameForString: remainder.
  (scheme = 'http' or: [ scheme = 'https' ])
    ifTrue: [ ^ self httpFromString: aString ].
  newUrl := (self urlClassForScheme: scheme) new
    privateInitializeFromText: remainder.
  newUrl privateFragment: fragment.
  ^ newUrl