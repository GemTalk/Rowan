private-initialization
privateInitializeFromText: aString
  "Calculate host and path from a file URL in String format.
	Some malformed formats are allowed and interpreted by guessing."

  | schemeName pathString bare |
  host := ''.
  bare := aString trimSeparators.
  schemeName := RwUrl schemeNameForString: bare.
  (schemeName == nil or: [ schemeName ~= self schemeName ])
    ifTrue: [ pathString := bare ]
    ifFalse: [ 
      "First remove schemeName and colon"
      bare := bare copyFrom: schemeName size + 2 to: bare size.
      (bare beginsWith: '//')
        ifTrue: [ pathString := bare copyFrom: 3 to: bare size ]
        ifFalse: [ pathString := bare ] ].
  self initializeFromPathString: pathString