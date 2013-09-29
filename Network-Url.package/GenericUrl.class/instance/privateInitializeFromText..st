"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing
"
privateInitializeFromText: aString
	schemeName := Url schemeNameForString: aString.
	locator :=  schemeName 
		ifNil: [ aString ]
		ifNotNil: [ aString copyFrom: (schemeName size+2) to: aString size ].