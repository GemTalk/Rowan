"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: parsing
"
privateInitializeFromText: aString

	| remainder ind specifiedSchemeName |
	remainder := aString.
	schemeName
		ifNil: 
			[specifiedSchemeName := CypressUrl schemeNameForString: remainder.
			specifiedSchemeName
				ifNotNil: 
					[schemeName := specifiedSchemeName.
					remainder := remainder copyFrom: schemeName size + 2 to: remainder size].
			schemeName
				ifNil: 
					["assume HTTP"
					schemeName := 'http']].

	"remove leading // if it's there"
	(remainder indexOfSubCollection: '//') = 1
		ifTrue: [remainder := remainder copyFrom: 3 to: remainder size].

	"get the query"
	ind := remainder indexOf: $?.
	ind > 0
		ifTrue: 
			[query := remainder copyFrom: ind + 1 to: remainder size.
			remainder := remainder copyFrom: 1 to: ind - 1].

	"get the authority"
	ind := remainder indexOf: $/.
	ind > 0
		ifTrue: 
			[ind = 1
				ifTrue: [authority := '']
				ifFalse: 
					[authority := remainder copyFrom: 1 to: ind - 1.
					remainder := remainder copyFrom: ind + 1 to: remainder size]]
		ifFalse: 
			[authority := remainder.
			remainder := ''].

	"extract the username+password"
	ind := authority indexOf: $@.
	ind > 0
		ifTrue: 
			[username := authority copyFrom: 1 to: ind - 1.
			authority := authority copyFrom: ind + 1 to: authority size.
			ind := username indexOf: $:.
			ind > 0
				ifTrue: 
					[password := (self
								decodeHttpEscapesOf: (username copyFrom: ind + 1 to: username size))
									asByteArray decodeFromUTF8
								asString.
					username := username copyFrom: 1 to: ind - 1]
				ifFalse: [password := nil].
			username := (self decodeHttpEscapesOf: username) asByteArray
						decodeFromUTF8 asString].

	"Extract the port"
	(authority includes: $:)
		ifTrue: 
			[| lastColonIndex portString |
			lastColonIndex := authority findLast: [:c | c = $:].
			portString := authority copyFrom: lastColonIndex + 1 to: authority size.
			(portString allSatisfy: [:each | each isDigit])
				ifTrue: 
					[port := Integer fromString: portString.
					port > 65535 ifTrue: [self error: 'Invalid port number'].
					authority := authority copyFrom: 1 to: lastColonIndex - 1]
				ifFalse: [self error: 'Invalid port number']].

	"get the path"
	path := self privateParsePath: remainder relativeTo: #()
