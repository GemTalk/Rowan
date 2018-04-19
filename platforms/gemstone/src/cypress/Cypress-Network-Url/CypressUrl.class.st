"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

A Uniform Resource Locator.  It specifies the location of a document on the Internet.  The base class is abstract; child classes break different types of URLs down in ways appropriate for that type.
"
Class {
	#name : 'CypressUrl',
	#superclass : 'Object',
	#instVars : [
		'fragment'
	],
	#category : 'Cypress-Network-Url'
}

{ #category : 'instance creation' }
CypressUrl class >> absoluteFromFileNameOrUrlString: aString [
	"Return a URL from and handle Strings without schemes
	as local relative FileUrls instead of defaulting to a HttpUrl
	as absoluteFromText: does."

	^(CypressUrl schemeNameForString: aString)
		ifNil: [CypressFileUrl workingDirectory newFromRelativeText: aString]
		ifNotNil: [CypressUrl absoluteFromText: aString]

]

{ #category : 'instance creation' }
CypressUrl class >> absoluteFromText: aString [
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	"CypressUrl absoluteFromText: 'http://chaos.resnet.gatech.edu:8000/docs/java/index.html?A%20query%20#part'" 
	"CypressUrl absoluteFromText: 'msw://chaos.resnet.gatech.edu:9000/testbook?top'"
	"CypressUrl absoluteFromText: 'telnet:chaos.resnet.gatech.edu'"
	"CypressUrl absoluteFromText: 'file:/etc/passwd'"

	| remainder index scheme fragment newUrl |
	"trim surrounding whitespace"
	remainder := aString trimSeparators.

	"extract the fragment, if any"
	index := remainder indexOf: $#.
	index > 0 ifTrue: [
		fragment := remainder copyFrom: index + 1 to: remainder size.
		remainder := remainder copyFrom: 1 to: index - 1].

	"choose class based on the scheme name, and let that class do the bulk of the parsing"
	scheme := self schemeNameForString: remainder.
	newUrl := (self urlClassForScheme: scheme) new privateInitializeFromText: remainder.
	newUrl privateFragment: fragment.
	^newUrl

]

{ #category : 'parsing' }
CypressUrl class >> combine: baseURL withRelative: relURL [ 
	"Take two URL as string form, combine them and return the corresponding URL in string form"

	^((self absoluteFromText: baseURL) newFromRelativeText: relURL) asString

]

{ #category : 'instance creation' }
CypressUrl class >> for: aString [
	"Return a URL from a string and handle
	a String without a scheme as a HttpUrl."

	^self absoluteFromText: aString

]

{ #category : 'encoding' }
CypressUrl class >> isCharacterSafeForHttp: aChar [
	"Answer whether a character is 'safe', or needs to be escaped when used, eg, in a URL."

	^aChar codePoint < 128
		and: [aChar isAlphaNumeric or: ['.-_' includes: aChar]]

]

{ #category : 'constants' }
CypressUrl class >> schemeName [
	"When searching for a class to handle a particular scheme, make sure that Url classes never match by default. This is so that abstract Url classes e.g. HierarchicalUrl can be iterated over, but will not be selected"

	^ nil.

]

{ #category : 'parsing' }
CypressUrl class >> schemeNameForString: aString [
	"Get the scheme name from a string, or return nil if it's not specified. 
	Used in internal parsing routines - an outsider may as well use asUrl. 
	Return scheme in lowercases."
	
	"Url schemeNameForString: 'http://www.yahoo.com'"
	"Url schemeNameForString: '/etc/passwed'"
	"Url schemeNameForString: '/etc/testing:1.2.3'"

	| index schemeName |
	index := aString indexOf: $: ifAbsent: [^ nil].
	schemeName := aString copyFrom: 1 to: index - 1.
	(schemeName allSatisfy: [:each | each isLetter]) ifFalse: [^ nil].
	^ schemeName asLowercase

]

{ #category : 'parsing' }
CypressUrl class >> urlClassForScheme: scheme [

	| allSubclasses |
	allSubclasses := self userId = System myUserProfile userId
		ifTrue: [ ClassOrganizer new allSubclassesOf: self ]
		ifFalse: [ (ClassOrganizer newWithRoot: self forUserId: self userId) allSubclassesOf: self ].
	^allSubclasses detect: [:urlClass | urlClass schemeName = scheme]
		ifNone: [CypressGenericUrl]

]

{ #category : 'encoding' }
CypressUrl class >> writeWithHttpEscapes: aCollection on: aStream [
	"Write the given string or Utf8 on the stream with 'dangerous' characters 
	escaped to their %XX form, for use in HTTP transactions.
	Note that Utf8s containing code points over 128 will not work properly here."

	aCollection do: 
			[:each |
			| char |
			char := each asCharacter.
			(self isCharacterSafeForHttp: char)
				ifTrue: [aStream nextPut: char]
				ifFalse: 
					[| int |
					aStream nextPut: $%.
					int := each asInteger.
					int // 16 printOn: aStream base: 16 showRadix: false.
					int \\ 16 printOn: aStream base: 16 showRadix: false]]

]

{ #category : 'downloading' }
CypressUrl >> activate [
	"spawn an external handler for this URL"
	

]

{ #category : 'converting' }
CypressUrl >> asString [

	^self printString

]

{ #category : 'converting' }
CypressUrl >> asURI [
	^self asString asURI

]

{ #category : 'converting' }
CypressUrl >> asUrl [
	^self

]

{ #category : 'converting' }
CypressUrl >> asUrlRelativeTo: aUrl [
	^self

]

{ #category : 'accessing' }
CypressUrl >> authority [
	^''

]

{ #category : 'encoding' }
CypressUrl >> decodeHttpEscapesOf: aString [
	"decode string including %XX form
	 (adapted from Pharo 2.0)"

	| unescaped pos sourceSize |
	unescaped := ReadWriteStreamPortable on: String new.
	pos := 1.
	sourceSize := aString size.
	[pos > sourceSize] whileFalse: 
			[| char |
			char := aString at: pos.
			(char = $% and: [pos + 2 <= sourceSize])
				ifTrue: 
					[| asciiVal |
					asciiVal := ((aString at: pos + 1) asUppercase digitValueInRadix: 16) * 16
								+ ((aString at: pos + 2) asUppercase digitValueInRadix: 16).
					asciiVal > 255 ifTrue: [^aString].
					unescaped nextPut: (Character withValue: asciiVal).
					pos := pos + 3]
				ifFalse: 
					[char = $+
						ifTrue: [unescaped nextPut: Character space]
						ifFalse: [unescaped nextPut: char].
					pos := pos + 1]].
	^unescaped contents

]

{ #category : 'converting' }
CypressUrl >> downloadUrl [
	^self asString

]

{ #category : 'fragment' }
CypressUrl >> fragment [
	^fragment

]

{ #category : 'downloading' }
CypressUrl >> hasContents [
	"whether this URL can download contents to be displayed; if not, it fundamentally requires an outside application to deal with it.  For example, mailto: and telnet: urls"
	^false

]

{ #category : 'parsing' }
CypressUrl >> newFromRelativeText: aString [
	"return a URL relative to the current one, given by aString.  For instance, if self is 'http://host/dir/file', and aString is '/dir2/file2', then the return will be a Url for 'http://host/dir2/file2'"

	"if the scheme is the same, or not specified, then use the same class"

	| newSchemeName remainder fragmentStart newFragment newUrl bare |

	bare := aString trimSeparators.
	newSchemeName := CypressUrl schemeNameForString: bare.
	(newSchemeName notNil and: [ newSchemeName ~= self schemeName ]) ifTrue: [
		"different scheme -- start from scratch"
		^CypressUrl absoluteFromText: aString ].

	remainder := bare.

	"remove the fragment, if any"
	fragmentStart := remainder indexOf: $#.
	fragmentStart > 0 ifTrue: [
		newFragment := remainder copyFrom: fragmentStart+1 to: remainder size. 
		remainder := remainder copyFrom: 1 to: fragmentStart-1].

	"remove the scheme name"
	newSchemeName ifNotNil: [
		remainder := remainder copyFrom: (newSchemeName size + 2) to: remainder size ].

	"create and initialize the new url"
	newUrl := self class new privateInitializeFromText: remainder  relativeTo: self.


	"set the fragment"
	newUrl privateFragment: newFragment.


	^newUrl

]

{ #category : 'printing' }
CypressUrl >> printOn: aStream [

	^self subclassResponsibility: #printOn:

]

{ #category : 'fragment' }
CypressUrl >> privateFragment: aString [
	fragment := aString

]

{ #category : 'parsing' }
CypressUrl >> privateInitializeFromText: aString [

	^self subclassResponsibility: #privateInitializeFromText:

]

{ #category : 'parsing' }
CypressUrl >> privateInitializeFromText: aString relativeTo: aUrl [
	"initialize from the given string, as a relative URL.  aString will have had the scheme name removed, if it was present to begin with.  If it was, then the scheme name was the same as the receiver's scheme name"

	"by default, just do regular initialization"
	^self privateInitializeFromText: aString

]

{ #category : 'classification' }
CypressUrl >> scheme [
	"return a string with the scheme of this URL.  For instance, HTTP"

	^self subclassResponsibility: #scheme

]

{ #category : 'classification' }
CypressUrl >> schemeName [
	"return a lowercase string with the scheme of this URL.  For instance, 'http'"

	^self subclassResponsibility: #schemeName

]

{ #category : 'fragment' }
CypressUrl >> withFragment: newFragment [
	"return a URL which is the same except that it has a different fragment"
	^self copy privateFragment: newFragment; yourself

]

{ #category : 'fragment' }
CypressUrl >> withoutFragment [
	"return a URL which is identical to the receiver except that it has no fragment associated with it"
	^self withFragment: nil

]

{ #category : 'encoding' }
CypressUrl >> writeWithHttpEscapes: aCollection on: aStream [

	self class writeWithHttpEscapes: aCollection on: aStream

]
