"
a URL type that can't be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.
"
Class {
	#name : 'CypGenericUrl',
	#superclass : 'CypUrl',
	#instVars : [
		'schemeName',
		'locator'
	],
	#category : 'Cypress-Core'
}

{ #category : 'parsing' }
CypGenericUrl class >> absoluteFromText: aString [
  | schemeName locator |
  schemeName := CypUrl schemeNameForString: aString.
  schemeName ifNil: [ ^ self schemeName: 'xnoscheme' locator: aString ].
  locator := aString copyFrom: schemeName size + 2 to: aString size.
  ^ self schemeName: schemeName locator: locator
]

{ #category : 'instance creation' }
CypGenericUrl class >> schemeName: schemeName  locator: locator [
	^self new schemeName: schemeName  locator: locator

]

{ #category : 'access' }
CypGenericUrl >> locator [
	^locator

]

{ #category : 'printing' }
CypGenericUrl >> printOn: aStream [
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].

]

{ #category : 'parsing' }
CypGenericUrl >> privateInitializeFromText: aString [
  schemeName := CypUrl schemeNameForString: aString.
  locator := schemeName
    ifNil: [ aString ]
    ifNotNil: [ aString copyFrom: schemeName size + 2 to: aString size ]
]

{ #category : 'parsing' }
CypGenericUrl >> privateInitializeFromText: aString relativeTo: aUrl [
	schemeName := aUrl schemeName.
	locator := aString.

]

{ #category : 'classification' }
CypGenericUrl >> scheme [
	^ self schemeName.

]

{ #category : 'access' }
CypGenericUrl >> schemeName [
	^schemeName

]

{ #category : 'private' }
CypGenericUrl >> schemeName: schemeName0  locator: locator0 [
	schemeName := schemeName0.
	locator := locator0.

]
