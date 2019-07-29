"
All Cypress classes are private to GemStone and are likely to be removed in a future release.

a URL type that can't be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.
"
Class {
	#name : 'CypressGenericUrl',
	#superclass : 'CypressUrl',
	#instVars : [
		'schemeName',
		'locator'
	],
	#category : 'Cypress-Network-Url'
}

{ #category : 'parsing' }
CypressGenericUrl class >> absoluteFromText: aString [

	| schemeName locator |
	schemeName := CypressUrl schemeNameForString: aString.
	schemeName ifNil: [^self schemeName: 'xnoscheme' locator: aString].
	locator := aString copyFrom: schemeName size + 2 to: aString size.
	^self schemeName: schemeName locator: locator

]

{ #category : 'instance creation' }
CypressGenericUrl class >> schemeName: schemeName  locator: locator [
	^self new schemeName: schemeName  locator: locator

]

{ #category : 'access' }
CypressGenericUrl >> locator [
	^locator

]

{ #category : 'printing' }
CypressGenericUrl >> printOn: aStream [
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].

]

{ #category : 'parsing' }
CypressGenericUrl >> privateInitializeFromText: aString [

	schemeName := CypressUrl schemeNameForString: aString.
	locator := schemeName
				ifNil: [aString]
				ifNotNil: [aString copyFrom: schemeName size + 2 to: aString size]

]

{ #category : 'parsing' }
CypressGenericUrl >> privateInitializeFromText: aString relativeTo: aUrl [
	schemeName := aUrl schemeName.
	locator := aString.

]

{ #category : 'classification' }
CypressGenericUrl >> scheme [
	^ self schemeName.

]

{ #category : 'access' }
CypressGenericUrl >> schemeName [
	^schemeName

]

{ #category : 'private' }
CypressGenericUrl >> schemeName: schemeName0  locator: locator0 [
	schemeName := schemeName0.
	locator := locator0.

]
