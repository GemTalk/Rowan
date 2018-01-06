"
a URL type that can't be broken down in any systematic way.  For example, mailto: and telnet: URLs.  The part after the scheme name is stored available via the #locator message.
"
Class {
	#name : 'RwGenericUrl',
	#superclass : 'RwUrl',
	#instVars : [
		'schemeName',
		'locator'
	],
	#category : 'Rowan-Url'
}

{ #category : 'parsing' }
RwGenericUrl class >> fromString: aString [
	| schemeName locator |
	schemeName := RwUrl schemeNameForString: aString.
	schemeName ifNil: [ ^self schemeName: 'xnoscheme' locator: aString ].
	locator := aString copyFrom: (schemeName size + 2) to: aString size.
	^self schemeName: schemeName locator: locator

]

{ #category : 'instance creation' }
RwGenericUrl class >> schemeName: schemeName  locator: locator [
	^self new schemeName: schemeName  locator: locator

]

{ #category : 'access' }
RwGenericUrl >> locator [
	^locator

]

{ #category : 'paths' }
RwGenericUrl >> pathForDirectory [

	^self locator
]

{ #category : 'printing' }
RwGenericUrl >> printOn: aStream [
	
	self schemeName ifNotNil: [
		aStream nextPutAll: self schemeName; nextPut: $:].
	
	aStream nextPutAll: self locator.

	self fragment ifNotNil: [
		aStream nextPut: $#; nextPutAll: self fragment].

]

{ #category : 'parsing' }
RwGenericUrl >> privateInitializeFromText: aString [
	schemeName := RwUrl schemeNameForString: aString.
	locator :=  schemeName 
		ifNil: [ aString ]
		ifNotNil: [ aString copyFrom: (schemeName size+2) to: aString size ].

]

{ #category : 'parsing' }
RwGenericUrl >> privateInitializeFromText: aString relativeTo: aUrl [
	schemeName := aUrl schemeName.
	locator := aString.

]

{ #category : 'classification' }
RwGenericUrl >> scheme [
	^ self schemeName.

]

{ #category : 'access' }
RwGenericUrl >> schemeName [
	^schemeName

]

{ #category : 'private' }
RwGenericUrl >> schemeName: schemeName0  locator: locator0 [
	schemeName := schemeName0.
	locator := locator0.

]
