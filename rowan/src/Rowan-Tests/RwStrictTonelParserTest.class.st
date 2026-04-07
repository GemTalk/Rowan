Class {
	#name : 'RwStrictTonelParserTest',
	#superclass : 'RwTonelParserTest',
	#category : 'Rowan-Tests'
}

{ #category : 'Testing' }
RwStrictTonelParserTest class >> shouldInheritSelectors [
	"Inherit selectors"

	^true
]

{ #category : 'tests' }
RwStrictTonelParserTest >> testMethod_01 [
	"common behavior between RwTonelParser and RwStrictTonelParser"

	| gotError |
	self
		assertParse: 'Object>>name'
		rule: #'method'
		equals: #(#('Object' nil) 'name').

	self
		assertParse: 'Object >> name: aString'
		rule: #'method'
		equals: #(#('Object' nil) 'name: aString').

	self
		assertParse: 'Object >> name: aString [ I do not care ]'
		rule: #'method'
		equals: #(#('Object' nil) 'name: aString').
	self
		assertParse: 'Object class >> name: aString'
		rule: #'method'
		equals: #(#('Object' 'class') 'name: aString').
	gotError := false.
	[ 
	self
		assertParse: 'Object class name: aString'
		rule: #'method'
		equals: 'single > ... expect error' ]
		on: Error
		do: [ :ex | 
			self
				assert: ex printString
				equals:
					'a RwTonelParseError occurred (error 2710), incorrect selector pattern. Expecting ''className >> selector'' or ''className class >> selector''  '.
			gotError := true ].
	gotError
		ifFalse: [ self error: 'missing expected error' ]
]

{ #category : 'tests' }
RwStrictTonelParserTest >> testMethod_02 [
	"different behavior between RwTonelParser and RwStrictTonelParser"

	| gotError |
	gotError := false.
	[ 
	"error message is not specific ... RwStrictTonelParser should provied a detailed error"
	self
		assertParse: 'Object class >name: aString'
		rule: #'method'
		equals: 'single > ... expect error' ]
		on: Error
		do: [ :ex | 
			self
				assert: ex printString
				equals: 'a RwTonelParseError occurred (error 2710), missing >> in tonel method source'.
			gotError := true ].
	gotError
		ifFalse: [ self error: 'missing expected error' ].
]

{ #category : 'private' }
RwStrictTonelParserTest >> tonelParserClass [

	^RwStrictTonelParser
]
