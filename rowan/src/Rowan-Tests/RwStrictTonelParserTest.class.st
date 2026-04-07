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

{ #category : 'private' }
RwStrictTonelParserTest >> tonelParserClass [

	^RwStrictTonelParser
]
