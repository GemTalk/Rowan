"
The purpose of this class is to have a platform-specific class with a generic name, through which we can resolve other names in a platform-specific manner.
This is the GemStone-specific one.
"
Class {
	#name : 'CypPlatformSpecificNameResolver',
	#superclass : 'Object',
	#category : 'Cypress-GemStone'
}

{ #category : 'queries' }
CypPlatformSpecificNameResolver class >> resolve: aName ifAbsent: absentBlock [
	"This is the GemStone version, which will also work with other platforms which have SUnit in the base."

	| resolution |
	resolution := SUnitNameResolver classNamed: aName.
	^resolution isNil ifTrue: [absentBlock value] ifFalse: [resolution]
]
