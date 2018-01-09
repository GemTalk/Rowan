"
No class-specific documentation for CypGsClassPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsClassPatch( classDefinition packageDefinition)

"
Class {
	#name : 'RwGsClassPatch',
	#superclass : 'RwGsPatch',
	#instVars : [
		'classDefinition',
		'packageDefinition'
	],
	#category : 'Rowan-GemStone'
}

{ #category : 'instance creation' }
RwGsClassPatch class >> for: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'private' }
RwGsClassPatch >> _classFormat: fmt forSubclassType: classType [
	"Legal types are normal, variable, and bytes."

	"These values need to be moved into Class"
	^classType = 'normal'
		ifTrue: [ fmt ]
		ifFalse: 
			[ classType = 'variable'
				ifTrue: [ fmt bitOr: 16r4.	"add indexable bit" ]
				ifFalse: 
					[ classType = 'bytes'
						ifTrue: [ (fmt bitAnd: 16r3 bitInvert) bitOr: 16r1 + 16r4 ]
						ifFalse: [self error: 'Unknown class type']]]
]

{ #category : 'accessing' }
RwGsClassPatch >> classDefinition [
	^classDefinition

]

{ #category : 'accessing' }
RwGsClassPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
RwGsClassPatch >> className [

	^classDefinition key
]

{ #category : 'actions' }
RwGsClassPatch >> existingSymbolDictionaryNamed: dictName [
	"If the current session's transient symbol list includes a dictionary with the given name, answer it. "

	| symbolName session symbolList association |
	symbolName := dictName asSymbol.
	session := GsCurrentSession currentSession.
	symbolList := session symbolList.
	^symbolList
		detect: [:each | (each at: symbolName ifAbsent: [nil]) == each]
		ifNone: 
			[ nil ]
]

{ #category : 'applying' }
RwGsClassPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
RwGsClassPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
RwGsClassPatch >> packageDefinition: newValue [
	packageDefinition := newValue

]

{ #category : 'accessing' }
RwGsClassPatch >> packageName [

	^packageDefinition key
]

{ #category : 'accessing' }
RwGsClassPatch >> superclassName [

	^classDefinition superclassName
]
