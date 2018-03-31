Class {
	#name : 'TDRowanMethodDefinitionElementBuilder',
	#superclass : 'TDMethodDefinitionElementBuilder',
	#category : 'Rowan-Tode'
}

{ #category : 'method source' }
TDRowanMethodDefinitionElementBuilder >> compileSource: theSource [

	| selector newSelector |
	selector := self selector.
	newSelector := self object
		rwCompile: theSource withGemstoneLineEndings
		classified: self methodProtocol.
	(selector asSymbol ~~ newSelector or: [ self object username notNil ])
		ifTrue: [ self differentMethodCreated: newSelector ].
	^ newSelector
]

{ #category : 'accessing' }
TDRowanMethodDefinitionElementBuilder >> editorWindowLabel [

	^ self inClass printString , '>>' , self selector asString , ' ['
		, self methodProtocol , ':' , self projectName , ':' , self packageName , ']'
]

{ #category : 'accessing' }
TDRowanMethodDefinitionElementBuilder >> packageName [

	^ self object rowanPackageName
]

{ #category : 'accessing' }
TDRowanMethodDefinitionElementBuilder >> projectName [

	^ self object rowanProjectName
]
