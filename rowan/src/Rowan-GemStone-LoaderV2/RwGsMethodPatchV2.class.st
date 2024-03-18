Class {
	#name : 'RwGsMethodPatchV2',
	#superclass : 'RwGsPatchV2',
	#instVars : [
		'isMeta',
		'methodDefinition',
		'classDefinition',
		'behavior',
		'selector',
		'compiledMethod'
	],
	#category : 'Rowan-GemStone-LoaderV2'
}

{ #category : 'instance creation' }
RwGsMethodPatchV2 class >> forMethod: aMethodDefinition isMeta: isMeta inClass: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		methodDefinition: aMethodDefinition;
		isMeta: isMeta;
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> behavior [

	^ behavior

]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> classDefinition [

	^ classDefinition
]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> className [

	^ self classDefinition key
]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> compiledMethod [

	^ compiledMethod
		ifNil: [ 
			| methodDictionary |
			methodDictionary := (behavior persistentMethodDictForEnv: 0 ) ifNil:[ Dictionary new ].
			selector := methodDefinition selector.
			compiledMethod := methodDictionary
				at: selector
				ifAbsent: [ self error: 'internal error - compiled method not present in method dictionary' ] ]
]

{ #category : 'compiling' }
RwGsMethodPatchV2 >> compileUsingNewClassesSymbolList: createdClasses andExistingClasses: tempSymbols [
	self
		primeBehaviorNewClassesSymbolList: createdClasses
		andExistingClasses: tempSymbols.
	behavior
		ifNil: [ 
			self
				error:
					'Class ' , self className printString , ' not found in the symbol dictionary '
						, self symbolDictionaryName printString , ' associated with the method '
						, methodDefinition selector printString ].

	[ 
	| sourceString protocol symbolList |
	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: 'protocol') asSymbol.
	compiledMethod := behavior
		compileMethod: sourceString
		dictionaries: symbolList
		category: protocol
		intoMethodDict: false
		intoCategories: nil
		environmentId: self methodEnvironmentId	"we do not want the compiled method added to the class methodDictionary" ]
		on: CompileError , CompileWarning
		do: [ :ex | 
			ex
				addText:
					(RwRepositoryComponentProjectReaderVisitor
						lineNumberStringForDefinition: methodDefinition).
			ex pass ]
]

{ #category : 'initializers' }
RwGsMethodPatchV2 >> isAnInitializer [

	"answer true if I represent a class initialization method"

	^ isMeta and: [ selector == #'initialize' ]

]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> methodDefinition [
	^methodDefinition

]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> methodDefinition: newValue [
	methodDefinition := newValue

]

{ #category : 'compiling' }
RwGsMethodPatchV2 >> methodEnvironmentId [

	"use the project load spec package properties"

	^ self projectDefinition gemstoneMethodEnvForPackageNamed: self packageName
]

{ #category : 'private' }
RwGsMethodPatchV2 >> primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].
	className := classDefinition key asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ 
					"cannot find class ... caller can decide whether or not that is a problem"
					^ self ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ]
]

{ #category : 'private' }
RwGsMethodPatchV2 >> primeBehaviorNewClassesSymbolList: createdClassesSymbolList andExistingClasses: tempSymbols [
	| className class symDictName |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].
	className := classDefinition key asSymbol.
	symDictName := self symbolDictionaryName.
	class := (RwGsPatchSet_V2
		lookupSymbolDictName: symDictName
		in: createdClassesSymbolList)
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ 
					"cannot find class ... caller can decide whether or not that is a problem"
					^ self ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ]
]

{ #category : 'printing' }
RwGsMethodPatchV2 >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: self className asString.
	isMeta
		ifTrue: [ aStream nextPutAll: ' class' ].
	aStream
		nextPutAll: '>>';
		nextPutAll: methodDefinition key asString
]

{ #category : 'private-installing' }
RwGsMethodPatchV2 >> propertiesProtocolName [

	^methodDefinition propertyAt: 'protocol' ifAbsent: ['unknown']
]

{ #category : 'initializers' }
RwGsMethodPatchV2 >> runInitializer [

	"execute class initialization methods"

	self isAnInitializer
		ifTrue: [ behavior thisClass initialize ]

]

{ #category : 'accessing' }
RwGsMethodPatchV2 >> selector [

	^ selector
]
