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
RwGsMethodPatchV2 >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

  [
	  | sourceString symbolList protocol |
	  sourceString := methodDefinition source.
	  symbolList := SymbolList with: tempSymbols.
	  protocol := (methodDefinition propertyAt: 'protocol') asSymbol.
	  compiledMethod := behavior
		  compileMethod: sourceString
		  dictionaries: symbolList
		  category: protocol
		  intoMethodDict: false "we do not want the compiled method added to the class methodDictionary"
		  intoCategories: nil
		  intoPragmas: nil
		  environmentId: self methodEnvironmentId
  ] on: (CompileError, CompileWarning) do:[:ex |
    ex addText: (RwRepositoryResolvedProjectTonelReaderVisitorV2 lineNumberStringForMethod: methodDefinition).
    ex pass 
  ]
]

{ #category : 'compiling' }
RwGsMethodPatchV2 >> compileUsingNewClasses: createdClasses andExistingClassSymbolList: tempSymbolList [
	self
		primeBehaviorNewClasses: createdClasses
		andExistingClassSymbolList: tempSymbolList.
	behavior
		ifNil: [ 
			self
				error:
					'Class ' , self className printString , ' not found in the symbol dictionary '
						, self symbolDictionaryName printString ].

	[ 
	| sourceString protocol |
	sourceString := methodDefinition source.
	protocol := (methodDefinition propertyAt: 'protocol') asSymbol.
	compiledMethod := behavior
		compileMethod: sourceString
		dictionaries: tempSymbolList
		category: protocol
		intoMethodDict: false
		intoCategories: nil
		intoPragmas: nil
		environmentId: self methodEnvironmentId	"we do not want the compiled method added to the class methodDictionary" ]
		on: CompileError , CompileWarning
		do: [ :ex | 
			ex
				addText:
					(RwRepositoryResolvedProjectTonelReaderVisitorV2
						lineNumberStringForMethod: methodDefinition).
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

{ #category : 'private' }
RwGsMethodPatchV2 >> loadedClassOrExtensionForMethod [

	"The loaded class or extension should already exist."

	| loadedPackage className |
	loadedPackage := Rowan image
		loadedPackageNamed: self packageName
		ifAbsent: [ 
			self
				error: 'Internal error -- attempt to add a method to a nonexistent package.' ].
	className := classDefinition key.
	^ loadedPackage
		classOrExtensionForClassNamed: className
		ifAbsent: [ 
			self
				error:
					'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.' ]

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

	^ self projectDefinition methodEnvForPackageNamed: self packageName
]

{ #category : 'private-installing' }
RwGsMethodPatchV2 >> packageProtocolName [

	"If I'm for an extension method, my protocol in the image may need to be *<packageName>*."

	^ '*' , self packageName
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
RwGsMethodPatchV2 >> primeBehaviorNewClasses: createdClassesSymbolList andExistingClassSymbolList: tempSymbolList [
	| className class symDictName |
	classDefinition key
		ifNil: [ 
			"class is being deleted ... we're done"
			^ self ].
	className := classDefinition key asSymbol.
	symDictName := self symbolDictionaryName.
	class := (RwGsPatchSet_V2_symbolList
		lookupSymbolDictName: symDictName
		in: createdClassesSymbolList)
		at: className
		ifAbsent: [ 
			(RwGsPatchSet_V2_symbolList lookupSymbolDictName: symDictName in: tempSymbolList)
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

{ #category : 'private-installing' }
RwGsMethodPatchV2 >> setProtocolTo: protocolString [

	| protocolSymbol |
	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [behavior addCategory: protocolSymbol].
	behavior moveMethod: selector toCategory: protocolSymbol
]
