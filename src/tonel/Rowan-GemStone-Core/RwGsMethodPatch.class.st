"
No class-specific documentation for CypGsMethodPatch, hierarchy is: 
Object
  CypGsPatch
    CypGsMethodPatch( isMeta methodDefinition classDefinition packageDefinition)

"
Class {
	#name : 'RwGsMethodPatch',
	#superclass : 'RwGsPatch',
	#instVars : [
		'isMeta',
		'methodDefinition',
		'classDefinition',
		'behavior',
		'selector',
		'compiledMethod'
	],
	#category : 'Rowan-GemStone-Core'
}

{ #category : 'instance creation' }
RwGsMethodPatch class >> forMethod: aMethodDefinition isMeta: isMeta inClass: aClassDefinition inPackage: aPackageDefinition [

	^(self new)
		methodDefinition: aMethodDefinition;
		isMeta: isMeta;
		classDefinition: aClassDefinition;
		packageDefinition: aPackageDefinition;
		yourself
]

{ #category : 'accessing' }
RwGsMethodPatch >> behavior [

	^ behavior

]

{ #category : 'accessing' }
RwGsMethodPatch >> classDefinition [

	^ classDefinition
]

{ #category : 'accessing' }
RwGsMethodPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'accessing' }
RwGsMethodPatch >> className [

	^ self classDefinition key
]

{ #category : 'accessing' }
RwGsMethodPatch >> compiledMethod [

	^ compiledMethod
		ifNil: [ 
			| methodDictionary |
			methodDictionary := behavior persistentMethodDictForEnv: 0.
			selector := methodDefinition selector.
			compiledMethod := methodDictionary
				at: selector
				ifAbsent: [ self error: 'internal error - compiled method not present in method dictionary' ] ]
]

{ #category : 'compiling' }
RwGsMethodPatch >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

	| sourceString symbolList protocol |
	self primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols.
	behavior
		ifNil: [ self error: 'Class ' , self className printString , ' not found.' ].

	sourceString := methodDefinition source.
	symbolList := SymbolList with: tempSymbols.
	protocol := (methodDefinition propertyAt: 'protocol') asSymbol.
	compiledMethod := behavior
		compileMethod: sourceString
		dictionaries: symbolList
		category: protocol
		intoMethodDict: false
		intoCategories: nil
		intoPragmas: nil
		environmentId: self methodEnvironmentId
]

{ #category : 'initializers' }
RwGsMethodPatch >> isAnInitializer [

	"answer true if I represent a class initialization method"

	^ isMeta and: [ selector == #'initialize' ]

]

{ #category : 'private-testing' }
RwGsMethodPatch >> isForExtensionMethod [

	"Answer true if the method is not in the same package that defines the class."

	| packageOfClass packageOfMethod loadedClass |
	packageOfMethod := self packageName.
	loadedClass := RwGsLoadedClass existingForClass: behavior thisClass.
	packageOfClass := loadedClass loadedPackage name.
	^ packageOfClass ~= packageOfMethod
]

{ #category : 'accessing' }
RwGsMethodPatch >> isMeta [

	^ isMeta
]

{ #category : 'accessing' }
RwGsMethodPatch >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'private' }
RwGsMethodPatch >> loadedClassOrExtensionForMethod [

	"The loaded class or extension should already exist."

	| loadedPackage className |
	loadedPackage := Rowan
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
RwGsMethodPatch >> methodDefinition [
	^methodDefinition

]

{ #category : 'accessing' }
RwGsMethodPatch >> methodDefinition: newValue [
	methodDefinition := newValue

]

{ #category : 'compiling' }
RwGsMethodPatch >> methodEnvironmentId [

	"use the project load spec package properties"

	^ self projectDefinition methodEnvForPackageNamed: self packageName
]

{ #category : 'private-installing' }
RwGsMethodPatch >> packageProtocolName [

	"If I'm for an extension method, my protocol in the image may need to be *<packageName>*."

	^ '*' , self packageName
]

{ #category : 'private' }
RwGsMethodPatch >> primeBehaviorNewClasses: createdClasses andExistingClasses: tempSymbols [

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

{ #category : 'printing' }
RwGsMethodPatch >> printOn: aStream [

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
RwGsMethodPatch >> propertiesProtocolName [

	^methodDefinition propertyAt: 'protocol' ifAbsent: ['unknown']
]

{ #category : 'initializers' }
RwGsMethodPatch >> runInitializer [

	"execute class initialization methods"

	self isAnInitializer
		ifTrue: [ behavior thisClass initialize ]

]

{ #category : 'accessing' }
RwGsMethodPatch >> selector [

	^ selector
]

{ #category : 'private-installing' }
RwGsMethodPatch >> setProtocol [
	"If I'm an extension method, my protocol in the image may need to be *<packageName>."

	self
		setProtocolTo: (Rowan configuration needsPackageProtocolNames
				& self isForExtensionMethod
					ifTrue: [self packageProtocolName]
					ifFalse: [self propertiesProtocolName])
]

{ #category : 'private-installing' }
RwGsMethodPatch >> setProtocolTo: protocolString [

	| protocolSymbol |
	protocolSymbol := protocolString asSymbol.
	(behavior includesCategory: protocolSymbol)
		ifFalse: [behavior addCategory: protocolSymbol].
	behavior moveMethod: selector toCategory: protocolSymbol
]
