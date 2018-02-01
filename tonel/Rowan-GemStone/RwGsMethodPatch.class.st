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
	#category : 'Rowan-GemStone'
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
RwGsMethodPatch >> classDefinition: newValue [
	classDefinition := newValue

]

{ #category : 'compiling' }
RwGsMethodPatch >> compileUsingNewClasses: createdClasses andExistingClasses: tempSymbols [

	| className class sourceString symbolList protocol |
	className := classDefinition key asSymbol.
	class := createdClasses
		at: className
		ifAbsent: [ 
			tempSymbols
				at: className
				ifAbsent: [ self error: 'Cannot find class to compile method in.' ] ].
	behavior := isMeta
		ifTrue: [ class class ]
		ifFalse: [ class ].
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

{ #category : 'applying' }
RwGsMethodPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: self packageName
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

	^ (self projectLoadSpecification platformSpec at: 'gemstone')
		methodEnvForPackageNamed: self packageName
]

{ #category : 'private-installing' }
RwGsMethodPatch >> packageProtocolName [

	"If I'm for an extension method, my protocol in the image may need to be *<packageName>*."

	^ '*' , self packageName
]

{ #category : 'printing' }
RwGsMethodPatch >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPutAll: ' for ';
		nextPutAll: classDefinition key.
	isMeta ifTrue: [aStream nextPutAll: ' class'].
	aStream
		nextPutAll: '>>';
		nextPutAll: methodDefinition key
]

{ #category : 'private-installing' }
RwGsMethodPatch >> propertiesProtocolName [

	^methodDefinition propertyAt: 'protocol' ifAbsent: ['unknown']
]

{ #category : 'initializers' }
RwGsMethodPatch >> runInitializer [

	"execute class initialization methods"

	(isMeta and: [ selector == #'initialize' ])
		ifTrue: [ behavior thisClass initialize ]
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
