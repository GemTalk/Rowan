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
		'packageDefinition',
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
	class := createdClasses at: className
				ifAbsent: 
					[tempSymbols at: className
						ifAbsent: [self error: 'Cannot find class to compile method in.']].
	behavior := isMeta ifTrue: [class class] ifFalse: [class].
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
				environmentId: 0

]

{ #category : 'accessing' }
RwGsMethodPatch >> isMeta: newValue [
	isMeta := newValue

]

{ #category : 'private' }
RwGsMethodPatch >> loadedClassOrExtensionForMethod [
	"The loaded class or extension should already exist."

	| packageName loadedPackage className |
	packageName := packageDefinition key.
	loadedPackage := Rowan loadedPackageNamed: packageName
				ifAbsent: 
					[self
						error: 'Internal error -- attempt to add a method to a nonexistent package.'].
	className := classDefinition key.
	^loadedPackage classOrExtensionForClassNamed: className
		ifAbsent: 
			[self
				error: 'Internal error -- attempt to add a method to a package in which its class is neither defined nor extended.']
]

{ #category : 'applying' }
RwGsMethodPatch >> markPackageNotDirty [

	self markNotDirtyPackageNamed: packageDefinition key
]

{ #category : 'accessing' }
RwGsMethodPatch >> methodDefinition [
	^methodDefinition

]

{ #category : 'accessing' }
RwGsMethodPatch >> methodDefinition: newValue [
	methodDefinition := newValue

]

{ #category : 'accessing' }
RwGsMethodPatch >> packageDefinition [
	^packageDefinition

]

{ #category : 'accessing' }
RwGsMethodPatch >> packageDefinition: newValue [
	packageDefinition := newValue

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

{ #category : 'initializers' }
RwGsMethodPatch >> runInitializer [
	"execute class initialization methods"

	(isMeta and: [ selector == #initialize ])
		ifTrue: [ behavior theNonMetaClass initialize ]
]
