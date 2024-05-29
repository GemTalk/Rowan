"
Defines a package, which is a bunch of class definitions and class extensions.
Properties include 'name'.
Sub-definitions are classDefinitions and classExtensions

"
Class {
	#name : 'RwPackageDefinition',
	#superclass : 'RwDefinition',
	#instVars : [
		'classDefinitions',
		'classExtensions',
		'traitDefinitions'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwPackageDefinition class >> newNamed: packageName [

	^ self
		withProperties: (SymbolDictionary with: #'name' -> packageName)
		classDefinitions: Dictionary new
		classExtensions: Dictionary new
		traitDefinitions: Dictionary new
]

{ #category : 'instance creation' }
RwPackageDefinition class >> withProperties: properties classDefinitions: classDefinitions classExtensions: classExtensionDefinitions [

	^(self basicNew)
		properties: properties;
		classDefinitions: classDefinitions;
		classExtensions: classExtensionDefinitions;
		yourself
]

{ #category : 'instance creation' }
RwPackageDefinition class >> withProperties: properties classDefinitions: classDefinitions classExtensions: classExtensionDefinitions traitDefinitions: traitDefinitions [

	^(self basicNew)
		properties: properties;
		classDefinitions: classDefinitions;
		classExtensions: classExtensionDefinitions;
		traitDefinitions: traitDefinitions;
		yourself
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassDefinition: aClassDefinition [

	^ self addDefinition: aClassDefinition to: classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassExtensionDefinition: aClassExtensionDefinition [

	^ self addDefinition: aClassExtensionDefinition to: classExtensions
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassExtensionNamed: className [

	^ self addClassExtensionDefinition: (RwClassExtensionDefinition newForClassNamed: className)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName category: categryName [

	^ self addClassDefinition: (RwClassDefinition newForClassNamed: className super: superclassName  category: categryName)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName category: categryName comment: comment [

	^ self addClassDefinition: (RwClassDefinition newForClassNamed: className super: superclassName  category: categryName comment: comment)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName category: category comment: comment type: type [
	"byteSubclass classes don't declare instvars"

	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: category
				comment: comment
				pools: #()
				type: type)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName category: category type: type [
	"byteSubclass classes don't declare instvars"

	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: #()
				classinstvars: #()
				classvars: #()
				category: category
				comment: nil
				pools: #()
				type: type)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type [
	"byteSubclass classes don't declare instvars"

	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: #()
				classinstvars: classinstvars
				classvars: classvars
				category: category
				comment: comment
				pools: pools
				type: type)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName classinstvars: classinstvars classvars: classvars category: category comment: comment type: type [
	"byteSubclass classes don't declare instvars"

	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: #()
				classinstvars: classinstvars
				classvars: classvars
				category: category
				comment: comment
				pools: #()
				type: type)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName instvars: instvars [
	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: instvars)
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName instvars: instvars category: category comment: comment [
	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: instvars
				classinstvars: #()
				classvars: #()
				category: category
				comment: comment
				pools: #()
				type: 'normal')
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment [
	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: instvars
				classinstvars: classinstvars
				classvars: classvars
				category: category
				comment: comment
				pools: #()
				type: 'normal')
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools [
	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: instvars
				classinstvars: classinstvars
				classvars: classvars
				category: category
				comment: comment
				pools: pools
				type: 'normal')
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassNamed: className super: superclassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type [
	^ self
		addClassDefinition:
			(RwClassDefinition
				newForClassNamed: className
				super: superclassName
				instvars: instvars
				classinstvars: classinstvars
				classvars: classvars
				category: category
				comment: comment
				pools: pools
				type: type)
]

{ #category : 'accessing' }
RwPackageDefinition >> addTraitDefinition: aTraitDefinition [
	aTraitDefinition packageName ifNil: [ aTraitDefinition packageName: self name ].
	aTraitDefinition packageName ~= self name
		ifTrue: [ 
			self error: 'The package name of trait (' , aTraitDefinition packageName.
			'), must match the name of the receiver (' , self name , ').' ].
	^ self addDefinition: aTraitDefinition to: traitDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> addTraitNamed: traitName [
	^ self
		addTraitNamed: traitName
		instvars: #()
		classinstvars: #()
		classvars: #()
]

{ #category : 'accessing' }
RwPackageDefinition >> addTraitNamed: traitName instvars:  instvars classinstvars: classinstvars classvars: classvars [
	^ self
		addTraitNamed: traitName
		instvars:  instvars
		classinstvars: classinstvars
		classvars: classvars
		category: nil
]

{ #category : 'accessing' }
RwPackageDefinition >> addTraitNamed: traitName instvars: instvars classinstvars: classinstvars classvars: classvars category: categoryNameOrNil [
	^ self
		addTraitDefinition:
			(RwTraitDefinition
				newForTraitNamed: traitName
				instvars: instvars
				classinstvars: classinstvars
				classvars: classvars
				category: categoryNameOrNil)
]

{ #category : 'accessing' }
RwPackageDefinition >> classDefinitionNamed: className [

	^ self classDefinitionNamed: className ifAbsent: [ self error: 'No class definition found with the name ', className printString ]
]

{ #category : 'accessing' }
RwPackageDefinition >> classDefinitionNamed: className ifAbsent: absentBlock [

	^ self classDefinitions at: className ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwPackageDefinition >> classDefinitions [

	^classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> classDefinitions: classDefinitionDictionary [

	classDefinitions := classDefinitionDictionary
]

{ #category : 'accessing' }
RwPackageDefinition >> classDefinitionsForCompare [
	^ self classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> classExtensionDefinitionNamed: className [

	^ self classExtensionDefinitionNamed: className ifAbsent: [ self error: 'No class extension definition found with the name ', className printString ]
]

{ #category : 'accessing' }
RwPackageDefinition >> classExtensionDefinitionNamed: className ifAbsent: absentBlock [

	^ self classExtensions at: className ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwPackageDefinition >> classExtensions [

	^classExtensions
]

{ #category : 'accessing' }
RwPackageDefinition >> classExtensions: classExtensionDefinitionsDictionary [

	classExtensions := classExtensionDefinitionsDictionary
]

{ #category : 'accessing' }
RwPackageDefinition >> classExtensionsForCompare [
	^ self classExtensions
]

{ #category : 'initialization' }
RwPackageDefinition >> initialize [

	super initialize.
	classDefinitions := Dictionary new.
	classExtensions := Dictionary new.
	traitDefinitions := Dictionary new.
]

{ #category : 'testing' }
RwPackageDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

  ^ classExtensions isEmpty and:[ classDefinitions isEmpty and:[ super isEmpty]]
]

{ #category : 'properties' }
RwPackageDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self propertyAt: #'name' ifAbsent: [nil]
]

{ #category : 'accessing' }
RwPackageDefinition >> moveClassExtension: classExtensionDefinition modifyMethodDefinitions: modifyMethodDefBlock toPackage: packageDefinition [
	classExtensionDefinition classMethodDefinitions values
		, classExtensionDefinition instanceMethodDefinitions values
		do: [ :methodDef | modifyMethodDefBlock cull: methodDef ].
	packageDefinition addClassExtensionDefinition: classExtensionDefinition
]

{ #category : 'accessing' }
RwPackageDefinition >> moveClassExtension: classExtensionDefinition toPackage: packageDefinition [
	self
		moveClassExtension: classExtensionDefinition
		modifyMethodDefinitions: [  ]
		toPackage: packageDefinition
]

{ #category : 'accessing' }
RwPackageDefinition >> moveClassExtensionNamed: className modifyMethodDefinitions: modifyMethodDefBlock toPackage: packageDefinition [
	| classExtensionDefinition |
	classExtensionDefinition := self classExtensionDefinitionNamed: className.
	self
		moveClassExtension: classExtensionDefinition
		modifyMethodDefinitions: modifyMethodDefBlock
		toPackage: packageDefinition
]

{ #category : 'accessing' }
RwPackageDefinition >> moveClassExtensionNamed: className toPackage: packageDefinition [
	| classExtensionDefinition |
	classExtensionDefinition := self classExtensionNamed: className.
	self
		moveClassExtension: classExtensionDefinition
		modifyMethodDefinitions: [  ]
		toPackage: packageDefinition
]

{ #category : 'accessing' }
RwPackageDefinition >> moveClassNamed: className modifyClassDefinition: classDefinitionBlock toPackage: packageDefinition [
	| classDefinition |
	classDefinition := self removeKey: className from: classDefinitions.
	classDefinitionBlock cull: classDefinition.
	packageDefinition addClassDefinition: classDefinition
]

{ #category : 'accessing' }
RwPackageDefinition >> moveClassNamed: className toPackage: packageDefinition [
	self
		moveClassNamed: className
		modifyClassDefinition: [  ]
		toPackage: packageDefinition
]

{ #category : 'copying' }
RwPackageDefinition >> postCopy [

	| oldDefs |
	super postCopy.
	oldDefs := classDefinitions.
	classDefinitions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | classDefinitions at: key put: value copy ].
	oldDefs := classExtensions.
	classExtensions := Dictionary new.
	oldDefs keysAndValuesDo: [:key :value | classExtensions at: key put: value copy ].
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassDefinition: aClassDefinition [

	self removeDefinition: aClassDefinition from: classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassExtensionDefinition: aClassExtension [

	self removeDefinition: aClassExtension from: classExtensions
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassExtensionDefinitionNamed: className [

	self removeKey: className from: classExtensions
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassExtensionNamed: className [
	^ self removeKey: className from: classExtensions
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassExtensionNamed: className ifAbsent: absentBlock [
	^ self removeKey: className from: classExtensions ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassNamed: className [
	^ self removeKey: className from: classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassNamed: className ifAbsent: absentBlock [

	self removeKey: className from: classDefinitions ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwPackageDefinition >> removeTraitNamed: traitName [
	^ self removeKey: traitName from: traitDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> renameTo: newPackageName packageConvention: thePackageConvention [
	self propertyAt: #'name' put: newPackageName.
	thePackageConvention = 'Rowan'
		ifTrue: [ ^ self ].
	thePackageConvention = 'Monticello'
		ifTrue: [ self error: 'not yet implemented' ].
	thePackageConvention ~= 'RowanHybrid'
		ifTrue: [ self error: 'unknown package convention' ].
	self classDefinitions
		valuesDo: [ :classDef | 
			classDef
				moveToPackageNamed: newPackageName
				packageConvention: thePackageConvention ].
	self classExtensions
		valuesDo: [ :classDef | 
			classDef
				moveToPackageNamed: newPackageName
				packageConvention: thePackageConvention ]
]

{ #category : 'accessing' }
RwPackageDefinition >> traitDefinitionNamed: traitName [

	^ self traitDefinitionNamed: traitName ifAbsent: [ self error: 'No trait definition found with the name ', traitName printString ]
]

{ #category : 'accessing' }
RwPackageDefinition >> traitDefinitionNamed: traitName ifAbsent: absentBlock [

	^ self traitDefinitions at: traitName ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwPackageDefinition >> traitDefinitions [
	^traitDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> traitDefinitions: traitDefinitionsDictionary [
	traitDefinitions := traitDefinitionsDictionary
]

{ #category : 'accessing' }
RwPackageDefinition >> traitDefinitionsForCompare [
	^ self traitDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> updateClassDefinition: aClassDefinition [

	self updateDefinition: aClassDefinition in: classDefinitions
]
