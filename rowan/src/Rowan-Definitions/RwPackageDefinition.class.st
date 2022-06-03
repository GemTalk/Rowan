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
		'classExtensions'
	],
	#category : 'Rowan-Definitions'
}

{ #category : 'instance creation' }
RwPackageDefinition class >> newNamed: packageName [

	^ self
		withProperties: (Dictionary with: 'name' -> packageName)
		classDefinitions: Dictionary new
		classExtensions: Dictionary new
]

{ #category : 'instance creation' }
RwPackageDefinition class >> withProperties: properties classDefinitions: classDefinitions classExtensions: classExtensionDefinitions [

	^(self basicNew)
		properties: properties;
		classDefinitions: classDefinitions;
		classExtensions: classExtensionDefinitions;
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

{ #category : 'initialization' }
RwPackageDefinition >> initialize [

	super initialize.
	classDefinitions := Dictionary new.
	classExtensions := Dictionary new
]

{ #category : 'testing' }
RwPackageDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & classDefinitions isEmpty & classExtensions isEmpty
]

{ #category : 'properties' }
RwPackageDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self propertyAt: 'name' ifAbsent: [nil]
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
RwPackageDefinition >> removeClassNamed: className [
	^ self removeKey: className from: classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> removeClassNamed: className ifAbsent: absentBlock [

	self removeKey: className from: classDefinitions ifAbsent: absentBlock
]

{ #category : 'accessing' }
RwPackageDefinition >> renameTo: newPackageName packageConvention: thePackageConvention [
	self propertyAt: 'name' put: newPackageName.
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
RwPackageDefinition >> updateClassDefinition: aClassDefinition [

	self updateDefinition: aClassDefinition in: classDefinitions
]
