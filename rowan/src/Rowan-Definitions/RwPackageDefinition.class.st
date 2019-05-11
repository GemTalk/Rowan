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
RwPackageDefinition >> addClassNamed: className super: superclassName instvars: instvars classinstvars: classinstvars classvars: classvars category: category comment: comment pools: pools type: type [

	^ self addClassDefinition: 
		(RwClassDefinition 
			newForClassNamed: className 
				super: superclassName 
				instvars: instvars 
				classinstvars: 
				classinstvars 
				classvars: classvars 
				category: category 
				comment: comment 
				pools: pools 
				type: type)
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
RwPackageDefinition >> removeClassNamed: className [

	self removeKey: className from: classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> updateClassDefinition: aClassDefinition [

	self updateDefinition: aClassDefinition in: classDefinitions
]
