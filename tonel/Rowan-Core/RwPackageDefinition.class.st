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
	#category : 'Rowan-Core'
}

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

	self addDefinition: aClassDefinition to: classDefinitions
]

{ #category : 'accessing' }
RwPackageDefinition >> addClassExtension: aClassExtensionDefinition [

	self addDefinition: aClassExtensionDefinition to: classExtensions
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
RwPackageDefinition >> classExtensions [

	^classExtensions
]

{ #category : 'accessing' }
RwPackageDefinition >> classExtensions: classExtensionDefinitionsDictionary [

	classExtensions := classExtensionDefinitionsDictionary
]

{ #category : 'comparing' }
RwPackageDefinition >> compareAgainstBase: aDefinition [

	| modification classesModification classExtensionsModification |
	modification := RwPackageModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	classesModification := RwClassesModification new.
	classExtensionsModification := RwClassesModification new.
	self
		compareDictionary: classDefinitions
		againstBaseDictionary: aDefinition classDefinitions
		into: classesModification
		elementClass: RwClassDefinition.
	self
		compareDictionary: classExtensions
		againstBaseDictionary: aDefinition classExtensions
		into: classExtensionsModification
		elementClass: RwClassExtensionDefinition.
	modification
		classesModification: classesModification;
		classExtensionsModification: classExtensionsModification.
	^modification
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
