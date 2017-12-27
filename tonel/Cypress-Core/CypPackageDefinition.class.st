"
Defines a package, which is a bunch of class definitions and class extensions.
Properties include 'name'.
Sub-definitions are classDefinitions and classExtensions

"
Class {
	#name : 'CypPackageDefinition',
	#superclass : 'CypDefinition',
	#instVars : [
		'classDefinitions',
		'classExtensions'
	],
	#category : 'Cypress-Core'
}

{ #category : 'instance creation' }
CypPackageDefinition class >> withProperties: properties classDefinitions: classDefinitions classExtensions: classExtensionDefinitions [

	^(self basicNew)
		properties: properties;
		classDefinitions: classDefinitions;
		classExtensions: classExtensionDefinitions;
		yourself
]

{ #category : 'accessing' }
CypPackageDefinition >> addClassDefinition: aClassDefinition [

	self addDefinition: aClassDefinition to: classDefinitions
]

{ #category : 'accessing' }
CypPackageDefinition >> addClassExtension: aClassExtensionDefinition [

	self addDefinition: aClassExtensionDefinition to: classExtensions
]

{ #category : 'accessing' }
CypPackageDefinition >> classDefinitions [

	^classDefinitions
]

{ #category : 'accessing' }
CypPackageDefinition >> classDefinitions: classDefinitionDictionary [

	classDefinitions := classDefinitionDictionary
]

{ #category : 'accessing' }
CypPackageDefinition >> classExtensions [

	^classExtensions
]

{ #category : 'accessing' }
CypPackageDefinition >> classExtensions: classExtensionDefinitionsDictionary [

	classExtensions := classExtensionDefinitionsDictionary
]

{ #category : 'comparing' }
CypPackageDefinition >> compareAgainstBase: aDefinition [

	| modification classesModification classExtensionsModification |
	modification := CypPackageModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	classesModification := CypClassesModification new.
	classExtensionsModification := CypClassesModification new.
	self
		compareDictionary: classDefinitions
		againstBaseDictionary: aDefinition classDefinitions
		into: classesModification
		elementClass: CypClassDefinition.
	self
		compareDictionary: classExtensions
		againstBaseDictionary: aDefinition classExtensions
		into: classExtensionsModification
		elementClass: CypClassExtensionDefinition.
	modification
		classesModification: classesModification;
		classExtensionsModification: classExtensionsModification.
	^modification
]

{ #category : 'initialization' }
CypPackageDefinition >> initialize [

	super initialize.
	classDefinitions := Dictionary new.
	classExtensions := Dictionary new
]

{ #category : 'testing' }
CypPackageDefinition >> isEmpty [
	"Answer true if this definition does not actually define anything."

	^super isEmpty & classDefinitions isEmpty & classExtensions isEmpty
]

{ #category : 'properties' }
CypPackageDefinition >> key [
	"Answer an object that can be used to uniquely identify myself in the context of my container."

	^self propertyAt: 'name' ifAbsent: [nil]
]
