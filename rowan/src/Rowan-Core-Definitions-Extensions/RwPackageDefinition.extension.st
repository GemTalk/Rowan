Extension { #name : 'RwPackageDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwPackageDefinition >> compareAgainstBase: aDefinition [

	| modification classesModification classExtensionsModification |
	modification := RwPackageModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	classesModification := RwClassesModification new.
	classExtensionsModification := RwClassExtensionsModification new.
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
