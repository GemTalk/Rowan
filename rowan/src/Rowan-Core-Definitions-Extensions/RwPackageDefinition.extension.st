Extension { #name : 'RwPackageDefinition' }

{ #category : '*rowan-core-definitions-extensions' }
RwPackageDefinition >> compareAgainstBase: aDefinition [

	| modification classesModification classExtensionsModification traitsModification |
	modification := RwPackageModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	classesModification := RwClassesModification new.
	classExtensionsModification := RwClassExtensionsModification new.
	traitsModification := RwTraitsModification new.
	self
		compareDictionary: self classDefinitionsForCompare
		againstBaseDictionary: aDefinition classDefinitionsForCompare
		into: classesModification
		elementClass: RwClassDefinition.
	self
		compareDictionary: self classExtensionsForCompare
		againstBaseDictionary: aDefinition classExtensionsForCompare
		into: classExtensionsModification
		elementClass: RwClassExtensionDefinition.
	self
		compareDictionary: self traitDefinitionsForCompare
		againstBaseDictionary: aDefinition traitDefinitionsForCompare
		into: traitsModification
		elementClass: RwTraitDefinition.
	modification
		classesModification: classesModification;
		traitsModification: traitsModification;
		classExtensionsModification: classExtensionsModification.
	^modification
]
