comparing
compareAgainstBase: aDefinition

	| modification instanceMethodsModification classMethodsModification className |
	modification := self _modificationClass before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	className := self _classNameForCompare: aDefinition.
	instanceMethodsModification := self _methodsModificationClass
		extendedClassName: className.
	classMethodsModification := self _methodsModificationClass
		extendedClassName: className.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionary: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification
		elementClass: RwMethodDefinition
		isMeta: false.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionary: aDefinition classMethodDefinitions
		into: classMethodsModification
		elementClass: RwMethodDefinition
		isMeta: true.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^ modification
