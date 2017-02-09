"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| modification classesModification classExtensionsModification |
	modification := CypPackageModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	classesModification := CypClassesModification new.
	classExtensionsModification := CypClassesModification new.
	self
		compareDictionary: classDefinitions
		againstBaseDictionary: aDefinition classDefinitions
		into: classesModification.
	self
		compareDictionary: classExtensions
		againstBaseDictionary: aDefinition classExtensions
		into: classExtensionsModification.
	modification
		classesModification: classesModification;
		classExtensionsModification: classExtensionsModification.
	^modification