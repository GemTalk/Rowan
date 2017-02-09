"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| modification instanceMethodsModification classMethodsModification |
	modification := CypClassModification before: aDefinition after: self.
	modification
		propertiesModification: (self comparePropertiesAgainstBase: aDefinition).
	instanceMethodsModification := CypMethodsModification new.
	classMethodsModification := CypMethodsModification new.
	self
		compareDictionary: instanceMethodDefinitions
		againstBaseDictionary: aDefinition instanceMethodDefinitions
		into: instanceMethodsModification.
	self
		compareDictionary: classMethodDefinitions
		againstBaseDictionary: aDefinition classMethodDefinitions
		into: classMethodsModification.
	modification
		instanceMethodsModification: instanceMethodsModification;
		classMethodsModification: classMethodsModification.
	^modification