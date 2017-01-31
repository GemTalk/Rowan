"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| modification mod |
	modification := CypModificationSet before: aDefinition after: self.
	mod := self comparePropertiesAgainstBase: aDefinition.
	mod isEmpty ifFalse: [modification addAll: mod].
	mod := self compareDictionary: classMethodDefinitions
				againstBaseDictionary: aDefinition classMethodDefinitions.
	mod isEmpty ifFalse: [modification addAll: mod].
	mod := self compareDictionary: instanceMethodDefinitions
				againstBaseDictionary: aDefinition instanceMethodDefinitions.
	mod isEmpty ifFalse: [modification addAll: mod].
	^modification