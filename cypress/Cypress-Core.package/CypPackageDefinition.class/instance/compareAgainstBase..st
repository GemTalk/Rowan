"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| modification mod |
	modification := CypModificationSet new.
	mod := self comparePropertiesAgainstBase: aDefinition.
	mod isEmpty ifFalse: [modification addAll: mod].
	mod := self compareDictionary: classDefinitions
				againstBaseDictionary: aDefinition classDefinitions.
	mod isEmpty ifFalse: [modification addAll: mod].
	mod := self compareDictionary: classExtensions
				againstBaseDictionary: aDefinition classExtensions.
	mod isEmpty ifFalse: [modification addAll: mod].
	^modification