"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
externalRequirements

	| unloaded providedByUnloaded |
	unloaded := self itemsWithMissingRequirements.
	providedByUnloaded := (self collection: unloaded
				gather: [:e | e provisions]) asSet.
	^self required keys
		reject: [:globalName | providedByUnloaded includes: globalName]