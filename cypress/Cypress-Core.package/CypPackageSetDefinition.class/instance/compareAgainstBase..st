"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| result |
	result := CypPackageSetModification new.
	self
		compareDictionary: packages
		againstBaseDictionary: aDefinition packages
		into: result.
	result
		before: aDefinition;
		after: self.
	^result