"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: comparing
"
compareAgainstBase: aDefinition

	| modifications |
	modifications := self compareDictionary: packages
				againstBaseDictionary: aDefinition packages.
	modifications
		before: aDefinition;
		after: self.
	^modifications