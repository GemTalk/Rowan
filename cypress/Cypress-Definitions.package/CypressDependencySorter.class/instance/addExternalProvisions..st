"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addExternalProvisions: aCollection
	(aCollection intersection: self externalRequirements)
		do: [:globalName | self addProvision: globalName]
