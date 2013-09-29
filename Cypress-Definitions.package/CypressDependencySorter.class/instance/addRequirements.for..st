"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
