"
notice: This work is protected by copyright. All rights reserved.
category: private
"
addRequirements: aCollection for: aPatchOperation
	aCollection do: [:globalName | self addRequirement: globalName for: aPatchOperation]
