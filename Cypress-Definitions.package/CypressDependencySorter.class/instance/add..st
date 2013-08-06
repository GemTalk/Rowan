"
notice: This work is protected by copyright. All rights reserved.
category: building
"
add: aPatchOperation
	| requirements |
	requirements := self unresolvedRequirementsFor: aPatchOperation.
	requirements isEmpty
		ifTrue: [self addToOrder: aPatchOperation]
		ifFalse: [self addRequirements: requirements for: aPatchOperation].
	^ aPatchOperation
