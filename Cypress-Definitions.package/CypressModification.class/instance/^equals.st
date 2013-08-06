"
notice: This work is protected by copyright. All rights reserved.
category: initialization
"
= aPatchOperation
	^(super = aPatchOperation) and: [self obsoletion = aPatchOperation obsoletion and: [ self modification = aPatchOperation modification]]
