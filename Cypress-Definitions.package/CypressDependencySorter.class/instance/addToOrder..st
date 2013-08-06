"
notice: This work is protected by copyright. All rights reserved.
category: private
"
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
