"
notice: This work is protected by copyright. All rights reserved.
category: visiting
"
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
