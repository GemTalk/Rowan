"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: visiting
"
instanceMethod: instanceBlock classMethod: classBlock

	^(self classIsMeta
		ifTrue: [ classBlock ]
		ifFalse: [ instanceBlock ]) value: self
