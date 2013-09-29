"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
addToOrder: aPatchOperation
	self orderedItems add: aPatchOperation.
	aPatchOperation provisions do: [:globalName | self addProvision: globalName ].
