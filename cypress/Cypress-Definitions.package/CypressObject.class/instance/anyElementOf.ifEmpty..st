"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
anyElementOf: aCollection ifEmpty: aBlock

	aCollection do: [:each | ^each].
	^aBlock value
