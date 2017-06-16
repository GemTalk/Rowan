"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: miscellany
"
collection: aCollection gather: aOneArgBlock
	"Evaluate the block once for each element of aCollection. The block should answer a collection. 
	Answer an Array containing all elements of all the answered collections."

	| result |
	result := Array new.
	aCollection do: [:each | result addAll: (aOneArgBlock value: each)].
	^result