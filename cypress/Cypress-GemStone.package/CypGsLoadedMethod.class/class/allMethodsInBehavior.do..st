"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: enumeration
"
allMethodsInBehavior: behavior do: aBlock
	"Iterate over all methods defined in the given behavior, evaluating the block with each."

	behavior selectors
		do: [:selector | aBlock value: (behavior compiledMethodAt: selector)]