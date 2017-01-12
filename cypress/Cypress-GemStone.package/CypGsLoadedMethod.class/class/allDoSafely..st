"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: enumeration
"
allDoSafely: aBlock
	"Evaluate aBlock for every registered LoadedMethod in the system. 
	The block may modify the MethodRegistry, so we must copy the values first."

	MethodRegistry values do: aBlock