"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: enumeration
"
allDoSafely: aBlock
	"Evaluate aBlock for every registered LoadedClass in the system. 
	The block may modify the ClassRegistry, so we must copy the values first."

	ClassRegistry values do: aBlock