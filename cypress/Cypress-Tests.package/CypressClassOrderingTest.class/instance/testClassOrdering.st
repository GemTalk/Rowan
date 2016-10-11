"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
testClassOrdering

	unorderedClasses := self someRandomClasses.
	orderedClasses := CypressObject new
				determineClassHierarchicalOrder: unorderedClasses.
	self assert: unorderedClasses size equals: orderedClasses size.
	orderedClasses do: [:aClass | self checkOrderingOf: aClass]