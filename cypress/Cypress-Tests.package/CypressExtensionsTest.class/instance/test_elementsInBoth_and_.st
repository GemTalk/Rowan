"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_elementsInBoth_and_

	self
		assert: (CypressObject elementsInBoth: #(1 2 3) and: #()) equals: #();
		assert: (CypressObject elementsInBoth: #() and: #(1 2 3)) equals: #();
		assert: (CypressObject elementsInBoth: #(1 2 3) and: #(1 2 3))
			equals: #(1 2 3);
		assert: (CypressObject elementsInBoth: #(1 2 3) and: #(2 3 4))
			equals: #(2 3);
		assert: (CypressObject elementsInBoth: #(1 2 3) and: #(3 4 5)) equals: #(3);
		assert: (CypressObject elementsInBoth: #(1 2 3) and: #(4 5 6)) equals: #()