"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_elementsIn_butNotIn_
	"Set theoretic difference means elements from the first collection not in the second."

	self
		assert: (CypressObject elementsIn: #(1 2 3) butNotIn: #()) equals: #(1 2 3);
		assert: (CypressObject elementsIn: #() butNotIn: #(1 2 3)) equals: #();
		assert: (CypressObject elementsIn: #(1 2 3) butNotIn: #(1 2 3)) equals: #();
		assert: (CypressObject elementsIn: #(1 2 3) butNotIn: #(2 3 4))
			equals: #(1);
		assert: (CypressObject elementsIn: #(1 2 3) butNotIn: #(3 4 5))
			equals: #(1 2);
		assert: (CypressObject elementsIn: #(1 2 3) butNotIn: #(4 5 6))
			equals: #(1 2 3)