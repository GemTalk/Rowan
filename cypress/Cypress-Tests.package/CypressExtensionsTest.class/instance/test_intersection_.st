"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: tests
"
test_intersection_

	self
		assert: (#(1 2 3) intersection: #()) sortAscending equals: #();
		assert: (#() intersection: #(1 2 3)) sortAscending equals: #();
		assert: (#(1 2 3) intersection: #(1 2 3)) sortAscending equals: #(1 2 3);
		assert: (#(1 2 3) intersection: #(2 3 4)) sortAscending equals: #(2 3);
		assert: (#(1 2 3) intersection: #(3 4 5)) sortAscending equals: #(3);
		assert: (#(1 2 3) intersection: #(4 5 6)) sortAscending equals: #()
