"
notice: This work is protected by copyright. All rights reserved.
category: tests
"
test_gather_

	self
		assert: (#(1 2 3) gather: [:each | each * 10 + 1 to: each * 10 + each])
		equals: #(11 21 22 31 32 33)