"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutClassImplementationsOn: aStream

	self
		fileOut: 'Class Implementation'
		implementationsFrom: self classesInFileInOrder
		on: aStream
