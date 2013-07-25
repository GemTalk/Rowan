filing out - private
fileOutClassImplementationsOn: aStream

	self
		fileOut: 'Class Implementation'
		implementationsFrom: self classes
		on: aStream
