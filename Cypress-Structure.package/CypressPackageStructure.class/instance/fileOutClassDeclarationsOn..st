filing out - private
fileOutClassDeclarationsOn: aStream

	self classesInFileInOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]