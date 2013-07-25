filing out - private
fileOutClassDeclarationsOn: aStream

	self classes
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]