"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutClassDeclarationsOn: aStream

	self classesInFileInOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]