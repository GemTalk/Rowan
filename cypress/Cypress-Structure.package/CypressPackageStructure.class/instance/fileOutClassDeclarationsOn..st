"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: filing out - private
"
fileOutClassDeclarationsOn: aStream

	self classesInFileInOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]