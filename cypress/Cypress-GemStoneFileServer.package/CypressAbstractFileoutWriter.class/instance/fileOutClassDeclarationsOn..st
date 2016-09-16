"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassDeclarationsOn: aStream

	self classesInDependencyOrder
		do: [:classStructure | self fileOutClassDeclaration: classStructure on: aStream]