"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutClassesOn: aStream

	self
		fileOutClassesPreambleOn: aStream;
		fileOutClassDeclarationsOn: aStream;
		fileOutClassImplementationsOn: aStream