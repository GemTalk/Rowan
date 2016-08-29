"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutClassesOn: aStream

	self
		fileOutClassesPreambleOn: aStream;
		fileOutClassDeclarationsOn: aStream;
		fileOutClassImplementationsOn: aStream