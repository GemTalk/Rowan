"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: writing - private
"
fileOutExtensionImplementationsOn: aStream

	self
		fileOut: 'Class Extension'
		implementationsFrom: self extensions
		on: aStream
