"
notice: This work is protected by copyright. All rights reserved.
category: filing out - private
"
fileOutExtensionImplementationsOn: aStream

	self
		fileOut: 'Class Extension'
		implementationsFrom: self extensions
		on: aStream
