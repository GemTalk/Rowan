"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
packageName

	| extension extensionSize stopIndex |
	extension := self packageExtension.
	extensionSize := extension size.
	stopIndex := self name
				indexOfSubCollection: extension
				startingAt: self name size - extensionSize + 1
				ifAbsent: [self name size + 1].
	^self name copyFrom: 1 to: stopIndex - 1
