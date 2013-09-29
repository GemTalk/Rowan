"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing
"
packageExtensionOr: aBlock

	^self properties at: 'extension' ifAbsent: aBlock
