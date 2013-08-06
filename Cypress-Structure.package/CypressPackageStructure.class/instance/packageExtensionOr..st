"
notice: This work is protected by copyright. All rights reserved.
category: accessing
"
packageExtensionOr: aBlock

	^self properties at: 'extension' ifAbsent: aBlock
