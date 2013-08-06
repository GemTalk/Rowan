"
notice: This work is protected by copyright. All rights reserved.
category: accessing - private
"
methodNoticeLine

	^self packageStructure properties
		at: 'copyrightLine'
		ifAbsent: [self repository copyrightProperty]