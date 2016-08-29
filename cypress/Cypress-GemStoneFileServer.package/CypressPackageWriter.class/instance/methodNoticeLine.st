"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: accessing - private
"
methodNoticeLine

	^self packageStructure properties
		at: 'copyrightLine'
		ifAbsent: [self repository copyrightProperty]