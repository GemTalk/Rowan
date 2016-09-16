"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing
"
initializeDefaultCopyrightNotice

	self defaultCopyrightNotice isNil ifFalse: [^self].
	self defaultCopyrightNotice: 'This work is protected by copyright. All rights reserved.'
