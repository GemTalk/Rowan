"
notice: Copyright 2013 GemTalk Systems, LLC. Open source under the MIT license.
category: initializing - private
"
fixupMissingCopyrightProperty

	self copyrightProperty isEmpty ifFalse: [^self].
	self initializeDefaultCopyrightProperty.
