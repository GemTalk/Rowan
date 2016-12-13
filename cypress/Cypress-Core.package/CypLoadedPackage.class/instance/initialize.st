"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initialize

	super initialize.
	loadedClasses := Set new.
	loadedExtendedClasses := Set new.
	isDirty := false