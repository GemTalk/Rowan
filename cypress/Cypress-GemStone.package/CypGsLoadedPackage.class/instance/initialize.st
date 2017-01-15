"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: initialization
"
initialize

	super initialize.
	loadedClasses := KeyValueDictionary new.
	loadedClassExtensions := KeyValueDictionary new.
	"A new package is dirty until it has been shown to be identical to a repository."
	isDirty := true