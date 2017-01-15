"
notice: Copyright 2013,2016 GemTalk Systems, LLC. Open source under the MIT license.
category: private
"
markPackageDirty
	"LoadedClass may still be nil if we're creating a new LoadedMethod."

	loadedClass ifNotNil: [loadedClass markPackageDirty]