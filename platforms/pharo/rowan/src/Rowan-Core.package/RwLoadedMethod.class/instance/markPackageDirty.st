private
markPackageDirty
	"LoadedClass may still be nil if we're creating a new LoadedMethod."

	loadedClass ifNotNil: [loadedClass markPackageDirty]