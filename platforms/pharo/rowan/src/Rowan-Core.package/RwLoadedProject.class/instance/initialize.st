initialization
initialize

	super initialize.
	loadedPackages := KeyValueDictionary new.
	isDirty := true. "a project is dirty if it has changes that are not written to disk"
