running
setUp
	super setUp.
	filesystem := self _createFileSystem.
	toDelete := OrderedCollection new.
	projectsToDelete := OrderedCollection new.