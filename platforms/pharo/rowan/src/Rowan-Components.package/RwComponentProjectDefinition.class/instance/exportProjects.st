exporting
exportProjects

	self projectNames do: [:projectName |
		self error: 'not yet implemented' ].
	self projectNames isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self projectsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]