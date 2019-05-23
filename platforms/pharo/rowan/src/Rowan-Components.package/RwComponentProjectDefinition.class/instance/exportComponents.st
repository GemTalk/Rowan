exporting
exportComponents

	self components values do: [:component|
		component exportToUrl: 'file:',  self configsRoot pathString, '/' ].
	self components isEmpty
		ifTrue: [
			"add README.md as placeholder to ensure that the directory is preserved by git"
			(self configsRoot /  'README', 'md') writeStreamDo: [ :fileStream | ] ]