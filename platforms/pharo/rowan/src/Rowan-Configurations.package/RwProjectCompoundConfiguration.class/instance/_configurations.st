private
_configurations

	| spec urlBase |
	self configurationNames isEmpty ifTrue: [ ^ #() ].
	spec := self _specification.
	urlBase := 'file:' , (spec repositoryRootPath asFileReference / spec configsPath) pathString, '/'.
	^ self configurationNames
		collect: [ :configName | 
			| url |
			url := urlBase , configName , '.ston'.
			(RwAbstractProjectConfiguration fromUrl: url)
				projectName: self projectName;
				yourself ]