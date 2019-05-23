private
_configurations: configDirPath forProject: aProjectName

	| urlBase |
	self configurationNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,configDirPath asFileReference pathString, '/'.
	^ self configurationNames
		collect: [ :configName | 
			| url |
			url := urlBase , configName , '.ston'.
			(RwAbstractProjectConfiguration fromUrl: url)
				projectName: aProjectName;
				yourself ]