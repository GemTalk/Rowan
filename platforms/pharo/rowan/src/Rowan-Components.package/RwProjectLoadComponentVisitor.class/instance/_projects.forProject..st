private
_projects: projectDirPath forProject: ignored

	| urlBase |
	self projectNames isEmpty ifTrue: [ ^ #() ].
	urlBase := 'file:' ,projectDirPath asFileReference pathString, '/'.
	^ self projectNames
		collect: [ :prjName | 
			| url |
			url := urlBase , prjName , '.ston'.
			(RwSpecification fromUrl: url)
				projectName: prjName;
				yourself ]