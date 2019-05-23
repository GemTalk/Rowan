accessing
configsPath

	^ configsPath ifNil: [ configsPath := 'rowan/configs' ]
