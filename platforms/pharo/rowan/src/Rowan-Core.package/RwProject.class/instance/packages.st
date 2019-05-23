accessing
packages

	^ self packageNames collect: [ :packageName | RwPackage newNamed: packageName ]