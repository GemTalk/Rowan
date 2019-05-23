instance creation
method: methodSelector isMeta: isMeta inClassNamed: className isClassExtension: classExtension  intoPackageNamed: packageName


	^ (self classNamed: className isClassExtension: classExtension  intoPackageNamed: packageName)
		selector: methodSelector;
		isMetaclass: isMeta;
		yourself