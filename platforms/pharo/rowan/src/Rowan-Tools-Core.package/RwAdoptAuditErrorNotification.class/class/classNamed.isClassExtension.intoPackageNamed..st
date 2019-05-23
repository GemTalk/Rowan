instance creation
classNamed: className isClassExtension: classExtension  intoPackageNamed: packageName

	^ self new
		className: className;
		isClassExtension: classExtension;
		packageName: packageName;
		yourself