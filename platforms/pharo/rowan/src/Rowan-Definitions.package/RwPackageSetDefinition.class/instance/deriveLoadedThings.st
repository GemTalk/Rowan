deriving
deriveLoadedThings

	^ RwEntitySet
		withAll:
			((self definitionNames
				collect: [ :packageName | Rowan image loadedPackageNamed: packageName ifAbsent: [  ] ])
				select: [ :each | each notNil ])
