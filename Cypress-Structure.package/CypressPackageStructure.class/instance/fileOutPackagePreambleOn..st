filing out - private
fileOutPackagePreambleOn: aStream

	aStream
		nextPutAll: '! Package: ', self packageName; lf;
		nextPutAll: '! Written: ', DateAndTime now printString; lf;
		lf;
		lf;
		nextPutAll: '! Remove existing behavior from package ', self packageName; lf;
		nextPutAll: '!!!! This can be cleaned up when some package functionality is moved to the base system.'; lf;
		lf;
		nextPutAll: 'doit'; lf;
		nextPutAll: '| packageName |'; lf;
		nextPutAll: 'packageName := ', self packageName printString, '.'; lf;
		nextPutAll: 'System myUserProfile symbolList do: [:symDict |'; lf;
		nextPutAll: '	symDict do: [:possibleClass |'; lf;
		nextPutAll: '			| toRemove |'; lf;
		nextPutAll: '		possibleClass isBehavior ifTrue: ['; lf;
		nextPutAll: '			{possibleClass. possibleClass class} do: [:aClass |'; lf;
		nextPutAll: '				aClass category = packageName'; lf;
		nextPutAll: '					ifTrue: ['; lf;
		nextPutAll: '							"*anythingbutpackagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										(each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]])'; lf;
		nextPutAll: '										or: [each first ~= $*]]'; lf;
		nextPutAll: '					]'; lf;
		nextPutAll: '					ifFalse: ['; lf;
		nextPutAll: '							"*packagename[-anything]"'; lf;
		nextPutAll: '						toRemove := aClass categoryNames select: '; lf;
		nextPutAll: '										[:each |'; lf;
		nextPutAll: '										each first = $* and: [(each size = (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2])'; lf;
		nextPutAll: '														or: [each size > (packageName size + 1) and: [(each findStringNoCase: packageName startingAt: 2) = 2 and: [(each at: packageName size + 2) = $-]]]]]'; lf;
		nextPutAll: '					].'; lf;
		nextPutAll: '				toRemove do: [:each | aClass removeCategory: each].'; lf;
		nextPutAll: '			]'; lf;
		nextPutAll: '		]'; lf;
		nextPutAll: '	]'; lf;
		nextPutAll: '].'; lf;
		nextPutAll: '%'; lf;
		lf;
		lf
