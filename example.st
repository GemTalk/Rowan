"Example of loading information about packages from the cypress repository and writing the 
 corresponding Topaz and chunk-format files in separate directories."

| packageManager repo topazRepo chunkRepo |
packageManager := CypressPackageManager2 new.
repo := CypressAbstractRepository
                        onUrl: 'cypress:/export/galbadia1/users/rsargent/git/CypressReferenceImplementation/cypress/' asUrl
                        alias: 'Reference Implementation'.
packageManager lookForPackagesInRepository: repo.
topazRepo := CypressAbstractRepository
                        onUrl: 'topaz:/export/galbadia1/users/rsargent/git/CypressReferenceImplementation/topaz/' asUrl
                        alias: 'Reference Implementation'.
chunkRepo := CypressAbstractRepository
                        onUrl: 'chunk:/export/galbadia1/users/rsargent/git/CypressReferenceImplementation/fileout/' asUrl
                        alias: 'Reference Implementation'.
packageManager packageInformationList
	do: [:each | 
		packageManager
			assignRepository: topazRepo to: each;
			assignRepository: chunkRepo to: each;
			writeChangesToAllRepositoriesFor: each].

