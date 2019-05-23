accessing
useGit

	^ self properties at: 'useGit' ifAbsent: [ false ]