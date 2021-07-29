set u SystemUser p swordfish
login

run
	"Script to be run in 3.2.15 prior to upgrade to 3.5.7 to 3.6.2"
	"Disown Rowan projects - all Rowan classes and methods will be
		updated as part of the 3.6.2 upgrade process"
	| projectNames |
	projectNames := #( 'Rowan' 'STON' 'Cypress' 'Tonel' ).
	projectNames do: [:projectName |
		Rowan projectTools disown
			disownProjectNamed: projectName ].
	
	"Disown extensions for kernel GemStone classes
		the extension methods will be re-installed as part of the
		3.6.2 upgrade process"
	projectNames := #( '...GemStoneExtensions' '...TestsGemStoneExtensions' ).
	projectNames do: [:projectName |
		(Rowan image loadedProjectNamed: projectName) 
				loadedPackages keys do: [:packageName |
					Rowan packageTools disown disownPackageNamed: packageName ] ].
System commit
%

logout
