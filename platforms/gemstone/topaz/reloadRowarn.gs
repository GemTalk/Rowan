run
	| deprecationAction suite strm res projectNames warnings |
	deprecationAction := Deprecated deprecatedAction.
	warnings := {}.
	[
		projectNames := #( 'Rowan' 'STON' 'Cypress' 'Tonel' 'FileSystemGs' ).
		projectNames do: [:projectName |
			[			
				"make sure test group is loaded ... include deprecated packages"
				Rowan projectTools load
					loadProjectNamed: projectName
					withGroupNames: #('core' "'tests'" 'deprecated' 'jadeServer' 'deprecated tests') ]

 		warnings isEmpty ifFalse: [
			GsFile gciLogServer: 'COMPILE WARNINGS: '.
			warnings do: [:warning | GsFile gciLogServer: '	', warning ]. 
			self error: 'Warnings during project load' ]. 

		System commit. 
  ]]
%
