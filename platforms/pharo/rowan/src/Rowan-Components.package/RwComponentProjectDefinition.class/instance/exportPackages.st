exporting
exportPackages
	"attempt to do incremental write to disk, however, if disk cannot be read, write all packages to disk"
	| projectSetDefinition |
	projectSetDefinition := [ (self class newForProjectReference: self projectRef) readProjectSet ]
		on: Error
		do: [:ignored | RwProjectSetDefinition new ].
	self exportPackages: projectSetDefinition.