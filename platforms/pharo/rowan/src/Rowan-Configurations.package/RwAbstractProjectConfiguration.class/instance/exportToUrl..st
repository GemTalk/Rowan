exporting
exportToUrl: fileUrl

	^ self copy
		initializeForExport
		_exportToUrl: fileUrl