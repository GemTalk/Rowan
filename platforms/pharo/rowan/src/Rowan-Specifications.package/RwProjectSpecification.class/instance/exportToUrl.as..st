exporting
exportToUrl: fileUrl as: fileName

	^ self copy
		initializeForExport
		_exportToUrl: fileUrl as: fileName