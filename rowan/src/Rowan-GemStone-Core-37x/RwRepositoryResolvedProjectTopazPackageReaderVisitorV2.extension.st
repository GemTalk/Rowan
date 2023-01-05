Extension { #name : 'RwRepositoryResolvedProjectTopazPackageReaderVisitorV2' }

{ #category : '*rowan-gemstone-core-37x' }
RwRepositoryResolvedProjectTopazPackageReaderVisitorV2 >> gsFileinPackagerClass [
	"acessor method needed, because GsFileinPackager class cannot be filed into a 3.6.x image that is about to be upgraded until the upgrade is complete. So it is worth a bit of schizophrenic packaging to smooth over the rough patch."
	"https://github.com/GemTalk/Rowan/issues/820"
	^ GsFileinPackager
]
