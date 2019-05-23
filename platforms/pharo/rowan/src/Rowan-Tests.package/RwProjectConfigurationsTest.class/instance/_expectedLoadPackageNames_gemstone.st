private
_expectedLoadPackageNames_gemstone

	^ (self _expectedLoadPackageNames_common , 
			#('Rowan-Services-Extensions' 'Rowan-Services-Core' 'Rowan-GemStone-Core' 
					'Rowan-GemStone-Kernel' 'Rowan-GemStone-Loader' 'Rowan-GemStone-Components'
					'Rowan-GemStone-Components-Extensions'
					'Rowan-GemStone-Loader-Extensions' 'Rowan-Services-Tests') ) sort