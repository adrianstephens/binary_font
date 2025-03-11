
import * as fs from 'fs/promises';
import * as fontbin from '../dist';

(async() => {
	// Load a font file
	const font = await fontbin.loadFile('/System/Library/Fonts/Supplemental/Brush Script.ttf');

	if (font && font instanceof fontbin.Font) {

		// Access font properties
		console.log(font.numGlyphs());

		const mapping = font.getGlyphMapping();
		if (mapping) {
			const id = mapping['f'.charCodeAt(0)];
			const svg = font.getGlyphSVG(id);
			if (svg)
				await fs.writeFile('./glyph.svg', svg.toString());
		}
	}
})();
