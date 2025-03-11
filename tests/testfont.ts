
import { readFileSync, writeFileSync} from 'fs';
import {fontLoad} from '../dist/load';
import {Font} from '../dist/font';

(async() => {
	// Load a font file
	const fontData = readFileSync('/System/Library/Fonts/Supplemental/Brush Script.ttf');

	// Parse the font
	const font = await fontLoad(fontData);
	if (font && font instanceof Font) {

		// Access font properties
		console.log(font.numGlyphs());

		const mapping = font.getGlyphMapping();
		if (mapping) {
			const id = mapping['A'.charCodeAt(0)];
			const svg = font.getGlyphSVG(id);
			if (svg) {
				writeFileSync('./glyph.svg', svg.toString());
			}
		}
	}
})();
