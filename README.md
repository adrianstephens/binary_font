# Binary Fonts

This package provides readers for various font formats, using the @isopodlabs/binary, and the @isopodlabs/xml libraries.

It supports reading type 1, TTF, SVG, bitmap and color glyphs.

Non bitmap glyphs can be converted to SVGs.


## Usage
Here's an example of how to use the binary_font package to read a font file and output an svg for the glyph representing 'A':
```typescript
import { readFileSync, writeFileSync } from 'fs';
import { fontLoad, Font } from '@isopodlabs/binary_font';

// Load a font file
const fontData = readFileSync('path/to/font.ttf');

// Parse the font
const font = load(fontData);
if (font && font instanceof Font) {

    // Access font properties
    console.log(font.numGlyphs());

    const mapping = font.getGlyphMapping();
	if (mapping) {
        const id = mapping['A'.charCodeAt(0)];
        const svg = font.getGlyphSVG(id);
		if (svg)
			writeFileSync('./glyph.svg', svg.toString());
    }
}

```

## Supported File Types
The binary_font package supports reading and parsing the following font formats:

- TrueType Fonts (TTF)
- TrueType Collections (TTC)
- OpenType Fonts (OTF)
- Web Open Font Format (WOFF)
- Web Open Font Format 2 (WOFF2)

It has support for the following data blocks:

- OS/2
- head
- hhea
- vhea
- maxp
- name
- cmap
- gasp
- sbix
- GSUB
- GPOS
- CPAL  Color Palette Table
- COLR  Color Table
- SVG   Scalable Vector Graphics
- CFF   Compact Font Format
- DSIG

## API

### Interfaces
```
interface Glyph {
    min: float2;
    max: float2;
    curve?: curveVertex[];
    refs?: glyphRef[];
    instructions?: Uint8Array;
}
```
A single glyph in the font.
```
interface GlyphImage {
    originOffset: float2;
    graphicType: string;
    data: Uint8Array;
}
```
A single glyph in a bitmap font.

### Font
The Font class provides methods to access font properties and glyph data.
#### Methods
- `numGlyphs(): number`

    Returns the number of glyphs in the font.
- `getGlyph(id: number): Glyph | undefined`

    Returns the glyph data for the specified glyph ID.
- `getGlyphMapping(): number[] | undefined`

    Returns the glyph mapping array.
- `getGlyphImages(ppem: number): GlyphImage[] | undefined`

    Returns the glyph images for the specified pixels per em (ppem).
- `getGlyphImage(id: number, ppem: number): GlyphImage | undefined`

    Returns the glyph images for the specified pixels per em (ppem).
- `getGlyphCOLR(id: number): Layer[] | undefined`

    Returns the COLR layers for the specified glyph ID.
- `getGlyphSVG(id: number, preferCOLR?: boolean): xml.Element | undefined`

    Returns the SVG representation of the specified glyph ID.
#### Properties
Each block found in the font exists as a property on the Font class. The properties are typed according to the specs used to read them.

### FontGroup
The FontGroup holds an array of Fonts read from, say, a TTC file
#### Methods
- `getSub(sub: string): Font | undefined`

    Returns the font with the matching name.

#### Properties
- `fonts: Font[]`

    The array of fonts.

### Functions
- `load(data: Uint8Array): Promise<Font | FontGroup | undefined>`

    loads the font data from the given data array.

## License

This project is licensed under the MIT License.