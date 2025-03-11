# Binary Fonts

This package provides readers for various font formats, using the @isopodlabs/binary binary file loading library.

It supports reading type 1, TTF, SVG, bitmap and color glyphs.

Non bitmap glyphs can be converted to SVGs.


## Usage
Here's an example of how to use the binary_font package to read a font file:
```typescript
import { readFileSync } from 'fs';
import { Font } from 'binary_font';

// Load a font file
const fontData = readFileSync('path/to/font.ttf');

// Parse the font
const font = new Font(fontData);

// Access font properties
console.log(font.numGlyphs());

const mapping = font.getGlyphMapping();
const id = mapping['A'.charCodeAt(0)];
const glyph = font.getGlyph(id);
```

## Supported File Types
he binary_font package supports reading and parsing the following font formats:

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

## License

This project is licensed under the MIT License.