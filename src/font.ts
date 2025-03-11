import * as binary from '@isopodlabs/binary';
import * as xml from '@isopodlabs/xml';
import {float2, float2x3, identity2x3, extent2} from './vector';
import {color, CURVE, curveVertex, curveExtent, parseCurve, FILL, EXTEND, Fill, Layer, parseSVGpath, makeSVGPath} from './curves';

import {CFF} from "./cff";
import {CPAL, COLR} from "./colr";

function makeClass<I>() {
	return (class {} as new () => I);
}

export function lowerBound<T>(array: T[], func: (a: T, i: number) => boolean) {
	let i = 0;
	for (let n = array.length; n; n >>= 1) {
		const mid = i + (n >> 1);
		if (func(array[mid], mid)) {
			i = mid + 1;
			--n;
		}
	}
	return i;
}

function argmin<T>(array: T[], fn?:(i: T) => number) {
	let mini	= 0;
	let minv	= fn ? fn(array[0]) : array[0];
	for (let i = 1; i < array.length; i++) {
		const v = fn ? fn(array[i]) : array[i];
		if (v < minv) {
			mini = i;
			minv = v;
		}
	}
	return mini;
}

function arrayReverse<T>(array: T[], start: number, end: number): void {
    while (start < end) {
        [array[start], array[end]] = [array[end], array[start]];
        start++;
        end--;
    }
}

function arrayRotate<T>(array: T[], start: number, end: number, shift: number): void {
    const length = end - start;
    if (length > 1 && shift % length) {
		shift = ((shift % length) + length) % length;
		arrayReverse(array, start, end - 1);
		arrayReverse(array, start, start + shift - 1);
		arrayReverse(array, start + shift, end - 1);
	}
}

function as<T>(type: binary.TypeT<T>) {
	return binary.as(type, i => i as T);
}

//-----------------------------------------------------------------------------
//	TTF types
//-----------------------------------------------------------------------------

const timeAdjust = new Date('1970-01-01T00:00:00Z').getTime() - new Date('1904-01-01T00:00:00Z').getTime();

const datetime64 = binary.as(binary.INT64_BE, i => new Date(Number(i) * 1000 - timeAdjust));
const TAG		= binary.StringType(4);
const u8 		= binary.UINT8;
const u16 		= binary.UINT16_BE;
const s16 		= binary.INT16_BE;
const u32 		= binary.UINT32_BE;
const u24		= binary.UINT(24, true);

const fixed16	= binary.asFixed(u16, 14);
const fixed8	= binary.asFixed(u8, 6);

export function vec2(type: binary.TypeT<number>): binary.TypeT<float2> {
	return binary.as({x: type, y: type}, v => float2(v.x, v.y));
}

const enum PLATFORM {
	UNICODE				= 0,	//Various
	MACINTOSH			= 1,	//Script manager code
	ISO					= 2,	//ISO encoding [deprecated]
	WINDOWS				= 3,	//Windows encoding
	CUSTOM				= 4,	//Custom
};

const enum ENCODING {
	UNI_default			= 0,	//deprecated
	UNI_1_1				= 1,	//deprecated
	UNI_iso_10646		= 2,	//deprecated
	UNI_2_bmp			= 3,	//Unicode BMP only
	UNI_2_full			= 4,	//Unicode full repertoire
	UNI_variation		= 5,	//for use with subtable format 14
	UNI_full			= 6,	//for use with subtable format 13

	MAC_Roman			= 0,
	MAC_Japanese		= 1,
	MAC_TradChinese		= 2,
	MAC_Korean			= 3,
	MAC_Arabic			= 4,
	MAC_Hebrew			= 5,
	MAC_Greek			= 6,
	MAC_Russian			= 7,
	MAC_RSymbol			= 8,
	MAC_Devanagari		= 9,
	MAC_Gurmukhi		= 10,
	MAC_Gujarati		= 11,
	MAC_Oriya			= 12,
	MAC_Bengali			= 13,
	MAC_Tamil			= 14,
	MAC_Telugu			= 15,
	MAC_Kannada			= 16,
	MAC_Malayalam		= 17,
	MAC_Sinhalese		= 18,
	MAC_Burmese			= 19,
	MAC_Khmer			= 20,
	MAC_Thai			= 21,
	MAC_Laotian			= 22,
	MAC_Georgian		= 23,
	MAC_Armenian		= 24,
	MAC_SimpChinese		= 25,
	MAC_Tibetan			= 26,
	MAC_Mongolian		= 27,
	MAC_Geez			= 28,
	MAC_Slavic			= 29,
	MAC_Vietnamese		= 30,
	MAC_Sindhi			= 31,
	MAC_Uninterpreted	= 32,

	ISO_ASCII7			= 0,
	ISO_10646			= 1,
	ISO_8859_1			= 2,

	WIN_Symbol			= 0,
	WIN_UCS2			= 1,
	WIN_ShiftJIS		= 2,
	WIN_PRC				= 3,
	WIN_Big5			= 4,
	WIN_Wansung			= 5,
	WIN_Johab			= 6,
	WIN_Reserved7		= 7,
	WIN_Reserved8		= 8,
	WIN_Reserved9		= 9,
	WIN_UCS4			= 10,
};

//-----------------------------------------------------------------------------
//	OS/2
//-----------------------------------------------------------------------------

const PANOSE = {
	bFamilyType:		u8,
	bSerifStyle:		u8,
	bWeight:			u8,
	bProportion:		u8,
	bContrast:			u8,
	bStrokeVariation:	u8,
	bArmStyle:			u8,
	bLetterform:		u8,
	bMidline:			u8,
	bXHeight:			u8,
};

const OS2 = {
	version:			u16,			//0x0004
	avg_char_width:		s16,
	weight_class:		u16,
	width_class:		u16,
	type:				u16,
	subscript_size:		vec2(s16),
	subscript_offset:	vec2(s16),
	superscript_size:	vec2(s16),
	superscript_offset:	vec2(s16),
	strikeout_size:		s16,
	strikeout_position:	s16,
	family_class:		s16,
	panose:				PANOSE,
	unicode_range:		binary.ArrayType(4, u32),
	vend_id:			binary.StringType(4),
	selection:			u16,
	first_char_index:	u16,
	last_char_index:	u16,
	//v0
	typo_ascender:		s16,
	typo_descender:		s16,
	typo_line_gap:		s16,
	win_ascent:			u16,
	win_descent:		u16,
	//v1
	_: binary.If(obj=>obj.version >= 1, {
		codepage_range:		binary.ArrayType(2, u32),
	//v2,3,4
		_: binary.If(obj=>obj.version >= 2, {
			height:				s16,
			cap_height:			s16,
			default_char:		u16,
			break_char:			u16,
			max_context:		u16,
	//v5
			_: binary.If(obj=>obj.version >= 5, {
				LowerOpticalPointSize:	u16,
				UpperOpticalPointSize:	u16,
			})
		})
	})
};

//-----------------------------------------------------------------------------
//	head
//-----------------------------------------------------------------------------

enum headFLAGS {
	ybaseline 		= 1 << 0,// - y value of 0 specifies baseline
	xlsb			= 1 << 1,// - x position of left most black bit is LSB
	scaled_differ	= 1 << 2,// - scaled point size and actual point size will differ (i.e. 24 point glyph differs from 12 point glyph scaled by factor of 2)
	int_scale		= 1 << 3,// - use integer scaling instead of fractional
	ms0				= 1 << 4,// - (used by the Microsoft implementation of the TrueType scaler)
	vertical		= 1 << 5,// - This bit should be set in fonts that are intended to e laid out vertically, and in which the glyphs have been drawn such that an x-coordinate of 0 corresponds to the desired vertical baseline.
//			= 1 << 6,// - This bit must be set to zero.
	arabic	= 1 << 7,// - This bit should be set if the font requires layout for correct linguistic rendering (e.g. Arabic fonts).
	metamorphosis	= 1 << 8,// - This bit should be set for a GX font which has one or more metamorphosis effects designated as happening by default.
	strong_rl		= 1 << 9,// - This bit should be set if the font contains any strong right-to-left glyphs.
	indic			= 1 << 10,// - This bit should be set if the font contains Indic-style rearrangement effects.
//	= 1 << 11,// - Defined by Adobe.
//	= 1 << 12,// - Defined by Adobe.
};
enum headSTYLE {
	bold		= 1 << 0,
	italic		= 1 << 1,
	underline	= 1 << 2,
	outline		= 1 << 3,
	shadow		= 1 << 4,
	condensed	= 1 << 5,
	extended	= 1 << 6,
};
enum headDIRECTION {
	LEFTRIGHT_STRONG	= 1,	// Only strongly left to right glyphs
	LEFTRIGHT			= 2,	// Like 1 but also contains neutrals
	RIGHTLEFT_STRONG	= -1,	// Only strongly right to left glyphs
	RIGHTLEFT			= -2,	// Like -1 but also contains neutrals
};

const head = {
	version:			u32,		//0x00010000 if (version 1.0)
	font_revision:		u32,		//set by font manufacturer
	checksum_adj:		u32,		//To compute: set it to 0, calculate the checksum for the 'head' table and put it in the table directory, sum the entire font as u32, then store B1B0AFBA - sum. The checksum for the 'head' table will not be wrong. That is OK.
	magic:				u32,		//set to 0x5F0F3CF5
	flags:				as<headFLAGS>(u16),
	units_per_em:		u16,		//range from 64 to 16384
	created:			datetime64,	//international date
	modified:			datetime64,	//international date
	//for all glyph bounding boxes
	min:				vec2(s16),
	max:				vec2(s16),
	macStyle:			as<headSTYLE>(u16),
	lowestRecPPEM:		u16,		//smallest readable size in pixels
	fontDirectionHint:	as<headDIRECTION>(s16),		//0 Mixed directional glyphs
	indexToLocFormat:	s16,		//0 for short offsets, 1 for long
	glyphDataFormat:	s16,		//0 for current format
};

export function loadLocs(loca: Uint8Array, glyf: Uint8Array, indexToLocFormat: number) {
	const locs = indexToLocFormat === 0
		? [...binary.utils.to16(loca, true)].map(i => i * 2)
		: Array.from(binary.utils.to32(loca, true));

	return locs.slice(0, -1).map((loc: number, i: number) => glyf.subarray(loc, locs[i + 1]));
}

//-----------------------------------------------------------------------------
//	hhea	Horizontal Header Table	+ hmtx
//	vhea	Vertical Header Table	+ vmtx
//-----------------------------------------------------------------------------

const MetricsHead = {
	version:				u32,	//0x00010000 (1.0)
	ascent:					s16,	//Distance from baseline of highest ascender
	descent:				s16,	//Distance from baseline of lowest descender
	lineGap:				s16,	//typographic line gap
	advanceMax:				u16,	//must be consistent with horizontal metrics
	minASideBearing:		s16,	//must be consistent with horizontal metrics
	minBSideBearing:		s16,	//must be consistent with horizontal metrics
	maxExtent:				s16,	//max(lsb + (xMax-xMin))
	caretSlopeRise:			s16,	//used to calculate the slope of the caret (rise/run) set to 1 for vertical caret
	caretSlopeRun:			s16,	//0 for vertical
	caretOffset:			s16,	//set value to 0 for non-slanted fonts
	reserved:				binary.ArrayType(4, s16),			//set value to 0
	metricDataFormat:		s16,	//0 for current format
	numOfLongMetrics:		u16,	//number of advance widths in metrics table
};

interface Metrics {
	metrics: {
		advance:	number,
		bearing:	number,
	}[],
	bearing: 		number[],
};

export function loadMetrics(data: Uint8Array, numGlyphs: number, numMetrics: number): Metrics {
	const	hfile	= new binary.stream(data);
	return {
		metrics:	binary.readn(hfile, {advance: u16, bearing: s16}, numMetrics),
		bearing:	binary.readn(hfile, s16, numGlyphs - numMetrics)
	};
}

//-----------------------------------------------------------------------------
//	maxp	Maximum Profile
//-----------------------------------------------------------------------------

const maxp = {
	version:				u32,	//0x00010000 (1.0)
	numGlyphs:				u16,	//the number of glyphs in the font
	_: binary.If(obj=>obj.version >= 0x00010000, {
		maxPoints:				u16,	//points in non-compound glyph
		maxContours:			u16,	//contours in non-compound glyph
		maxComponentPoints:		u16,	//points in compound glyph
		maxComponentContours:	u16,	//contours in compound glyph
		maxZones:				u16,	//set to 2
		maxTwilightPoints:		u16,	//points used in Twilight Zone (Z0)
		maxStorage:				u16,	//number of Storage Area locations
		maxFunctionDefs:		u16,	//number of FDEFs
		maxInstructionDefs:		u16,	//number of IDEFs
		maxStackElements:		u16,	//maximum stack depth
		maxSizeOfInstructions:	u16,	//byte count for glyph instructions
		maxComponentElements:	u16,	//number of glyphs referenced at top level
		maxComponentDepth:		u16,	//levels of recursion, set to 0 if font has only simple glyphs
	}),
};

//-----------------------------------------------------------------------------
//	name	Naming Table
//-----------------------------------------------------------------------------

const enum ID {
	ID_COPYRIGHT		= 0,	//Copyright notice.
	ID_FAMILY			= 1,	//Font Family. This string is the font family name the user sees on Macintosh platforms.
	ID_SUBFAMILY		= 2,	//Font Subfamily. This string is the font family the user sees on Macintosh platforms.
	ID_IDENTIFICATION	= 3,	//Unique subfamily identification.
	ID_FULLNAME			= 4,	//Full name of the font.
	ID_VERSION			= 5,	//Version of the name table.
	ID_PS_NAME			= 6,	//PostScript name of the font. Note: A font may have only one PostScript name and that name must be ASCII.
	ID_TRADEMARK		= 7,	//Trademark notice.
	ID_MANUFACTURER		= 8,	//Manufacturer name.
	ID_DESIGNER			= 9,	//Designer; name of the designer of the typeface.
	ID_DESCRIPTION		= 10,	//Description; description of the typeface. Can contain revision information, usage recommendations, history, features, and so on.
	ID_VENDOR_URL		= 11,	//URL of the font vendor (with procotol, e.g., http://, ftp://). If a unique serial number is embedded in the URL, it can be used to register the font.
	ID_DESIGNER_URL		= 12,	//URL of the font designer (with protocol, e.g., http://, ftp://)
	ID_LICENSE			= 13,	//License description; description of how the font may be legally used, or different example scenarios for licensed use. This field should be written in plain language, not legalese.
	ID_LICENSE_URL		= 14,	//License information URL, where additional licensing information can be found.
	ID_RESERVED			= 15,	//Reserved
	ID_PREF_FAMILY		= 16,	//Preferred Family (Windows only); In Windows, the Family name is displayed in the font menu; the Subfamily name is presented as the Style name. For historical reasons, font families have contained a maximum of four styles, but font designers may group more than four fonts to a single family. The Preferred Family and Preferred Subfamily IDs allow font designers to include the preferred family/subfamily groupings. These IDs are only present if they are different from IDs 1 and 2.
	ID_PREF_SUBFAMILY	= 17,	//Preferred Subfamily (Windows only); In Windows, the Family name is displayed in the font menu; the Subfamily name is presented as the Style name. For historical reasons, font families have contained a maximum of four styles, but font designers may group more than four fonts to a single family. The Preferred Family and Preferred Subfamily IDs allow font designers to include the preferred family/subfamily groupings. These IDs are only present if they are different from IDs 1 and 2.
	ID_COMPATIBLE		= 18,	//Compatible Full (Macintosh only); On the Macintosh, the menu name is constructed using the FOND resource. This usually matches the Full Name. If you want the name of the font to appear differently than the Full Name, you can insert the Compatible Full Name in ID 18. This name is not used by the Mac OS itself, but may be used by application developers (e.g., Adobe).
	ID_SAMPLE			= 19,	//Sample text. This can be the font name, or any other text that the designer thinks is the best sample text to show what the font looks like.
	//20 - 255	Reserved for future expansion.
	ID_FONT_SPECIFIC	= 256,	// 256 - 32767	Font-specific names (layout features and settings, variations, track names, etc.)
};

const NameRecord = {
	platformID:		as<PLATFORM>(u16),	//Platform identifier code.
	encodingID:		as<ENCODING>(u16),	//Platform-specific encoding identifier.
	languageID:		u16,	//Language identifier.
	nameID:			as<ID>(u16),	//Name identifiers.
	length:			u16,	//Name string length in bytes.
	offset:			u16,	//Name string offset in bytes from stringOffset.
};

class name extends binary.ReadClass({
	format:			u16,	//Format selector. Set to 0.
	count:			u16,	//The number of nameRecords in this name table.
	stringOffset:	u16,	//Offset in bytes to the beginning of the name character strings.
}) {
	names:	string[] = [];

	constructor(file: binary.stream) {
		super(file);
		const names	= binary.readn(file, NameRecord, this.count);
		for (const r of names) {
			const i = r.nameID;
			switch (r.platformID) {
				case PLATFORM.UNICODE:
					break;
				case PLATFORM.MACINTOSH:
					this.names[i] = binary.utils.decodeText(file.buffer_at(this.stringOffset + r.offset, r.length));
					break;
				case PLATFORM.WINDOWS:
					switch (r.encodingID) {
						case ENCODING.WIN_UCS2:
							this.names[i] = binary.utils.decodeText(file.buffer_at(this.stringOffset + r.offset, r.length), 'utf16be');
							break;
					};
					break;
			}
		}
	}
}

//-----------------------------------------------------------------------------
//	cmap	Character to Glyph Index Mapping Table
//-----------------------------------------------------------------------------

const cmapGroup = {
	start: u32, end: u32, glyph: u32
};

interface cmapTable {
	map(): number[];
}

const cmap_format_header1 = {
	length:		u16,	//Length in bytes of the subtable (set to 262 for format 0)
	language:	u16,	//Language code for this encoding subtable, or zero if language-independent
};

const cmap_format_header2 = {
	format_low:	u16,	//Subtable format; set to 8.0,10.0,12.0
	length:		u32,	//Byte length of this subtable (including the header)
	language:	u32,	//Language code for this encoding subtable, or zero if language-independent
};

class cmapByteEncoding extends binary.Class({...cmap_format_header1,
	glyphs:	binary.ArrayType(256, u8),	//An array that maps character codes to glyph index values
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
		return this.glyphs;
	}
}

class cmapHighByteTable extends binary.Class({...cmap_format_header1,
	subHeaderKeys:	binary.ArrayType(256, u16),		//Array that maps high bytes to subHeaders: value is index * 8
	data:		binary.Remainder,
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
	//subHeaders:		binary.RemainingArrayType({
	//	start:	u16,
	//	count:	u16,
	//	delta:	s16,
	//	range:	binary.OffsetType(u16,u16),
	//})
		const map: number[] = [];
		for (let i = 0; i < 256; i++) {
			const	offset	= this.subHeaderKeys[i] + 256 * 2;
			const	sub		= binary.read(new binary.stream(this.data.slice(offset)), {
				start:	u16,
				count:	u16,
				delta:	s16,
				range:	u16,
			});
			const	coffset	= i * 256 + sub.start;
			const	glyphs	= binary.utils.to16(this.data.slice(offset + sub.range, sub.count * 2), true);
			for (let c = 0; c < sub.count; c++)
				map[c + coffset] = (glyphs[c] + sub.delta) & 0xffff;
		}
		return map;
	}
};

class cmapSegmentDelta extends binary.Class({...cmap_format_header1,
	segCountX2: 	u16,			//2 * segCount
	searchRange: 	u16,			//2 * (2**FLOOR(log2(segCount)))
	entrySelector: 	u16,			//log2(searchRange/2)
	rangeShift: 	u16,			//(2 * segCount) - searchRange
	endCode: 		binary.RemainingArrayType(u16),			//Ending character code for each segment, last = 0xFFFF.
}) implements cmapTable {
/*
	u16	endCode[segCount];			//Ending character code for each segment, last = 0xFFFF.
	u16	reservedPad;				//This value should be zero
	u16	startCode[segCount];		//Starting character code for each segment
	u16	idDelta[segCount];			//Delta for all character codes in segment
	u16	idRangeOffset[segCount];	//Offset in bytes to glyph indexArray, or 0
	u16	glyphIndexArray[variable];	//Glyph index array
*/
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		const count = this.segCountX2 / 2;
		const ends	= this.endCode;
		for (let i = 0; i < count; i++) {
			const r		= i + 1 + count * 3;
			const end	= ends[i];
			const start = ends[i + 1 + count * 1];
			const delta	= ends[i + 1 + count * 2];

			if (ends[r]) {
				const glyphs = ends.slice(r + ends[r] / 2);
				for (let c = start; c <= end; c++)
					map[c] = glyphs[c - start];
			} else {
				for (let c = start; c <= end; c++)
					map[c] = (c + delta) & 0xffff;
			}
		}
		return map;
	}
};

class cmapTrimmedMapping extends binary.Class({...cmap_format_header1,
	start:			u16,	//First character code of subrange
	glyphs:			binary.ArrayType(u16, u16),	//Array of glyph index values for character codes in the range
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		this.glyphs.forEach((i, x) => map[x + this.start] = i);
		return map;
	}
}

class cmapMixed1632 extends binary.Class({...cmap_format_header2,
	is32:		binary.ArrayType(8192, u8),			//Tightly packed array of bits (8K bytes total) indicating whether the particular 16-bit (index) value is the start of a 32-bit character code
	groups:		binary.ArrayType(u32, cmapGroup),
}) implements cmapTable {
	//bool		is32bit(uint16 c)	const	{ return is32[c / 8] & (1 << (~c & 7)); }
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		return map;
	}
}

class cmapTrimmedArray extends binary.Class({...cmap_format_header2,
	start:	u32,	//First character code covered
	glyphs:	binary.ArrayType(u32, u16),	//Array of glyph indices for the character codes covered
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		this.glyphs.forEach((i, x) => map[x + this.start] = i);
		return map;
	}
}

class cmapSegmented extends binary.Class({...cmap_format_header2,
	groups:	binary.ArrayType(u32, cmapGroup),
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		for (const g of this.groups) {
			const	offset = g.glyph - g.start;
			for (let c = g.start; c <= g.end; c++)
				map[c] = c + offset;
		}
		return map;
	}
}

class cmapManyToOne extends binary.Class({...cmap_format_header2,
	groups:	binary.ArrayType(u32, cmapGroup),
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		for (const g of this.groups) {
			for (let c = g.start; c <= g.end; c++)
				map[c] = g.glyph;
		}
		return map;
	}
}

class cmapVariationSeq extends binary.Class({
	length:	u32,
	variations:	binary.ArrayType(u32, {
		selector:	u24,
		default_uvs:		binary.OffsetType(u32, {
			ranges:		binary.ArrayType(u32, binary.as(u32, binary.BitFields({startUnicodeValue: 24, additionalCount: 8})))
		}),
		non_default_uvs:	binary.OffsetType(u32, {
			mappings:	binary.ArrayType(u32, {unicodeValue: u24, glyphID: u16})
		}),
	}),
}) implements cmapTable {
	//static get(file: binary._stream) { return new this(file); }
	map() {
		const map: number[] = [];
		return map;
	}
}

const cmap = {
	ver:	u16,	//0
	tables:	binary.ArrayType(u16, {
		platform:	u16,
		encoding:	u16,
		data:		as<cmapTable>(binary.OffsetType(u32, binary.Switch(u16, {
			0: 	cmapByteEncoding,
			2: 	cmapHighByteTable,
			4: 	cmapSegmentDelta,
			6: 	cmapTrimmedMapping,
			8: 	cmapMixed1632,
			10:	cmapTrimmedArray,
			12:	cmapSegmented,
			13:	cmapManyToOne,
			14:	cmapVariationSeq,
		}))),
	})
};

//-----------------------------------------------------------------------------
//	gasp	Grid-fitting and Scan-conversion Procedure Table
//-----------------------------------------------------------------------------

enum gaspBehavior {
	grid_fit	= 1,	//Use gridfitting
	do_gray		= 2,	//Use grayscale rendering
};

const gasp = {
	version:	u16,	//Version number (set to 0)
	entries:	binary.ArrayType(u16, {	//Sorted by ppem
		mac_ppem: u16,	//Upper limit of range, in PPEM
		behavior: as<gaspBehavior>(u16),	//Flags describing desired rasterizer behavior.
	})
};

//-----------------------------------------------------------------------------
//	glyf	Glyph Data
//-----------------------------------------------------------------------------

const enum INSTRUCTION {
	AA			= 0x7f,	// Adjust Angle
	ABS			= 0x64,	// ABSolute value
	ADD			= 0x60,	// ADD
	ALIGNPTS	= 0x27,	// ALIGN Points
	ALIGNRP		= 0x3c,	// ALIGN to Reference Point
	AND			= 0x5c,	// logical AND
	CALL		= 0x2b,	// CALL function
	CEILING		= 0x67,	// CEILING
	CINDEX		= 0x25,	// Copy the INDEXed element to the top of the stack
	CLEAR		= 0x22,	// CLEAR the stack
	DEBG		= 0x4f,	// DEBUG call
	DELTAC1		= 0x73,	// DELTA exception C1
	DELTAC2		= 0x74,	// DELTA exception C2
	DELTAC3		= 0x75,	// DELTA exception C3
	DELTAP1		= 0x5d,	// DELTA exception P1
	DELTAP2		= 0x71,	// DELTA exception P2
	DELTAP3		= 0x72,	// DELTA exception P3
	DEPTH		= 0x24,	// DEPTH of the stack
	DIV			= 0x62,	// DIVide
	DUP			= 0x20,	// DUPlicate top stack element
	EIF			= 0x59,	// End IF
	ELSE		= 0x1b,	// ELSE clause
	ENDF		= 0x2d,	// END Function definition
	EQ			= 0x54,	// EQual
	EVEN		= 0x57,	// EVEN
	FDEF		= 0x2c,	// Function DEFinition
	FLIPOFF		= 0x4e,	// set the auto FLIP Boolean to OFF
	FLIPON		= 0x4d,	// set the auto FLIP Boolean to ON
	FLIPPT		= 0x80,	// FLIP PoinT
	FLIPRGOFF	= 0x82,	// FLIP RanGe OFF
	FLIPRGON	= 0x81,	// FLIP RanGe ON
	FLOOR		= 0x66,	// FLOOR
	GC			= 0x46,	//-0x47[a] Get Coordinate projected onto the projection vector
	GETINFO		= 0x88,	// GET INFOrmation
	GFV			= 0x0d,	// Get Freedom Vector
	GPV			= 0x0c,	// Get Projection Vector
	GT			= 0x52,	// Greater Than
	GTEQ		= 0x53,	// Greater Than or EQual
	IDEF		= 0x89,	// Instruction DEFinition
	IF			= 0x58,	// IF test
	INSTCTRL	= 0x8e,	// INSTRuction execution ConTRoL
	IP			= 0x39,	// Interpolate Point
	ISECT		= 0x0f,	// moves point p to the InterSECTion of two lines
	IUP			= 0x30,	//-0x31[a] Interpolate Untouched Points through the outline
	JMPR		= 0x1c,	// JuMP Relative
	JROF		= 0x79,	// Jump Relative On False
	JROT		= 0x78,	// Jump Relative On True
	LOOPCALL	= 0x2a,	// LOOP and CALL function
	LT			= 0x50,	// Less Than
	LTEQ		= 0x51,	// Less Than or Equal
	MAX			= 0x8b,	// MAXimum of top two stack elements
	MD			= 0x49,	//-0x4a[a] Measure Distance
	MDAP		= 0x2e,	//-0x2f[a] Move Direct Absolute Point
	MDRP		= 0xc0,	//-0xdf[abcde] Move Direct Relative Point
	MIAP		= 0x3e,	//-0x3f[a] Move Indirect Absolute Point
	MIN			= 0x8c,	// MINimum of top two stack elements
	MINDEX		= 0x26,	// Move the INDEXed element to the top of the stack
	MIRP		= 0xe0,	//-0xff[abcde] Move Indirect Relative Point
	MPPEM		= 0x4b,	// Measure Pixels Per EM
	MPS			= 0x4c,	// Measure Point Size
	MSIRP		= 0x3a,	//-0x3b[a] Move Stack Indirect Relative Point
	MUL			= 0x63,	// MULtiply
	NEG			= 0x65,	// NEGate
	NEQ			= 0x55,	// Not EQual
	NOT			= 0x5c,	// logical NOT
	NPUSHB		= 0x40,	// PUSH N Bytes
	NPUSHW		= 0x41,	// PUSH N Words
	NROUND		= 0x6c,	//-0x6f[ab] No ROUNDing of value
	ODD			= 0x56,	// ODD
	OR			= 0x5b,	// logical OR
	POP			= 0x21,	// POP top stack element
	PUSHB		= 0xb0,	//-0xb7[abc] PUSH Bytes
	PUSHW		= 0xb8,	//-0xbf[abc] PUSH Words
	RCVT		= 0x45,	// Read Control Value Table entry
	RDTG		= 0x7d,	// Round Down To Grid
	ROFF		= 0x7a,	// Round OFF
	ROLL		= 0x8a,	//ROLL the top three stack elements
	ROUND		= 0x68,	//-0x6b[ab] ROUND value
	RS			= 0x43,	// Read Store
	RTDG		= 0x3d,	// Round To Double Grid
	RTG			= 0x18,	// Round To Grid
	RTHG		= 0x19,	// Round To Half Grid
	RUTG		= 0x7c,	// Round Up To Grid
	S45ROUND	= 0x77,	// Super ROUND 45 degrees
	SANGW		= 0x7e,	// Set Angle Weight
	SCANCTRL	= 0x85,	// SCAN conversion ConTRoL
	SCANTYPE	= 0x8d,	// SCANTYPE
	SCFS		= 0x48,	// Sets Coordinate From the Stack using projection vector and freedom vector
	SCVTCI		= 0x1d,	// Set Control Value Table Cut-In
	SDB			= 0x5e,	// Set Delta Base in the graphics state
	SDPVTL		= 0x86,	//-0x87[a] Set Dual Projection Vector To Line
	SDS			= 0x5f,	// Set Delta Shift in the graphics state
	SFVFS		= 0x0b,	// Set Freedom Vector From Stack
	SFVTCA		= 0x04,	//-0x05[a] Set Freedom Vector To Coordinate Axis
	SFVTL		= 0x08,	//-0x09[a] Set Freedom Vector To Line
	SFVTP		= 0x0e,	// Set Freedom Vector To Projection Vector
	SHC			= 0x34,	//-0x35[a] SHift Contour using reference point
	SHP			= 0x32,	//-0x33[a] SHift Point using reference point
	SHPIX		= 0x38,	// SHift point by a PIXel amount
	SHZ			= 0x36,	//-0x37[a] SHift Zone using reference point
	SLOOP		= 0x17,	// Set LOOP variable
	SMD			= 0x1a,	// Set Minimum Distance
	SPVFS		= 0x0a,	// Set Projection Vector From Stack
	SPVTCA		= 0x02,	//-0x03[a] Set Projection Vector To Coordinate Axis
	SPVTL		= 0x06,	//-0x07[a] Set Projection Vector To Line
	SROUND		= 0x76,	// Super ROUND
	SRP0		= 0x10,	// Set Reference Point 0
	SRP1		= 0x11,	// Set Reference Point 1
	SRP2		= 0x12,	// Set Reference Point 2
	SSW			= 0x1f,	// Set Single Width
	SSWCI		= 0x1e,	// Set Single Width Cut-In
	SUB			= 0x61,	// SUBtract
	SVTCA		= 0x00,	//-0x01[a] Set freedom and projection Vectors To Coordinate Axis
	SWAP		= 0x23,	// SWAP the top two elements on the stack
	SZP0		= 0x13,	// Set Zone Pointer 0
	SZP1		= 0x14,	// Set Zone Pointer 1
	SZP2		= 0x15,	// Set Zone Pointer 2
	SZPS		= 0x16,	// Set Zone PointerS
	UTP			= 0x29,	// UnTouch Point
	WCVTF		= 0x70,	// Write Control Value Table in Funits
	WCVTP		= 0x44,	// Write Control Value Table in Pixel units
	WS			= 0x42,	// Write Store
};

const enum SIMPLE {
	ON_CURVE			= 1 << 0,	//If set, the point is on the curve;Otherwise, it is off the curve.
	SHORT_X				= 1 << 1,	//the corresponding x-coordinate is 1 byte
	SHORT_Y				= 1 << 2,	//the corresponding y-coordinate is 1 byte
	REPEAT				= 1 << 3,	//If set, the next byte specifies the number of additional times this set of flags is to be repeated
	SAME_X				= 1 << 4,	//If short_x, this is a sign bit (clear is -ve); else if set the current x is the same as the previous x
	SAME_Y				= 1 << 5,	//If short_y, this is a sign bit (clear is -ve); else if set the current y is the same as the previous y
	OVERLAP				= 1 << 6,	//contours in the glyph description could overlap. When used, it must be set on the first flag byte for the glyph
};
const enum COMPOUND {
	ARG12_WORDS			= 1 << 0,	//the arguments are words; else they are bytes.
	ARGS_XY				= 1 << 1,	//the arguments are xy values; else they are points.
	ROUND_TO_GRID		= 1 << 2,	//round the xy values to grid
	HAVE_SCALE			= 1 << 3,	//there is a simple scale for the component
	MORE_COMPONENTS		= 1 << 5,	//at least one additional glyph follows this one.
	X_AND_Y_SCALE		= 1 << 6,	//the x direction will use a different scale than the y direction.
	TWO_BY_TWO			= 1 << 7,	//there is a 2-by-2 transformation
	HAVE_INSTRUCTIONS	= 1 << 8,	//instructions for the component character follow the last component.
	USE_MY_METRICS		= 1 << 9,	//Use metrics from this component for the compound glyph.
	OVERLAP				= 1 << 10,	//the components of this compound glyph overlap.
};

class Instructions extends Uint8Array {
    [index: number]: INSTRUCTION;
};

interface glyphRef {
	glyph:			number;
	mat:			float2x3;
};

export interface Glyph {
	min:			float2;
	max:			float2;
	curve?:			curveVertex[];
	refs?:			glyphRef[];
	instructions?:	Uint8Array;
};

export function readComposite(file: binary.stream) {
	const entryType = {
		flags: u16,
		index: u16,
	};

	const refs: glyphRef[] = [];
	let all_flags = 0;
	for (;;) {
		const	entry	= binary.read(file, entryType);
		const	flags	= entry.flags;
		all_flags	|= flags;

		const mat = identity2x3;
		if (flags & COMPOUND.ARG12_WORDS) {
			mat.z = float2(binary.read(file, fixed16), binary.read(file, fixed16));
		} else {
			mat.z = float2(binary.read(file, fixed8), binary.read(file, fixed8));
		}

		if (flags & COMPOUND.HAVE_SCALE) {
			mat.x.x = mat.y.y = binary.read(file, fixed16);
		} else if (flags & COMPOUND.X_AND_Y_SCALE) {
			mat.x.x = binary.read(file, fixed16);
			mat.y.y = binary.read(file, fixed16);
		} else if (flags & COMPOUND.TWO_BY_TWO) {
			mat.x = float2(binary.read(file, fixed16), binary.read(file, fixed16));
			mat.y = float2(binary.read(file, fixed16), binary.read(file, fixed16));
		}

		refs.push({glyph: entry.index, mat});
		if (!(flags & COMPOUND.MORE_COMPONENTS))
			break;
	}
	return {refs, have_instructions: !!(all_flags & COMPOUND.HAVE_INSTRUCTIONS)};
}

class GlyphReader extends binary.ReadClass({
	num_contours:	s16,
	min:			vec2(s16),
	max:			vec2(s16),
}) implements Glyph {
	curve?:			curveVertex[];
	refs?:			glyphRef[];
	instructions?:	Instructions;

	constructor(file: binary.stream) {
		super(file);

		if (this.num_contours < 0) {
			const {refs, have_instructions} = readComposite(file);
			this.refs = refs;
			if (have_instructions)
				this.instructions	= binary.read(file, binary.Buffer(u16)) as Instructions;

		} else {
			const end_pts		= binary.readn(file, u16, this.num_contours);
			this.instructions	= binary.read(file, binary.Buffer(u16));
			const npts			= end_pts[this.num_contours - 1] + 1;

			const flags: number[] = [];
			for (let i = 0; i < npts; ++i) {
				const	f = binary.read(file, binary.UINT8);
				flags[i] = f;
				if (f & SIMPLE.REPEAT) {
					for (let n = binary.read(file, binary.UINT8); n--;)
						flags[++i] = f;
				}
			}
			
			function getDelta(SHORT: boolean, SAMEPOS: boolean) {
				return SHORT
					? binary.read(file, u8) * (SAMEPOS ? 1 : -1)
					: SAMEPOS ? 0 : binary.read(file, s16);
			}


			let		x	= 0;
			const curve = flags.map(f => new curveVertex(
				x += getDelta(!!(f & SIMPLE.SHORT_X), !!(f & SIMPLE.SAME_X)),
				0,
				f & SIMPLE.ON_CURVE ? CURVE.ON_CURVE : CURVE.OFF_BEZ2,
			));

			x = 0;
			flags.forEach((f, i) => {
				curve[i].y	 = x += getDelta(!!(f & SIMPLE.SHORT_Y), !!(f & SIMPLE.SAME_Y));
			});

			for (let i = 0, j = 0; i < this.num_contours; i++) {
				const j1 = end_pts[i] + 1;
				if (curve[j].flags != 1) {
					for (let k = j; k < j1; k++) {
						if (curve[k].flags == 1) {
							arrayRotate(curve, j, k, j1 - j);
							break;
						}
					}
				}
				curve[j].flags = CURVE.ON_BEGIN;
				j = j1;
			}
			this.curve	= curve;
		}
	}
}

//-----------------------------------------------------------------------------
//	sbix	Standard Bitmap Graphics Table
//-----------------------------------------------------------------------------

export interface GlyphImage {
	originOffset:	float2;			//The position of the left edge of the bitmap graphic in relation to the glyph design space origin.
	graphicType:	string,//'jpg '|'png '|'tiff',
	data:			Uint8Array;
};

const sbix = {
	version:	u16,	//1
	flags:		binary.asEnum(u16, {DRAW_OUTLINES: 1 << 1}),
	strikes:	binary.ArrayType(u32, binary.OffsetType(u32, {
		ppem:	u16,							//The PPEM size for which this strike was designed.
		ppi:	u16,							//The device pixel density (in PPI) for which this strike was designed. (E.g., 96 PPI, 192 PPI.)
		glyphs: binary.RemainingArrayType(as<GlyphImage>(binary.OffsetType(u32, {
			originOffset:	vec2(s16),			//The position of the left edge of the bitmap graphic in relation to the glyph design space origin.
			graphicType:	TAG,				//Indicates the format of the embedded graphic data: one of 'jpg ', 'png ' or 'tiff', or 'dupe' (which indicates data is a uint16be glyphid)
			data:			binary.Remainder,	//The actual embedded graphic data. The total length is inferred from sequential entries in the glyphDataOffsets array and the fixed size (8 bytes) of the preceding fields.
		}))),
	})),
};

//-----------------------------------------------------------------------------
//	SVG
//-----------------------------------------------------------------------------

const SVG = {
	version:	u16,//	Table version (starting at 0). Set to 0.
	documents:	binary.OffsetType(u32, binary.ArrayType(u16, {
		startGlyphID:	u16,
		endGlyphID:		u16,
		doc:			binary.OffsetType(u32, binary.Remainder),
		doc_length: 	u32,
	
	})),
	reserved:	u32,//	Set to 0.
};

//-----------------------------------------------------------------------------
//	GSUB + GPOS + GDEF Common
//-----------------------------------------------------------------------------

class Coverage extends binary.Class({coverage: binary.Switch(u16, {
	1: {
		glyphs:	binary.ArrayType(u16, u16),
	},
	
	2: {
		ranges:	binary.ArrayType(u16, {
			start:				u16,	//First glyph ID in the range
			end:				u16,	//Last glyph ID in the range
			startCoverageIndex:	u16,	//Coverage Index of first glyph ID in range
		}),
	},
})}) {
	lookup(glyph: number) {
		if ('glyphs' in this.coverage) {
			return lowerBound(this.coverage.glyphs, i => i < glyph);
		} else {
			const	i = lowerBound(this.coverage.ranges, i => i.start < glyph);
			return i == this.coverage.ranges.length || this.coverage.ranges[i].end < glyph ? -1 : glyph - this.coverage.ranges[i].start + this.coverage.ranges[i].startCoverageIndex;
		}
	}
}

const enum LookupFlag {
	RIGHT_TO_LEFT				= 0x0001,	//This bit relates only to the correct processing of the cursive attachment lookup type (GPOS lookup type 3). When this bit is set, the last glyph in a given sequence to which the cursive attachment lookup is applied, will be positioned on the baseline.
	IGNORE_BASE_GLYPHS			= 0x0002,	//If set, skips over base glyphs
	IGNORE_LIGATURES			= 0x0004,	//If set, skips over ligatures
	IGNORE_MARKS				= 0x0008,	//If set, skips over all combining marks
	USE_MARK_FILTERING_SET		= 0x0010,	//If set, indicates that the lookup table structure is followed by a MarkFilteringSet field. The layout engine skips over all mark glyphs not in the mark filtering set indicated.
	reserved					= 0x00E0,	//For future use (Set to zero)
	MARK_ATTACHMENT_TYPE_MASK	= 0xFF00,	//If not zero, skips over all marks of attachment type different from specified.
};

type AllValues<T> = T extends Record<number, Record<number, infer U>> ? U : never;

function LookupList<T extends Record<number, Record<number, binary.Type>>>(tables: T) {
	type A = Record<number, AllValues<T>>;

	return binary.ArrayType(u16, binary.OffsetType(u16, {
		type:	u16,
		flag:	as<LookupFlag>(u16),
		table:	binary.Switch(obj => obj.type, Object.fromEntries(Object.entries(tables).map(([k, v]) => [k, binary.ArrayType(u16, binary.OffsetType(u16, binary.Switch(u16, v as A)))]))),
		//u16	markFilteringSet;	//Index (base 0) into GDEF mark glyph sets structure. This field is only present if the USE_MARK_FILTERING_SET lookup flag is set.
	}));
}

const FeatureList = binary.ArrayType(u16, {
	tag:		TAG,
	feature:	binary.OffsetType(u16, {
		params:		binary.OffsetType(u16, binary.Remainder),
		indices:	binary.ArrayType(u16, u16),
	}),
});

const LangSys = {
	lookupOrderOffset:		binary.OffsetType(u16, binary.Remainder),		//= NULL (reserved for an offset to a reordering table)
	requiredFeatureIndex:	u16,	//Index of a feature required for this language system; if no required features = 0xFFFF
	featureIndices:			binary.ArrayType(u16, u16),
};

const Script = {
	defaultLangSys:		binary.OffsetType(u16, LangSys),
	langSysRecords:		binary.ArrayType(u16, {
		tag:		TAG,
		langSys:	binary.OffsetType(u16, LangSys),
	}),
};

const ScriptList = binary.ArrayType(u16, {
	tag:	TAG,
	script:	binary.OffsetType(u16, Script),
});

//-------------------------------------

const ClassDef = {
	format:	u16,
	data: binary.Switch(obj => obj.format, {
		1: {
			start:	u16,		//First glyph ID of the classValueArray
			values:	binary.ArrayType(u16, u16),
		},
		2: {
			ranges: binary.ArrayType(u16, {
				start:	u16,	//First glyph ID in the range
				end:	u16,	//Last glyph ID in the range
				Class:	u16,	//Applied to all glyphs in the range
			}),
		},
	})
};

//-------------------------------------

const SequenceLookup = {
	sequenceIndex:		u16,	//Index (zero-based) into the input glyph sequence
	lookupListIndex:	u16,	//Index (zero-based) into the LookupList
};

const ClassSequenceRuleSet = binary.ArrayType(u16, binary.OffsetType(u16, {
	glyphCount:			u16,	//Number of glyphs in the input glyph sequence
	seqLookupCount:		u16,	//Number of SequenceLookupRecords
	input:				binary.ArrayType(obj=>obj.glyphCount - 1, u16),	//Array of input glyph IDs'starting with the second glyph
	seqLookup:			binary.ArrayType(obj=>obj.seqLookupCount, SequenceLookup),
}));

const SequenceContext = {
	1: {
		coverage:	binary.OffsetType(u16, Coverage),
		rule_sets:	binary.ArrayType(u16, binary.OffsetType(u16, ClassSequenceRuleSet)),
	},
	
	2: {
		coverage:	binary.OffsetType(u16, Coverage),						
		class_defs:	binary.OffsetType(u16, ClassDef),						
		rule_sets:	binary.ArrayType(u16, binary.OffsetType(u16, ClassSequenceRuleSet)),
	},
	
	3: {
		glyphCount:	u16,
		seqLookupCount:	u16,
		coverages:	binary.ArrayType(obj=>obj.glyphCount, binary.OffsetType(u16, Coverage)),
		seqLookup:	binary.ArrayType(obj=>obj.seqLookupCount, SequenceLookup),
	}
};

//-------------------------------------

const ChainedSequenceRule = {
	_backtrack:	binary.ArrayType(u16, u16),
	//u16	backtrackGlyphCount;	//Number of glyphs in the backtrack sequence
	//u16	backtrackSequence;//[backtrackGlyphCount];	//Array of backtrack glyph IDs
	//u16	inputGlyphCount;	//Number of glyphs in the input sequence
	//u16	inputSequence;//[inputGlyphCount - 1];	//Array of input glyph IDs'start with second glyph
	//u16	lookaheadGlyphCount;	//Number of glyphs in the lookahead sequence
	//u16	lookaheadSequence;//[lookaheadGlyphCount];	//Array of lookahead glyph IDs
	//u16	seqLookupCount;	//Number of SequenceLookupRecords
	//SequenceLookup	seqLookupRecords;//[seqLookupCount];	//Array of SequenceLookupReco

	//auto	backtrack()	const	{ return _backtrack.all(); }
	//auto	input()		const	{ return ((glyph_table*)backtrack().end())->all(); }
	//auto	lookahead()	const	{ return ((glyph_table*)input().end())->all(); }
	//auto	seqLookup()	const	{ return ((table(u16, SequenceLookup)*)lookahead().end())->all(); }
};

const ChainedSequenceRuleSet = binary.ArrayType(u16, binary.OffsetType(u16, ChainedSequenceRule));

const ChainedSequenceContext = {
	1: {
		coverage:	binary.OffsetType(u16, binary.Remainder),
		rule_sets:	binary.ArrayType(u16, binary.OffsetType(u16, ChainedSequenceRuleSet)),
	},

	2: {
		coverage:	binary.OffsetType(u16, Coverage),
		backtrack:	binary.OffsetType(u16, ClassDef),	//table containing backtrack sequence context, from beginning of ChainedSequenceContextFormat2 table
		input:		binary.OffsetType(u16, ClassDef),	//table containing input sequence context, from beginning of ChainedSequenceContextFormat2 table
		lookahead:	binary.OffsetType(u16, ClassDef),	//table containing lookahead sequence context, from beginning of ChainedSequenceContextFormat2 table
		rule_sets:	binary.ArrayType(u16, binary.OffsetType(u16, ChainedSequenceRuleSet)),
	},

	3: {
		_backtrack:	binary.ArrayType(u16, binary.OffsetType(u16, Coverage)),
		//u16		backtrackGlyphCount;	//Number of glyphs in the backtrack sequence
		//binary.OffsetType(u16, void),	backtrackCoverageOffsets;//[backtrackGlyphCount]	Array of offsets to coverage tables for the backtrack sequence
		//u16		inputGlyphCount;	//Number of glyphs in the input sequence
		//binary.OffsetType(u16, void),	inputCoverageOffsets;//[inputGlyphCount]	Array of offsets to coverage tables for the input sequence
		//u16		lookaheadGlyphCount;	//Number of glyphs in the lookahead sequence
		//binary.OffsetType(u16, void),	lookaheadCoverageOffsets;//[lookaheadGlyphCount]	Array of offsets to coverage tables for the lookahead sequence
		//u16		seqLookupCount;	//Number of SequenceLookupRecords
		//SequenceLookup	seqLookupRecords;//[seqLookupCount]	Array of SequenceLookupRecords

		//auto	backtrack()	const	{ return _backtrack.all(); }
		//auto	input()		const	{ return ((glyph_table*)backtrack().end())->all(); }
		//auto	lookahead()	const	{ return ((glyph_table*)input().end())->all(); }
		//auto	seqLookup()	const	{ return ((table(u16, SequenceLookup)*)lookahead().end())->all(); }
	},
};

//-----------------------------------------------------------------------------
//	GSUB	Glyph Substitution Table
//-----------------------------------------------------------------------------

const GSUB = {
	version:		u32,
	scripts:		binary.OffsetType(u16, ScriptList),	
	features:		binary.OffsetType(u16, FeatureList),
	lookups:		binary.OffsetType(u16, LookupList({
		1: {//SINGLE		= 1,	// Replace one glyph with one glyph
			1:{//SINGLE1
				coverage:	binary.OffsetType(u16, Coverage),
				delta:		s16,		//Add to original glyph ID to get substitute glyph ID
			},
			2:{//SINGLE2
				coverage:	binary.OffsetType(u16, Coverage),
				subs:		binary.ArrayType(u16, u16)	//	Array of substitute glyph IDs ' ordered by Coverage index
			},
		},
		2: {//MULTIPLE		= 2,	// Replace one glyph with more than one glyph
			1:	 {//MULTIPLE1
				coverage:	binary.OffsetType(u16, Coverage),
				sequences:	binary.ArrayType(u16, binary.OffsetType(u16, binary.ArrayType(u16,u16))),
			},
		},
		3: {//ALTERN		= 3,	// Replace one glyph with one of many glyphs
			1: {//ALTERNATE1
				coverage:	binary.OffsetType(u16, Coverage),
				alternates:	binary.ArrayType(u16, binary.OffsetType(u16, binary.ArrayType(u16, u16))),
			},
		},
		4: {//LIGATURE		= 4,	// Replace multiple glyphs with one glyph
			1: {//LIGATURE1
				//struct Ligature		: LigatureGlyph, table(u16, u16) {};
				coverage:	binary.OffsetType(u16, Coverage),
				sets:		binary.ArrayType(u16, binary.OffsetType(u16, binary.ArrayType(u16, binary.OffsetType(u16, {ligature: u16})))),
			},
		},
		5: //CONTEXTUAL	= 5,	// Replace one or more glyphs in context
			SequenceContext,
		6: //CHAINED		= 6,	// Replace one or more glyphs in chained context
			ChainedSequenceContext,
		//7: {//EXTENSION		= 7,	// Extension mechanism for other substitutions (i.e. this excludes the Extension type substitution itself)
		//},
		8: {//REVERSE		= 8,	// Applied in reverse order, replace single glyph in chaining context
			1: {//REVERSE1
				coverage:	binary.OffsetType(u16, Coverage),
				_backtrack: binary.ArrayType(u16, binary.OffsetType(u16, binary.Remainder)),
				//u16	backtrackGlyphCount;	//Number of glyphs in the backtrack sequence.
				//binary.OffsetType(u16, void),	backtrackCoverageOffsets;//[backtrackGlyphCount]	Array of offsets to coverage tables in backtrack sequence, in glyph sequence order.
				//u16	lookaheadGlyphCount;	//Number of glyphs in lookahead sequence.
				//binary.OffsetType(u16, void),	lookaheadCoverageOffsets;//[lookaheadGlyphCount]	Array of offsets to coverage tables in lookahead sequence, in glyph sequence order.
				//u16	glyphCount;	//Number of glyph IDs in the substituteGlyphIDs array.
				//u16	substituteGlyphIDs[];//	Array of substitute glyph IDs ' ordered by Coverage index.
			
				//auto	backtrack()		const	{ return _backtrack.all(); }
				//auto	input()			const	{ return ((glyph_table*)backtrack().end())->all(); }
				//auto	lookahead()		const	{ return ((glyph_table*)input().end())->all(); }
				//auto	substitute()	const	{ return ((table(u16, u16)*)lookahead().end())->all(); }
			},
		},
	})),
	v1:	binary.Optional(obj => obj.version >= 0x00010001 , {
		variations:	binary.OffsetType(u32, binary.Remainder),
	}),
};

//-----------------------------------------------------------------------------
//	GPOS	Glyph Positioning Table
//-----------------------------------------------------------------------------

const enum ValueFormat {
	X_PLACEMENT			= 0x0001,	//Includes horizontal adjustment for placement
	Y_PLACEMENT			= 0x0002,	//Includes vertical adjustment for placement
	X_ADVANCE			= 0x0004,	//Includes horizontal adjustment for advance
	Y_ADVANCE			= 0x0008,	//Includes vertical adjustment for advance
	X_PLACEMENT_DEVICE	= 0x0010,	//Includes Device table (non-variable font) / VariationIndex table (variable font) for horizontal placement
	Y_PLACEMENT_DEVICE	= 0x0020,	//Includes Device table (non-variable font) / VariationIndex table (variable font) for vertical placement
	X_ADVANCE_DEVICE	= 0x0040,	//Includes Device table (non-variable font) / VariationIndex table (variable font) for horizontal advance
	Y_ADVANCE_DEVICE	= 0x0080,	//Includes Device table (non-variable font) / VariationIndex table (variable font) for vertical advance
	Reserved			= 0xFF00,	//For future use (set to zero)
}

function ValueRecord(format: ValueFormat) {
	return {
		xPlacement:	binary.Optional(format & ValueFormat.X_PLACEMENT,			s16),	//Horizontal adjustment for placement, in design units.
		yPlacement:	binary.Optional(format & ValueFormat.Y_PLACEMENT,			s16),	//Vertical adjustment for placement, in design units.
		xAdvance:	binary.Optional(format & ValueFormat.X_ADVANCE, 			s16),	//Horizontal adjustment for advance, in design units ' only used for horizontal layout.
		yAdvance:	binary.Optional(format & ValueFormat.Y_ADVANCE, 			s16),	//Vertical adjustment for advance, in design units ' only used for vertical layout.
		xPlaDevice:	binary.Optional(format & ValueFormat.X_PLACEMENT_DEVICE,	binary.OffsetType(u16, binary.Remainder)),
		yPlaDevice:	binary.Optional(format & ValueFormat.Y_PLACEMENT_DEVICE,	binary.OffsetType(u16, binary.Remainder)),
		xAdvDevice:	binary.Optional(format & ValueFormat.X_ADVANCE_DEVICE,		binary.OffsetType(u16, binary.Remainder)),
		yAdvDevice:	binary.Optional(format & ValueFormat.Y_ADVANCE_DEVICE,		binary.OffsetType(u16, binary.Remainder)),
	};
}
interface ValueRecord extends binary.ReadType<ReturnType<typeof ValueRecord>> {};

const Anchor = {
	format: u16,
	data: binary.Switch(obj=>obj.format, {
		1: {//Anchor1
			xCoordinate: 	s16,	//Horizontal value, in design units
			yCoordinate: 	s16,	//Vertical value, in design units
		},
		2: {//Anchor2
			xCoordinate: 	s16,	//Horizontal value, in design units
			yCoordinate: 	s16,	//Vertical value, in design units
			anchorPoint: 	u16,	//Index to glyph contour point
		},
		3: {//Anchor3
			xCoordinate:	s16,	//Horizontal value, in design units
			yCoordinate:	s16,	//Vertical value, in design units
			xDevice:		binary.OffsetType(u16, binary.Remainder),
			yDevice:		binary.OffsetType(u16, binary.Remainder),
		},
	})
};

const MarkRecord = {
	markClass:		u16,
	markAnchor:		binary.OffsetType(u16, Anchor),
};

const GPOS = {
	vsersion:		u32,
	scripts:		binary.OffsetType(u16, ScriptList),	
	features:		binary.OffsetType(u16, FeatureList),
	lookups:		binary.OffsetType(u16, LookupList({
		1: {//	SINGLE			Adjust position of a single glyph
			1:{//SINGLE1
				coverage:		binary.OffsetType(u16, Coverage),
				valueFormat:	u16,		
				valueRecord:	as<ValueRecord>(binary.FuncType(obj => ValueRecord(obj.valueFormat))),
			},
			2:{//SINGLE2
				coverage:		binary.OffsetType(u16, Coverage),
				valueFormat:	u16,		
				values:			binary.ArrayType(u16, as<ValueRecord>(binary.FuncType(obj => ValueRecord(obj.valueFormat)))),
			},
		},
		2: {//	PAIR			Adjust position of a pair of glyphs
			1: {//PAIR1
				coverage:		binary.OffsetType(u16, Coverage),
				valueFormat1:	u16,			//Defines the types of data in valueRecord1 ' for the first glyph in the pair (may be zero).
				valueFormat2:	u16,			//Defines the types of data in valueRecord2 ' for the second glyph in the pair (may be zero).
				pairSets:		binary.ArrayType(u16, binary.OffsetType(u16, binary.ArrayType(u16, {
					secondGlyph:	u16,				//Glyph ID of second glyph in the pair (first glyph is listed in the Coverage table).
					valueRecord1:	as<ValueRecord>(binary.FuncType(obj => ValueRecord(obj.obj.valueFormat1))),		//Positioning data for the first glyph in the pair.
					valueRecord2:	as<ValueRecord>(binary.FuncType(obj => ValueRecord(obj.obj.valueFormat2))),		//Positioning data for the second glyph in the pair.
				}))),
			},
			2: {//PAIR2
				coverage:		binary.OffsetType(u16, Coverage),
				valueFormat1:	u16,	// for the first glyph of the pair (may be zero).
				valueFormat2:	u16,	// for the second glyph of the pair (may be zero).
				classDef1Offset:binary.OffsetType(u16, ClassDef),	//Offset to ClassDef table, from beginning of PairPos subtable ' for the first glyph of the pair.
				classDef2Offset:binary.OffsetType(u16, ClassDef),	//Offset to ClassDef table, from beginning of PairPos subtable ' for the second glyph of the pair.
				class1Count:	u16,	//Number of classes in classDef1 table ' includes Class 0.
				class2Count:	u16,	//Number of classes in classDef2 table ' includes Class 0.
				records:		binary.ArrayType(obj => obj.class1Count * obj.class2Count, {
					valueRecord1:	as<ValueRecord>(binary.FuncType(obj => ValueRecord(obj.obj.valueFormat1))),	//Positioning for first glyph ' empty if valueFormat1 = 0.
					valueRecord2:	as<ValueRecord>(binary.FuncType(obj => ValueRecord(obj.obj.valueFormat2))),	//Positioning for second glyph ' empty if valueFormat2 = 0.
				}),//	Array of Class1 records, ordered by classes in classDef1.
			},
		},
		3: {//	CURSIVE			Attach cursive glyphs
			1:{//CURSIVE1
				coverage:	binary.OffsetType(u16, Coverage),
				entryExits:	binary.ArrayType(u16, {
					entryAnchor:	binary.OffsetType(u16, Anchor),
					exitAnchor:		binary.OffsetType(u16, Anchor),
				}),
			},
		},
		4: {//	MARK_TO_BASE	Attach a combining mark to a base glyph
			1:{//MARK_TO_BASE1
				markCoverage:	binary.OffsetType(u16, Coverage),
				baseCoverage:	binary.OffsetType(u16, Coverage),
				markClassCount:	u16,									//Number of classes defined for marks
				markArray:		binary.OffsetType(u16, binary.ArrayType(u16, MarkRecord)),
				baseArray:		binary.OffsetType(u16, binary.ArrayType(u16, binary.RemainingArrayType(binary.OffsetType(u16, Anchor)))),	//	Array of offsets (one per mark class) to Anchor tables. Offsets are from beginning of BaseArray table, ordered by class (offsets may be NULL).
			},
		},
		5: {//	MARK_TO_LIG		Attach a combining mark to a ligature
			1:{//MARK_TO_LIG1
				markCoverage:		binary.OffsetType(u16, Coverage),
				ligatureCoverage:	binary.OffsetType(u16, Coverage),
				markClassCount:		u16,								//Number of defined mark classes
				markArray:			binary.OffsetType(u16, binary.ArrayType(u16, MarkRecord)),
				ligatureArray:		binary.OffsetType(u16, binary.ArrayType(u16, binary.OffsetType(u16, binary.ArrayType(u16, binary.OffsetType(u16, Anchor))))),
			},
		},
		6: {//	MARK_TO_MARK	Attach a combining mark to another mark
			1:{//MARK_TO_MARK1
				mark1Coverage:	binary.OffsetType(u16, Coverage),
				mark2Coverage:	binary.OffsetType(u16, Coverage),
				markClassCount:	u16,							//Number of Combining Mark classes defined
				mark1Array:		binary.OffsetType(u16, binary.ArrayType(u16, MarkRecord)),
				mark2Array:		binary.OffsetType(u16, binary.ArrayType(u16, binary.OffsetType(u16, Anchor))),
			},
		},
		7: //	CONTEXTUAL		Position one or more glyphs in context
			SequenceContext,
		8: //	CHAINED			positioning	Position one or more glyphs in chained context
			ChainedSequenceContext,
		9: {//	EXTENSION		Extension mechanism for other positionings
			1:{//EXTENSION1
				type:	u16,
				extension:		binary.OffsetType(u32, binary.Remainder),
			},
		},
	})),
	variations:		binary.Optional(obj => obj.version >= 0x00010001,
		binary.OffsetType(u32, binary.Remainder)
	),
};

//-----------------------------------------------------------------------------
//	DSIG
//-----------------------------------------------------------------------------

const DSIG = {
	version:			u32,				// Version number of the DSIG table (0x00000001)
	numSignatures:		u16,				// Number of signatures in the table
	flags:				u16,				// Permission flags:bit 0: cannot be resigned,bits 1-7: Reserved (Set to 0)
	signatureRecords:	binary.ArrayType(obj => obj.numSignatures, {
		format:	u32,
		length:	u32,
		block:	binary.OffsetType(u32, binary.Switch(obj=>obj.format, {
			1:	{
				reserved1:	u16,				// Reserved for future use; set to zero.
				reserved2:	u16,				// Reserved for future use; set to zero.
				signature:	binary.Buffer(u32),	// PKCS#7 packetBlock1
			}
		})),
	}),
};

//-----------------------------------------------------------------------------
//	EOT	- embedded opentype
//-----------------------------------------------------------------------------
/*
const EOTname = binary.StringType(u16);

const EOTHeader = {
	//enum {MAGIC = 0x504c};

	eot_size:	u32,
	font_size:	u32,
	version:	u32,
	flags:		u32,
	panose:		PANOSE,
	charset:	u8,
	italic:		u8,
	weight:		u32,
	type:		u16,
	magic:		u16,
	unicode_range:			binary.ArrayType(4, u32),
	codepage_range:			binary.ArrayType(2, u32),
	checksum_adjustment:	u32,
	reserved:				binary.ArrayType(4, u32),

	//all padding values must be set to 0x0000
	padding1:		u16,
	family_name:	EOTname,		//Family string found in the name table of the font (name ID = 1)
	padding2:		u16,
	style_name:		EOTname,		//Subfamily string found in the name table of the font (name ID = 2)
	padding3:		u16,
	version_name:	EOTname,		//Version string found in the name table of the font (name ID = 5)
	padding4:		u16,
	full_name:		EOTname,		//Full name string found in the name table of the font (name ID = 4)

	v2_1:	binary.Optional(obj => obj.version >= 0x00020001, {
		padding5:		u16,
		root_string:	EOTname,

		v2_2:	binary.Optional(obj => obj.version >= 0x00020002, {
			root_string_checksum:	u32,
			EUDC_codepage:	u32,
			padding6:		u16,
			signature:		EOTname,
			EUDCFlags:		u32,			//processing flags for the EUDC font. Typical values might be TTEMBED_XORENCRYPTDATA and TTEMBED_TTCOMPRESSED.
			EUDCFontData:	EOTname,
		}),
	}),

	data:	binary.Buffer(obj => obj.font_size),//font_size];	//compressed or XOR encrypted as indicated by the processing flags.
};
*/
//-----------------------------------------------------------------------------
//	Font
//-----------------------------------------------------------------------------

function getElementById(e: xml.Element, id: string): xml.Element|undefined {
	if (e.attributes.id === id)
		return e;

	for (const i of e.allElements()) {
		const x = getElementById(i, id);
		if (x)
			return x;
	}
}

function gatherRefs(e: xml.Element, refs: Set<string>) {
	if (e.name === 'use')
		refs.add(e.attributes['xlink:href'].slice(1)); 

	const fill = e.attributes.fill;
	if (fill && fill.startsWith('url(#'))
		refs.add(fill.slice(5, -1)); 

	for (const i of e.allElements())
		gatherRefs(i, refs);
}

function ExtractSVG(svg: xml.Element, g: xml.Element) {
	const refs = new Set<string>;
	gatherRefs(g, refs);

	const children	= g.allElements();
	const defs		= svg.firstElement()?.elements.defs;
	
	if (defs) {
		const defs2: Record<string, xml.Element> = {};
		for (const i of defs.allElements())
			defs2[i.attributes.id] = i;

		const localsdefs = new xml.Element('defs');
		for (const i of refs.keys())
			localsdefs.add(defs2[i]);

		children.push(localsdefs);
	}

	const extent = new extent2;
	for (const i of g.allElements()) {
		if (i.name === 'path')
			extent.combine(curveExtent(parseSVGpath(i.attributes.d)));
	}
	return makeSVG(children, extent);
}

function makeSVG(children: xml.Element[], extent: extent2) {
	return new xml.Element('svg', {
		xmlns:			"http://www.w3.org/2000/svg",
		['xmlns:xlink']:"http://www.w3.org/1999/xlink",
		version:		"1.1",
		viewBox:		`${extent.min.x} ${extent.min.y} ${extent.ext.x} ${extent.ext.y}`,
		width:			extent.ext.x,
		height:			extent.ext.y
	}, children);
}

function SVGcolor(c: color) {
	return '#' + ((c.r << 16) + (c.g << 8) + (c.b << 0) + (1<< 24)).toString(16).slice(-6);
}
function makeSVGgradient(fill: Fill, id: string) {
	if (fill.type !== FILL.SOLID) {
		const stops = fill.gradient.stops.map(i => new xml.Element("stop", {offset: i.stop, 'stop-color': SVGcolor(i.color)}));
		const gradientUnits = 'userSpaceOnUse';
		const spreadMethod = fill.gradient.extend === EXTEND.REPEAT ? 'repeat' : fill.gradient.extend === EXTEND.REFLECT ? 'reflect' : undefined;

		switch (fill.type) {
			case FILL.LINEAR:
				return new xml.Element('linearGradient', {id, x1: fill.p0.x, y1: -fill.p0.y, x2: fill.p1.x, y2: -fill.p1.y, gradientUnits, spreadMethod}, stops);
			case FILL.SWEEP:
				return new xml.Element('sweepGradient', {gradientUnits, spreadMethod}, stops);//unsupported by svg
			case FILL.RADIAL:
				return new xml.Element('radialGradient', {id, fx: fill.c0.centre.x, fy: fill.c0.centre.y, fr: fill.c0.radius, cx: fill.c1.centre.x, cy: fill.c1.centre.y, r: fill.c1.radius, gradientUnits, spreadMethod}, stops);
		}
	}
}

function COLRtoSVG(layers: Layer[]) {
	let id = 1;

	const extent	= new extent2;
	const defs		= new xml.Element('defs');
	const children	= [defs, ...layers.map(j => {
		const d		= makeSVGPath(parseCurve(j.curves));
		const fill	= j.fill.type === FILL.SOLID
			? SVGcolor(j.fill.color)
			: (defs.add(makeSVGgradient(j.fill, `g${id}`)!), `url(#g${id++})`);
		extent.combine(curveExtent(parseSVGpath(d)));
		return new xml.Element("path", {d, fill});
	})];

	return makeSVG(children, extent);
}

const tableReaders = {
	'OS/2':	OS2,
	head:	head,
	hhea:	MetricsHead,
	vhea:	MetricsHead,
	maxp:	maxp,
	name:	name,
	cmap:	cmap,
	gasp:	gasp,
	sbix:	sbix,
	GSUB:	GSUB,
	GPOS:	GPOS,
	CPAL:	CPAL,
	COLR:	COLR,
	'SVG ':	SVG,
	'CFF ':	CFF,
	DSIG:	DSIG,

//	GDEF:	GDEF,
//	bgcl:	bgcl,
//	feat:	feat,
//	meta:	meta,
//	morx:	morx,
//	post:	post,
//	trak:	trak,
};

export const tableTypes = Object.keys(tableReaders);

export abstract class Font extends makeClass<{
    [K in keyof typeof tableReaders]?: binary.ReadType<typeof tableReaders[K]>
}>() {
	hmtx?:		Metrics;
	vmtx?:		Metrics;
	glyphdata?: Uint8Array[];
	cached:		(Glyph|undefined)[] = [];

	loadTable(tag: string, data: Uint8Array) {
		const type = tableReaders[tag as keyof typeof tableReaders];
		if (type)
			(this as any)[tag] = binary.read(new binary.stream(data), type);
	}

	numGlyphs() {
		return this.glyphdata?.length ?? this.COLR?.v1?.baseGlyphs.length ?? this.cached.length ?? 0;
	}

	getGlyph(id: number): Glyph | undefined {
		let glyph = this.cached[id];
		if (!glyph) {
			const cff = this['CFF '];
			glyph = cff					? cff.getGlyph(id)
				: this.glyphdata?.[id]	? new GlyphReader(new binary.stream(this.glyphdata[id]))
				: undefined;
			if (glyph)
				this.cached[id] = glyph;
		}
		return glyph;
	}

	getGlyphMapping() {
		if (this.cmap) {
			//let uvs_table;
	
			//score the tables to favour UNICODE_FULL
			const	i = argmin(this.cmap.tables, i => {
				switch (i.platform) {
					case PLATFORM.UNICODE:
						//if (i.encoding == ENCODING.UNI_variation)
						//	uvs_table = i;
						return  Math.abs(i.encoding - ENCODING.UNI_full);
					case PLATFORM.WINDOWS:
						return i.encoding == ENCODING.WIN_UCS4 ? 1 : i.encoding == ENCODING.WIN_UCS2 ? 2 : 3;
					default:
						return 4;
				}
			});
			
			return this.cmap.tables[i].data.map();
		}
	}

	getGlyphImages(ppem: number) {
		if (this.sbix) {
			const i 	=  argmin(this.sbix.strikes, i => Math.abs(i.ppem - ppem));
			const images = this.sbix.strikes[i].glyphs;

			let prev = images[0];
			for (const i of images.slice(1, -1)) {
				const size = i.data.byteOffset - prev.data.byteOffset;
				prev.data = prev.data.subarray(0, size);
				prev = i;
			}
			return images;
		}
	}
	
	getGlyphImage(id: number, ppem: number): GlyphImage | undefined {
		if (this.sbix) {
			const i 	=  argmin(this.sbix.strikes, i => Math.abs(i.ppem - ppem));
			const images = this.sbix.strikes[i].glyphs;
			if (images[id].graphicType === 'dupe')
				id = (images[id].data[0] << 8) + images[id].data[1];
			const image = images[id];
			return {...image, data:image.data.subarray(images[id + 1].data.byteLength - 8)};
		}
	}
	getGlyphCOLR(id: number) {
		return this.COLR && this.COLR.getGlyph(this, this.CPAL, id);
	}

	getGlyphSVG(id: number, preferCOLR = false) {
		if (preferCOLR) {
			const layers = this.getGlyphCOLR(id);
			if (layers)
				return COLRtoSVG(layers);
		}

		const svg = this['SVG '];
		if (svg) {
			for (const i of svg.documents) {
				if (id >= i.startGlyphID && id <= i.endGlyphID) {
					const svg	= xml.parse(binary.utils.decodeText(i.doc.subarray(0, i.doc_length)));
					const g		= getElementById(svg.firstElement()!, `glyph${id}`);
					if (g)
						return ExtractSVG(svg, g);
				}
			}
		}

		if (!preferCOLR) {
			const layers = this.getGlyphCOLR(id);
			if (layers)
				return COLRtoSVG(layers);
		}

		const glyph = this.getGlyph(id);
		if (glyph)
			return makeSVG(
				[new xml.Element('path', {d: makeSVGPath(parseCurve(glyph.curve!)), fill: 'black'})],
				new extent2(float2(glyph.min.x, - glyph.max.y), float2(glyph.max.x, -glyph.min.x))
			);
	}

}
//-----------------------------------------------------------------------------
//	Font Group
//-----------------------------------------------------------------------------

export abstract class FontGroup {
	fonts: Font[] = [];
	getSub(sub: string) {
		return this.fonts.find(i => (i.name as name).names[2] === sub);
	}
}

//-----------------------------------------------------------------------------
//	TTF
//-----------------------------------------------------------------------------

const SFNTHeader = {
	version:		u32,	// 0x00010000 for version 1.0 (or 'true' or 'typ1'); 'OTTO' for opentype
	num_tables:		u16,	// Number of tables.
	search_range:	u16,	// (Maximum power of 2 <= numTables) x 16.
	entry_selector:	u16,	// Log2(maximum power of 2 <= numTables).
	range_shift:	u16,	// NumTables x 16-searchRange.
	tables:	binary.ArrayType(obj => obj.num_tables, {
		tag:			TAG,	// 4 -byte identifier.
		checksum:		u32,	// CheckSum for this table.
		offset:			u32,	// Offset from beginning of TrueType font file.
		length:			u32,	// Length of this table.
	}),
};

export class TTF extends Font {
	static check(data: Uint8Array) {
		const u	= u32.get(new binary.stream(data));
		return u == 0x00010000 || u == binary.utils.stringCode('true') || u == binary.utils.stringCode('typ1');
	}

	constructor(file: binary.stream) {
		super();

		const sfnt	= binary.read(file, SFNTHeader);

		function findTable(tag: string) {
			return sfnt.tables.find(i => i.tag === tag);
		}
		function getTable(tag: string) {
			const table = findTable(tag);
			if (table)
				return file.buffer_at(table.offset, table.length);
		}

		for (const i in tableReaders) {
			const table = getTable(i);
			if (table)
				this.loadTable(i, table);
		}

		//glyph data
		const loca	= getTable("loca");
		const glyf	= getTable('glyf');
		if (loca && glyf)
			this.glyphdata = loadLocs(loca, glyf, this.head!.indexToLocFormat);

		//hmtx
		const hmtx = getTable('hmtx');
		if (hmtx && this.hhea)
			this.hmtx = loadMetrics(hmtx, this.numGlyphs(), this.hhea.numOfLongMetrics);
	}
}

//-----------------------------------------------------------------------------
//	TTC
//-----------------------------------------------------------------------------

const TTCHeader = {
	tag:			TAG,	// TrueType Collection ID string: 'ttcf'
	version:		u32,	// Version of the TTC Header (1.0), 0x00010000 or (2.0), 0x00020000
	num_fonts:		u32,
};
/*
const TTCHeader2 = {
	tag:			TAG,	// Tag indicating that a DSIG table exists, 0x44534947 ('DSIG') (null if no signature)
	length:			u32,	// The length (in bytes) of the DSIG table (null if no signature)
	offset:			u32,	// The offset (in bytes) of the DSIG table from the beginning of the TTC file (null if no signature)
};
*/
export class TTC extends FontGroup {
	static check(data: Uint8Array) {
		return TAG.get(new binary.stream(data)) === 'ttcf';
	}
	constructor(file: binary.stream) {
		super();
		const head	= binary.read(file, TTCHeader);
		if (head.tag == "ttcf") {
			const	offsets = binary.readn(file, u32, head.num_fonts);
			this.fonts = offsets.map(offset => new TTF(file.seek(offset)));
		}
	}

	getSub(sub: string) {
		return this.fonts.find(ttf => (ttf.name as name).names[2] === sub);
	}
}

