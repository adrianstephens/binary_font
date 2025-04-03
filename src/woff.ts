import * as zlib from 'zlib';
import * as xml from '@isopodlabs/xml';
import * as binary from '@isopodlabs/binary';
import {Font, vec2, Glyph, loadLocs, loadMetrics, readComposite, tableTypes} from './font';
import {curveVertex, curveExtent, parseCurve, makeCurveVertex} from './curves';
import {float2} from '@isopodlabs/maths/dist/vector';

const TAG		= binary.StringType(4);
const u8 		= binary.UINT8;
const u16 		= binary.UINT16_BE;
const s16 		= binary.INT16_BE;
const u32 		= binary.UINT32_BE;

//-----------------------------------------------------------------------------
//	WOFF
//-----------------------------------------------------------------------------

const WOFFHeader = {
	signature:		u32,	//0x774F4646 'wOFF'
	flavor:			u32,	//The "sfnt version" of the input font.
	length:			u32,	//Total size of the WOFF file.
	numTables:		u16,	//Number of entries in directory of font tables.
	reserved:		u16,	//Reserved; set to zero.
	totalSfntSize:	u32,	//Total size needed for the uncompressed font data, including the sfnt header, directory, and font tables (including padding).
	version:		u32,	//Major.Minor version of the WOFF file.
	metaOffset:		u32,	//Offset to metadata block, from beginning of WOFF file.
	metaLength:		u32,	//Length of compressed metadata block.
	metaOrigLength:	u32,	//Uncompressed size of metadata block.
	privOffset:		u32,	//Offset to private data block, from beginning of WOFF file.
	privLength:		u32,	//Length of private data block.

	tables:	binary.ArrayType(s => s.obj.numTables, {
		tag:			TAG,	//4-byte sfnt table identifier.
		offset:			u32,	//Offset to the data, from beginning of WOFF file.
		compLength:		u32,	//Length of the compressed data, excluding padding.
		origLength:		u32,	//Length of the uncompressed table, excluding padding.
		origChecksum:	u32,	//Checksum of the uncompressed table.
	}),
};

async function inflateAsync(buffer: Uint8Array) {
	return new Promise<Uint8Array>(resolve => zlib.inflate(buffer, (err, buffer) => resolve(buffer)));
};

async function brotliAsync(buffer: Uint8Array) {
	return new Promise<Uint8Array>(resolve => zlib.brotliDecompress(buffer, (err, buffer) => resolve(buffer)));
};

export class WOFF extends Font {
	metadata?:	xml.Element;
	priv?:		Uint8Array;

	static async load(file: binary.stream) {
		const me 	= new WOFF;
		const h		= binary.read(file, WOFFHeader);
		console.log(h);

		if (h.metaLength) {
			const buffer = inflateAsync(file.buffer_at(h.metaOffset, h.metaLength));
			me.metadata = xml.parse((await buffer).toString());
		}
		if (h.privLength) {
			const buffer = inflateAsync(file.buffer_at(h.privOffset, h.privLength));
			me.priv = await buffer;
		}

		function findTable(tag: string) {
			return h.tables.find(i => i.tag === tag);
		}
		function getTable(tag: string) {
			const table = findTable(tag);
			if (table) {
				const buffer = file.buffer_at(table.offset, table.compLength);
				return table.compLength != table.origLength
					? inflateAsync(buffer)
					: file.buffer_at(table.offset, table.origLength);
			}
		}
		for (const i of tableTypes) {
			const buffer = await getTable(i);
			if (buffer)
				me.loadTable(i, buffer);
		}

		//glyph data
		const loca = await getTable("loca");
		const glyf = await getTable('glyf');
		if (loca && glyf)
			me.glyphdata = loadLocs(loca, glyf, me.head!.indexToLocFormat);

		//hmtx
		const hmtx = await getTable('hmtx');
		if (hmtx && me.hhea)
			me.hmtx = loadMetrics(hmtx, me.numGlyphs(), me.hhea!.numOfLongMetrics);

		return me;
	}
}

//-----------------------------------------------------------------------------
//	WOFF2
//-----------------------------------------------------------------------------

const UShort255 = {
	get(s: binary._stream) {
		const oneMoreByteCode1    = 255;
		const oneMoreByteCode2    = 254;
		const wordCode            = 253;
		const lowestUCode         = 253;

		const code = u8.get(s);
		return	code == wordCode	? u16.get(s)
			:	code == oneMoreByteCode1	? u8.get(s) + lowestUCode
			:	code == oneMoreByteCode2	? u8.get(s) + lowestUCode * 2
			:	code;
	},
	put(_s: binary._stream, _v: number) {
	}
};

const UIntBase128 = {
	get(s: binary._stream) {
		let accum = 0;
		for (let i = 0; i < 5; i++) {
			const byte = u8.get(s);
			accum = (accum << 7) | (byte & 0x7F);
			if ((byte & 0x80) == 0)
				return accum;
		}
		throw new Error("bad uint");
	},
	put(_s: binary._stream, _v: number) {
	}
};

function MC(xbits: number, xdelta: number, ybits: number, ydelta: number) { return {xbits, xdelta, ybits, ydelta}; }

const TripletEncodingTable = [
	MC( 0, 0,	-8, 0),		MC( 0, 0,	+8, 0),		MC( 0, 0,	-8, 256),	MC( 0, 0,	+8, 256),
	MC( 0, 0,	-8, 512),	MC( 0, 0,	+8, 512),	MC( 0, 0,	-8, 768),	MC( 0, 0,	+8, 768),
	MC( 0, 0,	-8, 1024),	MC( 0, 0,	+8, 1024),	MC(-8, 0,	 0, 0),		MC(+8, 0,	 0, 0),
	MC(-8, 256,	 0, 0),		MC(+8, 256,	 0, 0),		MC(-8, 512,	 0, 0),		MC(+8, 512,	 0, 0),
	MC(-8, 768,	 0, 0),		MC(+8, 768,	 0, 0),		MC(-8, 1024, 0, 0),		MC(+8, 1024, 0, 0),
	MC(-4, 1,	-4, 1),		MC(+4, 1,	-4, 1),		MC(-4, 1,	+4, 1),		MC(+4, 1,	+4, 1),
	MC(-4, 1,	-4, 17),	MC(+4, 1,	-4, 17),	MC(-4, 1,	+4, 17),	MC(+4, 1,	+4, 17),
	MC(-4, 1,	-4, 33),	MC(+4, 1,	-4, 33),	MC(-4, 1,	+4, 33),	MC(+4, 1,	+4, 33),
	MC(-4, 1,	-4, 49),	MC(+4, 1,	-4, 49),	MC(-4, 1,	+4, 49),	MC(+4, 1,	+4, 49),
	MC(-4, 17,	-4, 1),		MC(+4, 17,	-4, 1),		MC(-4, 17,	+4, 1),		MC(+4, 17,	+4, 1),
	MC(-4, 17,	-4, 17),	MC(+4, 17,	-4, 17),	MC(-4, 17,	+4, 17),	MC(+4, 17,	+4, 17),
	MC(-4, 17,	-4, 33),	MC(+4, 17,	-4, 33),	MC(-4, 17,	+4, 33),	MC(+4, 17,	+4, 33),
	MC(-4, 17,	-4, 49),	MC(+4, 17,	-4, 49),	MC(-4, 17,	+4, 49),	MC(+4, 17,	+4, 49),
	MC(-4, 33,	-4, 1),		MC(+4, 33,	-4, 1),		MC(-4, 33,	+4, 1),		MC(+4, 33,	+4, 1),
	MC(-4, 33,	-4, 17),	MC(+4, 33,	-4, 17),	MC(-4, 33,	+4, 17),	MC(+4, 33,	+4, 17),
	MC(-4, 33,	-4, 33),	MC(+4, 33,	-4, 33),	MC(-4, 33,	+4, 33),	MC(+4, 33,	+4, 33),
	MC(-4, 33,	-4, 49),	MC(+4, 33,	-4, 49),	MC(-4, 33,	+4, 49),	MC(+4, 33,	+4, 49),
	MC(-4, 49,	-4, 1),		MC(+4, 49,	-4, 1),		MC(-4, 49,	+4, 1),		MC(+4, 49,	+4, 1),
	MC(-4, 49,	-4, 17),	MC(+4, 49,	-4, 17),	MC(-4, 49,	+4, 17),	MC(+4, 49,	+4, 17),
	MC(-4, 49,	-4, 33),	MC(+4, 49,	-4, 33),	MC(-4, 49,	+4, 33),	MC(+4, 49,	+4, 33),
	MC(-4, 49,	-4, 49),	MC(+4, 49,	-4, 49),	MC(-4, 49,	+4, 49),	MC(+4, 49,	+4, 49),
	MC(-8, 1,	-8, 1),		MC(+8, 1,	-8, 1),		MC(-8, 1,	+8, 1),		MC(+8, 1,	+8, 1),
	MC(-8, 1,	-8, 257),	MC(+8, 1,	-8, 257),	MC(-8, 1,	+8, 257),	MC(+8, 1,	+8, 257),
	MC(-8, 1,	-8, 513),	MC(+8, 1,	-8, 513),	MC(-8, 1,	+8, 513),	MC(+8, 1,	+8, 513),
	MC(-8, 257,	-8, 1),		MC(+8, 257,	-8, 1),		MC(-8, 257,	+8, 1),		MC(+8, 257,	+8, 1),
	MC(-8, 257,	-8, 257),	MC(+8, 257,	-8, 257),	MC(-8, 257,	+8, 257),	MC(+8, 257,	+8, 257),
	MC(-8, 257,	-8, 513),	MC(+8, 257,	-8, 513),	MC(-8, 257,	+8, 513),	MC(+8, 257,	+8, 513),
	MC(-8, 513,	-8, 1),		MC(+8, 513,	-8, 1),		MC(-8, 513,	+8, 1),		MC(+8, 513,	+8, 1),
	MC(-8, 513,	-8, 257),	MC(+8, 513,	-8, 257),	MC(-8, 513,	+8, 257),	MC(+8, 513,	+8, 257),
	MC(-8, 513,	-8, 513),	MC(+8, 513,	-8, 513),	MC(-8, 513,	+8, 513),	MC(+8, 513,	+8, 513),
	MC(-12, 0,	-12, 0),	MC(+12, 0,	-12, 0),	MC(-12, 0,	+12, 0),	MC(+12, 0,	+12, 0),
	MC(-16, 0,	-16, 0),	MC(+16, 0,	-16, 0),	MC(-16, 0,	+16, 0),	MC(+16, 0,	+16, 0),
];

function decodeTriplet(flags: number, file: binary._stream): float2 {
	const e		= TripletEncodingTable[flags & 0x7f];
	const xbits	= Math.abs(e.xbits);
	const ybits = Math.abs(e.ybits);
	const value = Number(binary.UINT(xbits + ybits, true).get(file));
	const x		= e.xdelta + (value >> ybits);
	const y		= e.ydelta + (value & ((1 << ybits) - 1));
	return float2(e.xbits < 0 ? -x : x, e.ybits < 0 ? -y : y);
}

const KnownTags = [
	'cmap',	'head',	'hhea',	'hmtx',	'maxp',	'name',	'OS/2',	'post',
	'cvt ',	'fpgm',	'glyf',	'loca',	'prep',	'CFF ',	'VORG',	'EBDT',
	'EBLC',	'gasp',	'hdmx',	'kern',	'LTSH',	'PCLT',	'VDMX',	'vhea',
	'vmtx',	'BASE',	'GDEF',	'GPOS',	'GSUB',	'EBSC',	'JSTF',	'MATH',
	'CBDT',	'CBLC',	'COLR',	'CPAL',	'SVG ',	'sbix',	'acnt',	'avar',
	'bdat',	'bloc',	'bsln',	'cvar',	'fdsc',	'feat',	'fmtx',	'fvar',
	'gvar',	'hsty',	'just',	'lcar',	'mort',	'morx',	'opbd',	'prop',
	'trak',	'Zapf',	'Silf',	'Glat',	'Gloc',	'Feat',	'Sill',	'xxxx',
];

const WOFF2Header = {
	signature:		u32,	//0x774F4632 'wOF2'
	flavor:			TAG,	//The "sfnt version" of the input font.
	length:			u32,	//Total size of the WOFF file.
	numTables:		u16,	//Number of entries in directory of font tables.
	reserved:		u16,	//Reserved; set to zero.
	totalSfntSize:	u32,	//Total size needed for the uncompressed font data, including the sfnt header, directory, and font tables (including padding).
	totalCompressedSize: u32,	//Total length of the compressed data block.
	version:		u32,	//Major.Minor version of the WOFF file.
	metaOffset:		u32,	//Offset to metadata block, from beginning of WOFF file.
	metaLength:		u32,	//Length of compressed metadata block.
	metaOrigLength:	u32,	//Uncompressed size of metadata block.
	privOffset:		u32,	//Offset to private data block, from beginning of WOFF file.
	privLength:		u32,	//Length of private data block.

	tables:	binary.ArrayType(s => s.obj.numTables, {
		flags:				u8,		//table type and flags
		tag:				binary.Switch(s => (s.obj.flags & 63) === 63 ? 1 : 0, {
			0: binary.Func(s => KnownTags[s.obj.flags & 63]),
			1: TAG,
		}),
		origLength:			UIntBase128,
		// transformed length (if applicable)
		transformLength:	binary.Optional(s => (s.obj.tag == 'glyf' || s.obj.tag == 'loca') === !(s.obj.flags >> 6), UIntBase128),
		data:				binary.DontRead<Uint8Array>(),
	}),

	sub:	binary.Optional(s => s.obj.flavor === 'ttcf', {
		version:	u32,
		subs:		binary.ArrayType(UShort255, {
			numTables:		UShort255,
			flavor:			u32,
			table_indices:	binary.ArrayType(s => s.obj.numTables, UShort255),
		})
	}),
};

class WOFF2TransformedGlyf extends binary.Class({
	reserved:				u16,	// 0x0000
	optionFlags:			u16,	// Bit 0: if set, indicates the presence of the overlapSimpleBitmap[] bit array. Bits 1-15: Reserved.
	numGlyphs:				u16,	// Number of glyphs
	indexFormat:			u16,	// Offset format for loca table, should be consistent with indexToLocFormat of the original head table (see [OFF] specification)
	nContourStreamSize:		u32,	// Size of nContour stream in bytes
	nPointsStreamSize:		u32,	// Size of nPoints stream in bytes
	flagStreamSize:			u32,	// Size of flag stream in bytes
	glyphStreamSize:		u32,	// Size of glyph stream in bytes (a stream of variable-length encoded values, see description below)
	compositeStreamSize:	u32,	// Size of composite stream in bytes (a stream of variable-length encoded values, see description below)
	bboxStreamSize:			u32,	// Size of bbox data in bytes representing combined length of bboxBitmap (a packed bit array) and bboxStream (a stream of Int16 values)
	instructionStreamSize:	u32,	// Size of instruction stream (a stream of UInt8 values)
}) {
	glyphs:	Glyph[];

	constructor(file: binary.stream) {
		super(file);
		const nContourStream		= file.read_buffer(this.nContourStreamSize);	//Stream of Int16 values representing number of contours for each glyph record
		const nPointsStream			= file.read_buffer(this.nPointsStreamSize);		//Stream of values representing number of outline points for each contour in glyph records
		const flagStream			= file.read_buffer(this.flagStreamSize);		//Stream of UInt8 values representing flag values for each outline point.
		const glyphStream			= file.read_buffer(this.glyphStreamSize);		//Stream of bytes representing point coordinate values using variable length encoding format (defined in subclause 5.2)
		const compositeStream		= file.read_buffer(this.compositeStreamSize);	//Stream of bytes representing component flag values and associated composite glyph data
		const bboxStream			= file.read_buffer(this.bboxStreamSize);
		const instructionStream		= file.read_buffer(this.instructionStreamSize);	//Stream of UInt8 values representing a set of instructions for each corresponding glyph
		const _overlapSimpleBitmap	= file.remainder();	//A numGlyphs-long bit array that provides values for the overlap flag [bit 6] for each simple glyph. (Flag values for composite glyphs are already encoded as part of the compositeStream[]).

		const nc	= Array.from(binary.utils.to16s(nContourStream, true));

		const pfile = new binary.stream(nPointsStream);
		const ffile	= new binary.stream(flagStream);
		const gfile	= new binary.stream(glyphStream);
		const cfile	= new binary.stream(compositeStream);
		const ifile	= new binary.stream(instructionStream);
		const bfile	= new binary.stream(bboxStream);

		const bboxBitmap = bfile.read_buffer((this.numGlyphs + 7) >> 3);

		this.glyphs = nc.map((nc, index) => {
			let bbox = bboxBitmap[index >> 3] & (1 << (index & 7)) ? binary.read(bfile, {min: vec2(s16), max: vec2(s16)}) : undefined;
			let instructions;

			if (nc < 0) {
				if (!bbox)
					throw new Error("missing bbox");

				const {refs, have_instructions} = readComposite(cfile);
				if (have_instructions) {
					const ins = UShort255.get(gfile);
					instructions = ifile.read_buffer(ins);
				}
	
				return {
					min: bbox.min,
					max: bbox.max,
					refs,
					instructions
				};

			} else {
				const curve: curveVertex[] = [];
				if (nc) {
					let	pt = float2(0,0);
					while (nc--) {
						const np = UShort255.get(pfile);
						for (let i = 0; i < np; ++i) {
							const flags = u8.get(ffile);
							pt = pt.add(decodeTriplet(flags, gfile));
							curve.push(makeCurveVertex(pt, i === 0 ? curveVertex.ON_BEGIN : flags & 0x80 ? curveVertex.OFF_BEZ2 : curveVertex.ON_CURVE));
						}
					}
					const ins = UShort255.get(gfile);
					instructions = ifile.read_buffer(ins);
				}

				if (!bbox)
					bbox = curveExtent(parseCurve(curve));

				return {
					min: bbox.min,
					max: bbox.max,
					curve,
					instructions
				};
			}
		});

	}
}

export class WOFF2 extends Font {
	metadata?:	xml.Element;
	priv?:		Uint8Array;

	static async load(file: binary.stream) {
		const me 	= new WOFF2;
		const h		= binary.read(file, WOFF2Header);
		console.log(h);

		if (h.metaLength) {
			const buffer = brotliAsync(file.buffer_at(h.metaOffset, h.metaLength));
			me.metadata = xml.parse((await buffer).toString());
		}
		if (h.privLength) {
			const buffer = brotliAsync(file.buffer_at(h.privOffset, h.privLength));
			me.priv = await buffer;
		}

		const comp		= file.read_buffer(h.totalCompressedSize);
		const uncomp	= await brotliAsync(comp);

		let offset = 0;
		for (const i of h.tables) {
			const	start = offset;
		  	offset	+= i.transformLength ?? i.origLength;
			i.data	= uncomp.subarray(start, offset);
		}

		function findTable(tag: string) {
			return h.tables.find(i => i.tag === tag);
		}

		for (const i of tableTypes) {
			const table = findTable(i);
			if (table && !(table.flags & 0xc0))
				me.loadTable(i, table.data!);
		}

		//glyph data
		const loca = findTable("loca");
		const glyf = findTable('glyf');
		if (loca && glyf) {
			if ((glyf.flags & 0xc0) == 0) {
				const g = new WOFF2TransformedGlyf(new binary.stream(glyf.data!));
				me.cached	= g.glyphs;
			} else {
				me.glyphdata = loadLocs(loca.data!, glyf.data!, me.head!.indexToLocFormat);
			}
		}

		//hmtx
		const hmtx = findTable('hmtx');
		if (hmtx && me.hhea) {
			const	numGlyphs	= me.numGlyphs();
			const	numMetrics	= me.hhea.numOfLongMetrics;
			if (hmtx.flags & 0xc0) {
				const	hfile		= new binary.stream(hmtx.data!);
				const	flags		= u8.get(hfile);
				const	metrics 	= binary.readn(hfile, u16, numMetrics).map(i => ({advance: i, bearing: 0}));
				const	minxs		= me.cached.map(i => i!.min.x);

				(flags & 1 ? minxs.slice(0, numMetrics) : binary.readn(hfile, s16, numMetrics))
					.forEach((i, index) => metrics[index].bearing = i);

				const bearing = !(flags & 2) ? binary.readn(hfile, s16, numGlyphs - numMetrics)	: minxs.slice(numMetrics);
				me.hmtx = {metrics, bearing};

			} else {
				me.hmtx = loadMetrics(hmtx.data!, numGlyphs, numMetrics);
			}
		}
		return me;
	}
}
