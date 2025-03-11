//-----------------------------------------------------------------------------
//	Load Font
//-----------------------------------------------------------------------------

import * as binary from '@isopodlabs/binary';
import {Font, FontGroup, TTF, TTC} from './font';
import {WOFF, WOFF2} from "./woff";

const TAG		= binary.StringType(4);

export async function fontLoad(data: Uint8Array): Promise<Font | FontGroup | undefined> {
	if (data.length < 256)
		return;

	const file		= new binary.stream(data);
	const tag		= TAG.get(file);
	switch (tag) {
		default:
			if (binary.utils.stringCode(tag) != 0x00000100)
				break;
			//fall through
		case 'true':
		case 'typ1':
		case 'OTTO':
			file.seek(0);
			return new TTF(file);

		case 'ttcf': return new TTC(file);
		case 'wOFF': return WOFF.load(file);
		case 'wOF2': return WOFF2.load(file);
	}
}