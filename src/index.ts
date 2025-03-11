import * as binary from '@isopodlabs/binary';
import {Font, FontGroup, TTF, TTC} from './font';
import {WOFF, WOFF2} from "./woff";

export {Font, FontGroup} from './font';

export function load(data: Uint8Array): Font | FontGroup | Promise<Font> | undefined {
	if (data.length < 256)
		return;

	const file		= new binary.stream(data);
	const tag		= binary.StringType(4).get(file);
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

import * as fs from 'fs/promises';

export async function loadFile(filename: string): Promise<Font | FontGroup | undefined> {
	return fs.readFile(filename).then(data => load(data));
}
