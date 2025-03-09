
export interface color {
	r: number,
	g: number,
	b: number,
	a: number,
}

interface vec2<T> {
	x:	T,
	y:	T,
}
interface vec3<T> {
	x:	T,
	y:	T,
	z:	T,
}
interface vec4<T> {
	x:	T,
	y:	T,
	z:	T,
	w:	T,
}

function hasz<T>(a: vec3<T>|vec2<T>): a is vec3<T> { return 'z' in a; }
function hasw<T>(a: vec4<T>|vec3<T>): a is vec4<T> { return 'w' in a; }

abstract class ops<C extends ops<C>> {
	abstract neg(): 			C;
	abstract mul(b: number): 	C;
	abstract add(b: C): 		C;
	abstract sub(b: C): 		C;
	abstract min(b: C): 		C;
	abstract max(b: C): 		C;
	abstract equal(b: C): 		boolean;
	abstract dot(b: C): 		number;
	abstract perp(): 			C;

	mid(b: C) 		{ return this.add(b).mul(0.5); }
	lensq()			{ return this.dot(this as unknown as C);}
	len()			{ return Math.sqrt(this.lensq());}
}

//export function lensq<C extends ops<C>>(a: C)					{ return a.dot(a);}
//export function len<C extends ops<C>>(a: C)						{ return Math.sqrt(lensq(a));}
export function normalise<C extends ops<C>>(a: C)				{ return a.mul(1 / a.len());}
export function project<C extends ops<C>>(a: C, b: C)			{ return b.mul(a.dot(b) / b.lensq()); }
export function reflect<C extends ops<C>>(a: C, b: C)			{ return project(a, b).mul(2).sub(a); }
export function lerp<C extends ops<C>>(a: C, b: C, t: number)	{ return a.add(b.sub(a).mul(t)); }

class extent<C extends ops<C>> {
	constructor(
		public min:C,	// = C.Infinity,
		public max:C	// = C.Infinity.neg(),
	) {}
	add(p: C) {
		this.min = this.min.min(p);
		this.max = this.max.max(p);
	}
	combine(b: extent<C>) {
		this.min = this.min.min(b.min);
		this.max = this.max.max(b.max);
	}
	get ext() 	{ return this.max.sub(this.min); }
}

//-----------------------------------------------------------------------------
// 1D
//-----------------------------------------------------------------------------

export class extent1 {
	constructor(
		public min	= Infinity,
		public max	= -Infinity
	) {}
	add(p: number) {
		this.min = Math.min(this.min, p);
		this.max = Math.max(this.max, p);
	}
	combine(b: extent1) {
		this.min = Math.min(this.min, b.min);
		this.max = Math.max(this.max, b.max);
	}
	get ext() 	{ return this.max - this.min; }
}

//-----------------------------------------------------------------------------
// 2D
//-----------------------------------------------------------------------------

export class Float2 extends ops<Float2> {
	constructor(public x: number, public y: number) { super(); }
	neg()				{ return float2(-this.x, -this.y);}
	mul(b: number)		{ return float2(this.x * b, this.y * b);}
	add(b: Float2)		{ return float2(this.x + b.x, this.y + b.y);}
	sub(b: Float2)		{ return float2(this.x - b.x, this.y - b.y);}
	min(b: Float2)		{ return float2(Math.min(this.x, b.x), Math.min(this.y, b.y));}
	max(b: Float2)		{ return float2(Math.max(this.x, b.x), Math.max(this.y, b.y));}
	equal(b: Float2)	{ return this.x === b.x && this.y === b.y;}
	dot(b: Float2)		{ return this.x * b.x + this.y * b.y;}
	cross(b: Float2)	{ return this.x * b.y - this.y * b.x;}
	perp()				{ return float2(-this.y, this.x); }
};

export const float2 = Object.assign(
    function (this: any, x: number, y: number): Float2 {
        return new Float2(x, y);
	} as {
		(x: number, y: number): float2;		// Callable signature
		new (x: number, y: number): float2;	// Constructor signature
    },
    {
		C:	Float2,
        zero() { return float2(0, 0); },
		translate(z: float2): float2x3 {
			return float2x3(float2(1, 0), float2(0,1), z);
		},
		scale(s: {x: number, y: number}|number) {
			if (typeof s === 'number')
				s = float2(s, s);
			return float2x2(float2(s.x, 0), float2(0, s.y));
		},
		rotate(t: number) {
			const s = Math.sin(t);
			const c = Math.cos(t);
			return float2x2(float2(c, s), float2(-s, c));
		}
    }
);

export type float2		= Float2;
export type float2x2	= vec2<float2>;
export type float2x3	= vec3<float2>;
export type float2x4	= vec4<float2>;

export function float2x2(x: float2, y: float2): float2x2			{ return {x, y}; }
export function float2x3(x: float2, y: float2, z: float2): float2x3	{ return {x, y, z}; }
export function float2x4(m: float2x3, w: float2): float2x4			{ return {x: m.x, y: m.y, z: m.z, w}; }
export const identity2x3	= float2x3(float2(1, 0), float2(0, 1), float2(0, 0));

export function mul2x2(m: float2x2, v: float2)				{ return m.x.mul(v.x).add(m.y.mul(v.y)); }
export function mul2x3(m: float2x3|float2x2, v: float2)		{ const t = mul2x2(m, v); return hasz(m) ? t.add(m.z) : t; }

export function matmul2(a: float2x3, b: float2x2): float2x3;
export function matmul2(a: float2x2, b: float2x3): float2x3;
export function matmul2(a: float2x3, b: float2x3): float2x3;
export function matmul2(a: float2x2, b: float2x2): float2x2;
export function matmul2(a: float2x3|float2x2, b: float2x3|float2x2): float2x3|float2x2 {
	return {
		x: mul2x3(a, b.x),
		y: mul2x3(a, b.y),
		...(hasz(b) ? {z: mul2x3(a, b.z)} : hasz(a) ? {z: a.z} : {})
	};
}

export function det2x2(m: float2x2)	{
	return m.x.cross(m.y);
}

export function inverse2x2(m: float2x2)	{
	const r = 1 / det2x2(m);
	return float2x2(float2(m.y.y * r, -m.x.y * r), float2(-m.y.x * r, m.x.x * r));
}

export function inverse2x3(m: float2x3)	{
	const i = inverse2x2(m);
	return m.z ? float2x3(i.x, i.y, mul2x2(i, m.z)) : i;
}

export function sincos_half(sc: float2) {
	const x = Math.sqrt(0.5 * (1 + sc.x));
	const y = Math.sqrt(0.5 * (1 - sc.x));
	return float2(x, sc.y < 0 ? -y : y);
}

// returns point on unit circle where |M.v| is largest
export function max_circle_point(m: float2x2) {
	const d = m.x.dot(m.y);
	if (d) {
		const	sc2		= normalise(float2(m.x.x * m.x.x + m.x.y * m.x.y - m.y.x * m.y.x - m.y.y * m.y.y, d * 2));
		return sincos_half(sc2);
	}
	return float2(1, 0);
}
export class extent2 extends extent<float2> {
	constructor(
		min	= float2(Infinity, Infinity),
		max = float2(-Infinity, -Infinity),
	) {
		super(min, max);
	}
}
//-----------------------------------------------------------------------------
// 2D geometry
//-----------------------------------------------------------------------------

export interface circle {
	centre:	float2;
	radius:	number;
}

export function circle(centre: float2, radius: number) {
	return {centre, radius};
}

export function triangle(a: float2, b: float2, c: float2) {
	return float2x3(b.sub(a), c.sub(a), a);
}

//-----------------------------------------------------------------------------
// 3D
//-----------------------------------------------------------------------------

class Float3 extends ops<Float3> {
	constructor(public x: number, public y: number, public z: number) { super(); }
	neg()				{ return float3(-this.x, -this.y, -this.z);}
	mul(b: number)		{ return float3(this.x * b, this.y * b, this.z * b);}
	add(b: Float3)		{ return float3(this.x + b.x, this.y + b.y, this.z + b.z);}
	sub(b: Float3)		{ return float3(this.x - b.x, this.y - b.y, this.z - b.z);}
	min(b: Float3)		{ return float3(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z));}
	max(b: Float3)		{ return float3(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z));}
	equal(b: Float3)	{ return this.x === b.x && this.y === b.y && this.z === b.z;}
	dot(b: Float3)		{ return this.x * b.x + this.y * b.y + this.z * b.z;}
	cross(b: Float3)	{ return this.x * b.y - this.y * b.x;}
	perp()				{ return this; }//TBD
}

export const float3 = Object.assign(
    (x: number, y: number, z: number) => new Float3(x, y, z),
    {
        zero() { return float3(0, 0, 0); }
    }
);

export type float3 		= Float3;
export type float3x3	= vec3<float3>;
export type float3x4	= vec4<float3>;

export function float3x3(x: float3, y: float3, z: float3)	{ return {x, y, z}; }
export function float3x4(x: float3, y: float3, z: float3, w: float3)	{ return {x, y, z, w}; }
export const identity3x4	= float3x4(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1), float3(0, 0, 0));

export function mul3x3(m: float3x3, v: float3)		{ return m.x.mul(v.x).add(m.y.mul(v.y)).add(m.z.mul(v.z)); }
export function mul3x4(m: float3x4|float3x3, v: float3)		{ const t = mul3x3(m, v); return hasw(m) ? t.add(m.w) : t; }

export function matmul3(a: float3x4, b: float3x3): float3x4;
export function matmul3(a: float3x3, b: float3x4): float3x4;
export function matmul3(a: float3x4, b: float3x4): float3x4;
export function matmul3(a: float3x3, b: float3x3): float3x3;
export function matmul3(a: float3x4|float3x3, b: float3x4|float3x3): float3x4|float3x3 {
	return {
		x: mul3x3(a, b.x),
		y: mul3x3(a, b.y),
		z: mul3x3(a, b.z),
		...(hasw(b) ? {z: mul3x4(a, b.w)} : hasw(a) ? {z: a.w} : {})
	};
}

export function translate3(w: float3) {
	return float3x4(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1),w);
}

export function scale3(s: float3|number) {
	if (typeof s === 'number')
		s = float3(s, s, s);
	return float3x3(float3(s.x, 0, 0), float3(0, s.y, 0), float3(0, s.z, 0));
}
/* TBD
export function rotate3(t: number) {
}

export function det3x3(m: float3x4)	{
}

export function inverse3x3(m: float3x3)	{
}

export function inverse3x4(m: float3x4)	{
}
*/

export class extent3 extends extent<float3> {
	constructor(
		min	= float3(Infinity, Infinity, Infinity),
		max = float3(-Infinity, -Infinity, -Infinity),
	) {
		super(min, max);
	}
}

//-----------------------------------------------------------------------------
// 3d geometry
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// 4D
//-----------------------------------------------------------------------------

class Float4 extends ops<Float4> {
	constructor(public x: number, public y: number, public z: number, public w: number) { super(); }
	neg()				{ return float4(-this.x, -this.y, -this.z, -this.w);}
	mul(b: number)		{ return float4(this.x * b, this.y * b, this.z * b, this.w * b);}
	add(b: Float4)		{ return float4(this.x + b.x, this.y + b.y, this.z + b.z, this.w + b.w);}
	sub(b: Float4)		{ return float4(this.x - b.x, this.y - b.y, this.z - b.z, this.w - b.w);}
	min(b: Float4)		{ return float4(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.w), Math.min(this.z, b.w));}
	max(b: Float4)		{ return float4(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.w), Math.max(this.z, b.w));}
	equal(b: Float4)	{ return this.x === b.x && this.y === b.y && this.z === b.z && this.w === b.w;}
	dot(b: Float4)		{ return this.x * b.x + this.y * b.y + this.z * b.z + this.w * b.w;}
	cross(_b: Float4)	{ return this; }//TBD
	perp()				{ return this; }//TBD
}

export const float4 = Object.assign(
    (x: number, y: number, z: number, w: number) => new Float4(x, y, z, w),
    {
        zero() { return float4(0, 0, 0, 9); }
    }
);

export type float4		= Float4;
