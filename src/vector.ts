interface vec2<T> {
	x:	T,
	y:	T,
}
interface vec3<T> {
	x:	T,
	y:	T,
	z:	T,
}

interface vec3s<T, T2, T3> extends vec3<T> {
	xy:	T2;
	xz:	T2;
	yx:	T2;
	yz:	T2;
	zx:	T2;
	zy:	T2;
	xyz: T3;
	xzy: T3;
	yxz: T3;
	yzx: T3;
	zxy: T3;
	zyx: T3;
}

interface vec4<T> {
	x:	T,
	y:	T,
	z:	T,
	w:	T,
}

function add_swizzles3<T, T2, T3>(obj: vec3s<T, T2, T3>, make2: (x: T, y: T) => T2, make3: (x: T, y: T, z: T) => T3) {
    Object.defineProperty(obj, "xy", { get() { return make2(this.x, this.y); } });
    Object.defineProperty(obj, "xz", { get() { return make2(this.x, this.z); } });
    Object.defineProperty(obj, "yx", { get() { return make2(this.y, this.x); } });
    Object.defineProperty(obj, "yz", { get() { return make2(this.y, this.z); } });
    Object.defineProperty(obj, "zx", { get() { return make2(this.z, this.x); } });
    Object.defineProperty(obj, "zy", { get() { return make2(this.z, this.y); } });

    Object.defineProperty(obj, "xyz", { get() { return make3(this.x, this.y, this.z); } });
    Object.defineProperty(obj, "xzy", { get() { return make3(this.x, this.z, this.y); } });
    Object.defineProperty(obj, "yxz", { get() { return make3(this.y, this.x, this.z); } });
    Object.defineProperty(obj, "yzx", { get() { return make3(this.y, this.z, this.x); } });
    Object.defineProperty(obj, "zxy", { get() { return make3(this.z, this.x, this.y); } });
    Object.defineProperty(obj, "zyx", { get() { return make3(this.z, this.y, this.x); } });
}

function hasz<T>(a: vec3<T>|vec2<T>): a is vec3<T> { return 'z' in a; }
function hasw<T>(a: vec4<T>|vec3<T>): a is vec4<T> { return 'w' in a; }

interface ops<C extends ops<C>> {
	neg(): 			C;
	mul(b: number): C;
	vmul(b: C):		C;
	add(b: C): 		C;
	sub(b: C): 		C;
	min(b: C): 		C;
	max(b: C): 		C;
	equal(b: C): 	boolean;
	dot(b: C): 		number;
	perp(): 		C;
	lensq():		number;
	len():			number;
}

export function mid<C extends ops<C>>(a: C, b: C) 	{ return a.add(b).mul(0.5); }
export function lensq<C extends ops<C>>(a: C)		{ return a.dot(a);}
export function len<C extends ops<C>>(a: C)			{ return Math.sqrt(lensq(a));}

export function normalise<C extends ops<C>>(a: C)				{ return a.mul(1 / a.len());}
export function project<C extends ops<C>>(a: C, b: C)			{ return b.mul(a.dot(b) / b.lensq()); }
export function reflect<C extends ops<C>>(a: C, b: C)			{ return project(a, b).mul(2).sub(a); }
export function lerp<C extends ops<C>>(a: C, b: C, t: number)	{ return a.add(b.sub(a).mul(t)); }

class extent<C extends ops<C>> {
	constructor(public min:C, public max:C) {}
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

export interface float2 extends vec2<number>, ops<float2> {
	cross(b: float2): number;
}

export const float2 = Object.assign(
    function (this: float2, x: number, y: number) {
		if (!this)
			return new float2(x, y);
		this.x = x;
		this.y = y;
	} as {
		(x: number, y: number): float2;		// Callable signature
		new (x: number, y: number): float2;	// Constructor signature
    },
    {// statics
        zero() { return float2(0, 0); },
		translate(z: float2) {
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

Object.assign(float2.prototype, {
	neg(this: float2)				{ return float2(-this.x, -this.y);},
	mul(this: float2, b: number)	{ return float2(this.x * b, this.y * b);},
	vmul(this: float2, b: float2)	{ return float2(this.x * b.x, this.y * b.y);},
	add(this: float2, b: float2)	{ return float2(this.x + b.x, this.y + b.y);},
	sub(this: float2, b: float2)	{ return float2(this.x - b.x, this.y - b.y);},
	min(this: float2, b: float2)	{ return float2(Math.min(this.x, b.x), Math.min(this.y, b.y));},
	max(this: float2, b: float2)	{ return float2(Math.max(this.x, b.x), Math.max(this.y, b.y));},
	equal(this: float2, b: float2)	{ return this.x === b.x && this.y === b.y;},
	dot(this: float2, b: float2)	{ return this.x * b.x + this.y * b.y;},
	perp(this: float2)				{ return float2(-this.y, this.x); },
	lensq(this: float2)				{ return this.dot(this);},
	len(this: float2)				{ return Math.sqrt(this.lensq());},
	cross(this: float2, b: float2)	{ return this.x * b.y - this.y * b.x;},
});

export class extent2 extends extent<float2> {
	constructor(
		min	= float2(Infinity, Infinity),
		max = float2(-Infinity, -Infinity),
	) {
		super(min, max);
	}
}

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

//-----------------------------------------------------------------------------
// 2D geometry
//-----------------------------------------------------------------------------

export interface circle {
	centre:	float2;
	radius:	number;
}

export function circle(centre: float2, radius: number): circle {
	return {centre, radius};
}

export function triangle(a: float2, b: float2, c: float2) {
	return float2x3(b.sub(a), c.sub(a), a);
}

//-----------------------------------------------------------------------------
// 3D
//-----------------------------------------------------------------------------

export interface float3 extends vec3s<number, float2, float3>, ops<float3> {
	cross(b: float3): float3;
}

export const float3 = Object.assign(
    function (this: float3, x: number, y: number, z: number) {
		if (!this)
			return new float3(x,y,z);
		this.x = x;
		this.y = y;
		this.z = z;
	} as {
		(x: number, y: number, z: number): float3;		// Callable signature
		new (x: number, y: number, z: number): float3;	// Constructor signature
    },
    {
        zero() { return float3(0, 0, 0); },
		translate(w: float3) {
			return float3x4(float3(1, 0, 0), float3(0,1,0), float3(0,0,1), w);
		},
		scale(s: vec3<number> | number) {
			if (typeof s === 'number')
				s = float3(s, s, s);
			return float3x3(float3(s.x, 0, 0), float3(0, s.y, 0), float3(0, 0, s.z));
		},
    }
);

Object.assign(float3.prototype, {
	neg(this: float3)				{ return float3(-this.x, -this.y, -this.z);},
	mul(this: float3, b: number)	{ return float3(this.x * b, this.y * b, this.z * b);},
	vmul(this: float3, b: float3)	{ return float3(this.x * b.x, this.y * b.y, this.z * b.z);},
	add(this: float3, b: float3)	{ return float3(this.x + b.x, this.y + b.y, this.z + b.z);},
	sub(this: float3, b: float3)	{ return float3(this.x - b.x, this.y - b.y, this.z - b.z);},
	min(this: float3, b: float3)	{ return float3(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z));},
	max(this: float3, b: float3)	{ return float3(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z));},
	equal(this: float3, b: float3)	{ return this.x === b.x && this.y === b.y && this.z === b.z;},
	dot(this: float3, b: float3)	{ return this.x * b.x + this.y * b.y + this.z * b.z;},
	perp(this: float3)				{ return this; },//TBD
	lensq(this: float3)				{ return this.dot(this);},
	len(this: float3)				{ return Math.sqrt(this.lensq());},
	cross(this: float3, b: float3)	{ return (b.vmul(this.zxy).sub(this.vmul(b.zxy))).zxy; }
});
//add_swizzles3(float3.prototype, (x, y) => ({ x, y }), (x, y, z) => ({ x, y, z }));
add_swizzles3(float3.prototype, float2, float3);

export class extent3 extends extent<float3> {
	constructor(
		min	= float3(Infinity, Infinity, Infinity),
		max = float3(-Infinity, -Infinity, -Infinity),
	) {
		super(min, max);
	}
}

export type float3x3	= vec3<float3>;
export type float3x4	= vec4<float3>;

export function float3x3(x: float3, y: float3, z: float3): float3x3	{ return {x, y, z}; }
export function float3x4(x: float3, y: float3, z: float3, w: float3): float3x4	{ return {x, y, z, w}; }
export const identity3x4	= float3x4(float3(1, 0, 0), float3(0, 1, 0), float3(0, 0, 1), float3(0, 0, 0));

export function mul3x3(m: float3x3, v: float3)				{ return m.x.mul(v.x).add(m.y.mul(v.y)).add(m.z.mul(v.z)); }
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

export function det3x3(m: float3x3)	{
	return m.x.dot(m.y.cross(m.z));
}

export function transpose3x3(m: float3x3)	{
	return float3x3(
		float3(m.x.x, m.y.x, m.z.x),
		float3(m.x.y, m.y.y, m.z.y),
		float3(m.x.z, m.y.z, m.z.z)
	);
}

export function inverse3x3(m: float3x3)	{
	const cofactors = float3x3(
		m.y.cross(m.z),
		m.z.cross(m.x),
		m.x.cross(m.y)
	);
	const rdet = 1 / m.x.dot(cofactors.x);
	return transpose3x3(float3x3(
		cofactors.x.mul(rdet),
		cofactors.y.mul(rdet),
		cofactors.z.mul(rdet)
	));
}

export function inverse3x4(m: float3x4)	{
	const i = inverse3x3(m);
	return float3x4(i.x, i.y, i.z, mul3x3(i, m.w));
}


//-----------------------------------------------------------------------------
// 3d geometry
//-----------------------------------------------------------------------------


//-----------------------------------------------------------------------------
// 4D
//-----------------------------------------------------------------------------


export interface float4 extends vec4<number>, ops<float4> {}

export const float4 = Object.assign(
    function (this: float4, x: number, y: number, z: number, w: number) {
		this.x = x;
		this.y = y;
		this.z = z;
		this.w = w;
	} as {
		(x: number, y: number, z: number, w: number): float4;		// Callable signature
		new (x: number, y: number, z: number, w: number): float4;	// Constructor signature
    },
    {
        zero() { return float4(0, 0, 0, 0); },
		scale(s: vec4<number> |number) {
			if (typeof s === 'number')
				s = float4(s, s, s, s);
			return float4x4(float4(s.x, 0, 0, 0), float4(0, s.y, 0, 0), float4(0, 0, s.z, 0), float4(0, 0, 0, s.w));
		},
    }
);

Object.assign(float4.prototype, {
	neg(this: float4)				{ return float4(-this.x, -this.y, -this.z, -this.w);},
	mul(this: float4, b: number)	{ return float4(this.x * b, this.y * b, this.z * b, this.w * b);},
	add(this: float4, b: float4)	{ return float4(this.x + b.x, this.y + b.y, this.z + b.z, this.w + b.w);},
	sub(this: float4, b: float4)	{ return float4(this.x - b.x, this.y - b.y, this.z - b.z, this.w - b.w);},
	min(this: float4, b: float4)	{ return float4(Math.min(this.x, b.x), Math.min(this.y, b.y), Math.min(this.z, b.z), Math.min(this.w, b.w));},
	max(this: float4, b: float4)	{ return float4(Math.max(this.x, b.x), Math.max(this.y, b.y), Math.max(this.z, b.z), Math.max(this.w, b.w));},
	equal(this: float4, b: float4)	{ return this.x === b.x && this.y === b.y && this.z === b.z && this.w === b.w;},
	dot(this: float4, b: float4)	{ return this.x * b.x + this.y * b.y + this.z * b.z + this.w * b.w;},
	perp(this: float4)				{ return this; },//TBD
	lensq(this: float4)				{ return this.dot(this);},
	len(this: float4)				{ return Math.sqrt(this.lensq());},
});

export type float4x4	= vec4<float4>;
export function float4x4(x: float4, y: float4, z: float4, w: float4): float4x4	{ return {x, y, z, w}; }