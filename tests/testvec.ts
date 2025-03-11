import {float2, float3} from '../src/vector';

interface equal<T> {
	equal(b: T): boolean;
}

function expect<T extends equal<T>>(v: T) {
	return {
		toEqual(v2: T) {
			if (!v.equal(v2))
				console.log("fail");
		}
	};
}

function test(name: string, fn: ()=>void) {
	console.log("testing: " + name);
	fn();
	console.log("finished: " + name);
}

const myVec3 = float3(1,2,3);

test('swizzle properties', () => {
	expect(myVec3.xy).toEqual(float2(1, 2));
	expect(myVec3.xz).toEqual(float2(1, 3));
	expect(myVec3.yx).toEqual(float2(2, 1));
	expect(myVec3.yz).toEqual(float2(2, 3));
	expect(myVec3.zx).toEqual(float2(3, 1));
	expect(myVec3.zy).toEqual(float2(3, 2));
  
	expect(myVec3.xyz).toEqual(float3(1, 2, 3));
	expect(myVec3.xzy).toEqual(float3(1, 3, 2));
	expect(myVec3.yxz).toEqual(float3(2, 1, 3));
	expect(myVec3.yzx).toEqual(float3(2, 3, 1));
	expect(myVec3.zxy).toEqual(float3(3, 1, 2));
	expect(myVec3.zyx).toEqual(float3(3, 2, 1));
});