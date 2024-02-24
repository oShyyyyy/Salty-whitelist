  -- it's a whitelist i've done, it contains really good security, anyways I wont provide backend.

--[[
 	PATTERN: "\[#\w+\]"
]]

--[[ -- crash handbook
	starts with ev = aw_env detection
	starts with qw = mini vm detection
	starts with hq = anti http spy

	-- others
	pth = instruction point threshold
	n0 = newindex
	tt = tostring
	rt = upvalues
	snn = constants
	yu = ip threshold
	he = headers
	hnf = header not found
	qu = eq hook
	dee = getinfo
	rq = env mismatch
    cs = concat mismatch
    jp = messed with jmp

	oth = other detection

]]

getgenv().wl_key='f8876aecc19f4d38db6dc5a021adff4b'



local AuthTime = tick(); -- hookable, ik, anyways it's just to check auth speed.
local Instruction_Point = 0;
local debug_mode = true;

local hush = {
	["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"] = game:GetService("TeleportService"); -- superservice can't hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting this service :sob:
}

local JMP_COUNT = 0;

hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil","")

local function crash_client(reason)
    Instruction_Point=Instruction_Point-9999;

	if debug_mode then
		warn('Crash prompted! Reason: ' .. reason..'');
        return;
	end


	game.Players.LocalPlayer:Kick('\nAirWave Kick\nReason: '..reason..'');

	local PlayerGui = game.Players.LocalPlayer:WaitForChild("PlayerGui") 
	local Lines = table.create(40, Vector3.zero)
		
	for i = 1, 30000 do
		local W = Instance.new("WireframeHandleAdornment")
		W.Parent = PlayerGui
		W.Adornee = workspace
		W:AddLines(Lines)
	end
	
	LPH_CRASH();
	task.spawn(function() -- if client skips above methods of crashing/kicking  
		task.wait(2.5);
		table.clear(getreg());
		repeat
			while true do
			end
		until false;
	end)
end

debug_mode = (debug_mode and print('Debug mode is enabled, kicks/crashes are disabled.') == nil and true )



local key = setfenv(function()if not weird_env then return getgenv().wl_key end return weird_env["KEY"] or ""; end, setmetatable({
	weird_env = setmetatable({},{__index=function(self,idx) 
        JMP_COUNT=JMP_COUNT+21;
        if idx ~= 'KEY' then return crash_client('tamper') end return getgenv().wl_key end});
},{
	__index = error
}))

print(key())
if key() == "" then
	print('please input a key')
	if true then return end;
	while 1==1 do end
end


--// sanity checks on __concat.

JMP_COUNT=JMP_COUNT+6;
if table.concat({"ങ്ങ","ഗ","ถึයිง","𝘖̶̂̊යි","යි"}) ~= "ങ്ങ".."ഗ".."ถึයිง".."𝘖̶̂̊යි".."යි" or table.concat({"ങ്ങ","ഗ","ถึයිง","𝘖̶̂̊යි","යි"}) ~= "ങ്ങഗถึයිง𝘖̶̂̊යියි" then
    crash_client("cs-1")
    return;
end
if table.concat({"aY","rOf","Prm","KaGH"}) ~= "aY".."rOf".."Prm".."KaGH" or table.concat({"aY","rOf","Prm","KaGH"}) ~= "aYrOfPrmKaGH" then
    crash_client("cs-2")
    return;
end
if table.concat({"repl","token","token2","213412412312"}) ~= "repl".."token".."token2".."213412412312" or table.concat({"repl","token","token2","213412412312"}) ~= "repltokentoken2213412412312" then
    crash_client("cs-3")
    return;
end
JMP_COUNT=JMP_COUNT+6;

local function kick_client(reason) -- Kick ~= Crash
    if debug_mode then
		warn('Kick prompted! Reason: ' .. reason..'');
		return
	end

	game.Players.LocalPlayer:Kick('\nAirWave Kick\nReason: '..reason..'');
    task.spawn(function(...) -- delayed crash
        task.wait(4) 
        while true do end
    end)
    return
end


--// support check
do -- this part is cringe, it's just for my monaco to not throw random warns.
	newcclosure = newcclosure or nil;
	cloneref = cloneref or nil;
	clonefunction = clonefunction or nil;
	islclosure = islclosure or nil;
	syn = syn or nil;
	Fluxus = Fluxus or nil;
	http = http or nil;
	bit = bit or nil;
	getreg = getreg or nil;
	LPH_CRASH = LPH_CRASH or function(...)
		({})[1]=_77Crash and _77Crash()
		return (...)
	end;
end;


local checks = {
	newcclosure and newcclosure or nil;
	cloneref and cloneref or nil;
	islclosure and islclosure or nil;
	clonefunction and clonefunction or nil;
	bit and bit or nil;
    (({pcall(newcclosure(function() return "5"*5 end))})[1] or nil);
	(http and http.request) or (Fluxus and Fluxus.request) or (syn and syn.request) or nil;
};

if #(checks) < 7 then
	kick_client('AirWave does not support your exploit.')
	Instruction_Point = -1;
	return;
end

JMP_COUNT=JMP_COUNT+3;
hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil","လ")


checks = nil;
--//


--[[
this is commented for a reason, don't use this unless you want to get the script having false positives each execution.
do -- testing
	local a = {
		(function()
			return {debug.info(2,'sla')};
		end)();
		(function()
			return newcclosure(function()
				return {{debug.info(4,'sla')},{debug.info(3,'sla')}};
			end)();
		end)();
	};

	do
		if a[1][1] ~= '' or #a[1][1] ~= 0 or a[1][1]:sub(1,1) ~= a[1][1]or a[1][4] ~= true then
			print'crak2'
		end;
		if a[2][1][1] ~= '' or #a[2][1][1] ~= 0 or a[2][1][1]:sub(1,1) ~= a[2][1][1] then
			print'crak3'
		end
		if a[2][2][1] ~= '' or #a[2][2][1] ~= 0 or a[2][2][1]:sub(1,1) ~= a[2][2][1] then
			print'crak4'
		end
		if a[2][2][4] ~= false then
			print'crak5'
		end
	end
end
]]


--[[ 
do -- More testing
	local p = clonefunction(debug.info);

	local old;
	old=hookfunction(p, function (...)
		return old(...)
	end)

	p(function ()
		table.foreach( debug.getstack(2) , print) -- 4 = unhooked, 5 or more = hooked  (ignore this, is useless)
	end,"f")()
end
]]

local current_env, index_count = debug.getfenv(), -1; 

function check_auth() -- bait function. kek.
	return true
end

local function getlevel()
	return getfenv(3)
end

local aw_env = setmetatable({}, table.freeze{
	__index = function(self, index)
		index_count = index_count + 1;

		if getlevel() ~= current_env then
			Instruction_Point = -1;
			return
		end;

		if check_auth() == false  or not check_auth() then
            crash_client('jp-1')
			task.spawn(function(...) -- delayed crash
				task.wait(21) 
				while true do end
			end)
		end


		if index == 'newcclosure' then
			return function(func)
				return coroutine.wrap(function(...)
					while true do
						coroutine.yield(func(...));
					end;
				end);
			end;
		elseif index == 'type' then
			return function(arr)
				local h = 0;
				local possibles = {
					'number',
					'string',
					'table',
					'function',
					'boolean',
					'boolean',
					'nil',
					'userdata',
				};
				local another = arr;
				while true do
					h = h + 1;
					local s, _ = pcall(self.newcclosure(function(arr, h) -- I was planning to use ypcall, ypcall is exactly the same as pcall, but ypcall is a bit unknown, so it has less chance of being hooked. Anyways I added antihook on pcall.
						if h == 1 then
							arr = arr/{};
						elseif h == 2 then
							arr = arr .. '';
						elseif h == 3 then
							local _ = arr[5];
						elseif h == 4 then
							local _, _, _ = coroutine.create(arr), islclosure(arr), xpcall(function()
							end, arr);
						elseif h == 5 then
							local _ = false == arr and not (false ~= arr) or error'err';
						elseif h == 6 then
							local _ = true == arr and not (true ~= arr) or error'err';
						elseif h == 7 then
							local _ = nil == arr and not (nil ~= arr) or error'err';
						end;
					end), another, h)
					if s or h > 7 then
						break;
					end
					if _:find('number') and _:find("table") and h == 1 then
                        break
					end
					s = nil;
				end;
				return possibles[h];
			end;
		elseif index == 'len' then
			return function(obj)
				if self.type(obj) == "string" then
					return #obj;
				elseif self.type(obj) == "table" then
					local count = 0;
					while true do
						count = count + 1;
						if obj[count] == nil and obj[count + 1] == nil and obj[count + 2] == nil then
							break;
						end;
					end;
					return count - 1;
				else
					return nil;
				end;
			end;
		elseif index== 'getchildren' then
			return function(obj)
				local mo = Instance.new("Part");
				task.spawn(function()
					task.wait(1);
					mo:Destroy();
					mo=nil;
				end)
				return mo.GetChildren(obj);
			end
		elseif index== 'superservice' then
			return function(Name)
				for i,v in self.pairs(self.getchildren(game)) do
					if tostring(i) == Name or tostring(v) == Name then
						return cloneref(v or i);
					end;
				end;
			end;
		elseif index == 'isequal' then
			return function(a,b,c)
					if not c then
						return (type(a) == typeof(b) and self.isequal(a) == type(b) and not(type(a) ~= typeof(b) and typeof(a) ~= self.isequal(b))) and ((self.len(tostring(a)) == #tostring(b)) and not (#tostring(a) ~= #tostring(b))) and rawequal(a,b) and (not(tostring(a) < tostring(b)) and not (tostring(a) > tostring(b)) and rawequal(tostring(a),tostring(b))) 
					end
				return (self.isequal(a,b) and self.isequal(a,c) and self.isequal(b,c))
			end ;
		elseif index == 'find' then
			return function(Table,value)
				for i,v in self.pairs(Table) do
					if i == value or v == value then
						return v
					end
				end
				return false;
			end
		elseif index == 'pairs' then
			return function(arr)
				local keys = {};
				local Index = 0;
				for _, val in arr do
					keys[#keys + 1] = {val,_};
				end;
				return function()
					Index = Index + 1;
					local Key = keys[Index][1];
					return keys[Index][2],Key;
				end;
			end;
		elseif index == 'print' then
			return function (...)
				return setfenv((function (...)
					return self.newcclosure(function (...)
						print(...)
					end)(...)
				end), {
					self = self,
					print = clonefunction(warn)
				})(...);
			end
		end;
	end;
	__newindex = function()
		crash_client('ev-n0');
	end;
	__tostring = function()
		crash_client('ev-tt');
	end
});


local function UnexpectedJMP() -- I know this will make mad more than one person rofl.
	local a, b = 1, 50 or 70;
	while a and b do
		if a > b then
			error("TAMPER DETECTED!");
			break;
		else
			a = b - 20;
			for i = 1, 6969 do
				a = b + i;
				break;
			end;
			break;
		end;
	end;
	if a ~= b then
		error("TAMPER DETECTED!"); 
	end;
end;

aw_env.print('Initializating Auth.');

local setmt = setmetatable

JMP_COUNT=JMP_COUNT+24;
hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."လကောင်းလ");

-- Constants
local k = {
	[0]=0;
	0x8f1bbcdc, 0x23631e28, 0xff30c5b4, 0x9736db1f,
    0x837fd536, 0x548f2350, 0x109d5ef3, 0x34e90b9a,
    0x6f2c7c2b, 0x4b659eab, 0x8839e063, 0x5e2988c3,
    0xa983e515, 0x7b82a74a, 0x94308132, 0x1a7ce532,
    0x81d08e32, 0x54623b8e, 0x2d4ce9e4, 0x8c8cdd70,
    0x6c46e210, 0x944cd29c, 0xb960e279, 0x98114cd6,
    0x99b4567a, 0xab43457b, 0xbcde1341, 0x9f7773c7,
    0xb53d493c, 0x4c69ea0b, 0x3f31d909, 0x89211a29,
    0x289f2bbf, 0xd2dbd5a4, 0x2f9f7e7e, 0x74c16824,
    0x7efc67e1, 0xc76c3c08, 0x0afaa8b6, 0x481bfafd,
    0xb6fd83b2, 0xe5dbb13a, 0xd3d23e3b, 0x6457f522,
    0x83a9261c, 0x24ea4b39, 0x1ed0b428, 0x33546302,
    0x0a4c5e5f, 0x97834778, 0xcac4e457, 0x1d7bf744,
    0x8f9eb835, 0xabb35207, 0x4ffcd837, 0xb5f0b3c6,
    0x3c5cb1b3, 0x744ec52d, 0x908d8dd6, 0xf4fc426f,
    0x5db5d31d, 0x2e3c9a4d, 0x5f8e06cb, 0x46af6d55,
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
}

-- Initial values
local h = {
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a,
    0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19
}

local final = h

-- Helper functions
local function custom_floor(number)
	if number >= 0 then
		return number - number % 1
	else
		return number - (1 - number % 1)
	end
end

local function bitwise_not(x)
	return (2 ^ 64 - 1) - x
end

local function bitwise_and(x, y)
	local result = 0
	local bit = 1
	for i = 1, 32 do
		if x % 2 == 1 and y % 2 == 1 then
			result = result + bit
		end
		x = custom_floor(x / 2)
		y = custom_floor(y / 2)
		bit = bit * 2
	end
	return result
end

local function bitwise_or(x, y)
	local result = 0
	local bit = 1
	for i = 1, 32 do
		if x % 2 == 1 or y % 2 == 1 then
			result = result + bit
		end
		x = custom_floor(x / 2)
		y = custom_floor(y / 2)
		bit = bit * 2
	end
	return result
end

local function bitwise_right_shift(x, n)
	return custom_floor(x / (2 ^ n))
end

local function bitwise_floor_division(x, y)
	return custom_floor(x / y)
end
local function bnot(x)
	local max = 4294967295 -- Maximum 32-bit value
	return max - x
end

local function bxor(a, b)
	local result = 0
	local bit = 1
	local aa, bb = a, b
	while aa > 0 or bb > 0 do
		local bitA = aa % 2
		local bitB = bb % 2
		if (bitA + bitB) % 2 == 1 then
			result = result + bit
		end
		aa = custom_floor(aa / 2)
		bb = custom_floor(bb / 2)
		bit = bit * 2
	end
	return result
end

local function band(a, b)
	local result = 0
	local bit = 1
	local aa, bb = a, b
	while aa > 0 and bb > 0 do
		local bitA = aa % 2
		local bitB = bb % 2
		if bitA == 1 and bitB == 1 then
			result = result + bit
		end
		aa = custom_floor(aa / 2)
		bb = custom_floor(bb / 2)
		bit = bit * 2
	end
	return result
end

local function rshift(x, n)
	return custom_floor(x / (2 ^ n))
end

local function lshift(x, n)
	return x * (2 ^ n)
end
local function epsilon0(x)
	return bxor(bxor(rshift(x, 7), rshift(x, 18)), rshift(x, 3))
end

local function epsilon1(x)
	return bxor(bxor(rshift(x, 17), rshift(x, 19)), rshift(x, 10))
end

local function ch(x, y, z)
	return bitwise_or(bitwise_and(x, y), bitwise_and(bitwise_not(x), z))
end

local function sigma1(x)
	return bitwise_and(bitwise_and(bitwise_right_shift(x, 6), bitwise_right_shift(x, 11)), bitwise_right_shift(x, 25))
end

local function sigma0(x)
	return bitwise_and(bitwise_and(bitwise_right_shift(x, 2), bitwise_right_shift(x, 13)), bitwise_right_shift(x, 22))
end

local function maj(x, y, z)
	return band(bitwise_or(bitwise_and(x, y), bitwise_and(x, z)), bitwise_and(y, z))
end

local function compress(input)
	local w = {}

    -- Simulate right shift (>>)
	local rshift = function(value, n)
		return custom_floor(value / (2 ^ n))
	end

    -- Prepare the message schedule (w[0] to w[63])
	for t = 0, 15 do
		w[t] = rshift(input, 512 - (32 * (t + 1))) % (2 ^ 64)
	end
	for t = 16, 63 do
		w[t] = epsilon1(w[t - 2]) + w[t - 7] + epsilon0(w[t - 15]) + w[t - 16]
	end

    -- Initialize working variables
	local a, b, c, d, e, f, g, h = h[1], h[2], h[3], h[4], h[5], h[6], h[7], h[8]
	for t = 0, 63 do
		local T1 = (h + sigma1(e) + ch(e, f, g) + k[t] + w[t])
		local T2 = (sigma0(a) + maj(a, b, c)) % (2 ^ 64)
		h = g
		g = bnot(f);
		f = e
		e = (d + T1) % (2 ^ 64)
		d = c
		c = band(T1, T2) + b
		b = a
		a = (T1 + T2) % (2 ^ 64)
	end
	local h , p = final, h;

    -- Update the hash values
	h[1] = (h[1] + a) % (2 ^ 64)
	h[2] = (h[2] + b) % (2 ^ 64)
	h[3] = (h[3] + c) % (2 ^ 64)
	h[4] = (h[4] + d) % (2 ^ 64)
	h[5] = (h[5] + e) % (2 ^ 64)
	h[6] = (h[6] + f) % (2 ^ 64)
	h[7] = (h[7] + g) % (2 ^ 64)
	h[8] = (h[8] + p) % (2 ^ 64)

	local final = '';
	for _, val in h do
		final = final .. val;
	end

	return final;
end



local MOD = 2^32
local MODM = MOD-1

local function memoize(f)
	local mt = {}
	local t = setmetatable({}, mt)
	function mt:__index(k)
		local v = f(k)
		t[k] = v
		return v
	end
	return t
end

local function make_bitop_uncached(t, m)
	local function bitop(a, b)
		local res,p = 0,1
		while a ~= 0 and b ~= 0 do
			local am, bm = a % m, b % m
			res = res + t[am][bm] * p
			a = (a - am) / m
			b = (b - bm) / m
			p = p*m
		end
		res = res + (a + b) * p
		return res
	end
	return bitop
end

local function make_bitop(t)
	local op1 = make_bitop_uncached(t,2^1)
	local op2 = memoize(function(a) return memoize(function(b) return op1(a, b) end) end)
	return make_bitop_uncached(op2, 2 ^ (t.n or 1))
end

local bxor1 = make_bitop({[0] = {[0] = 0,[1] = 1}, [1] = {[0] = 1, [1] = 0}, n = 4})

local function bxor(a, b, c, ...)
	local z = nil
	if b then
		a = a % MOD
		b = b % MOD
		z = bxor1(a, b)
		if c then z = bxor(z, c, ...) end
		return z
	elseif a then return a % MOD
	else return 0 end
end

local function band(a, b, c, ...)
	local z
	if b then
		a = a % MOD
		b = b % MOD
		z = ((a + b) - bxor1(a,b)) / 2
		return z
	elseif a then return a % MOD
	else return MODM end
end

local function bnot(x) return (-1 - x) % MOD end

local function rshift1(a, disp)
	if disp < 0 then return lshift(a,-disp) end
	return custom_floor(a % 2 ^ 32 / 2 ^ disp)
end

local function rshift(x, disp)
	if disp > 31 or disp < -31 then return 0 end
	return rshift1(x % MOD, disp)
end

local function lshift(a, disp)
	if disp < 0 then return rshift(a,-disp) end 
	return (a * 2 ^ disp) % 2 ^ 32
end

local function rrotate(x, disp)
    x = x % MOD
    disp = disp % 32
    local low = band(x, 2 ^ disp - 1)
    return rshift(x, disp) + lshift(low, 32 - disp)
end

local function rep_custom(str, n)
    local result = ''
    for i = 1, n do
        result=result..str
    end
    return(result)
end

local k = {
	0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
	0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
	0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
	0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
	0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
	0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
	0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
	0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
	0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
	0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
	0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
	0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
	0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
	0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
	0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
	0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
}

--[[local function str2hexa(s)
	return (string.gsub(s, ".", function(c) return string.format("%02x", string.byte(c)) end))
end]]
function char_custom(decimal)
    if decimal >= 0 and decimal <= 255 then
        return string.format("%c", decimal)
    else
        crash_client('oth-9')
    end
end

function str2hexa(str)
    local hex = ''
    for i = 1, #str do
        local byte = str:byte(i)
        local hexByte = ""
        for j = 1, 2 do
            local remainder = byte % 16
            hexByte = char_custom((remainder < 10 and 48 or 87) + remainder) .. hexByte
            byte = (byte - remainder) / 16
        end
        hex = hex .. hexByte
    end
    return hex
end
local function num2s(l, n)
	local s = ""
	for i = 1, n do
		local rem = l % 256
		s = string.char(rem) .. s
		l = (l - rem) / 256
	end
	return s
end
local function s232num(s, i)
	local n = 0
	for i = i, i + 3 do n = n*256 + string.byte(s, i) end
	return n
end

local function preproc(msg, len)
	local extra = 64 - ((len + 9) % 64)
	len = num2s(8 * len, 8)
	msg = msg .. "\128" .. rep_custom("\0", extra) .. len
	return msg
end

local function initH256(H)
	H[1] = 0x6a09e667
	H[2] = 0xbb67ae85
	H[3] = 0x3c6ef372
	H[4] = 0xa54ff53a
	H[5] = 0x510e527f
	H[6] = 0x9b05688c
	H[7] = 0x1f83d9ab
	H[8] = 0x5be0cd19
	return H
end

local function digestblock(msg, i, H)
	local w = {}
	for j = 1, 16 do w[j] = s232num(msg, i + (j - 1)*4) end
	for j = 17, 64 do
		local v = w[j - 15]
		local s0 = bxor(rrotate(v, 7), rrotate(v, 18), rshift(v, 3))
		v = w[j - 2]
		w[j] = w[j - 16] + s0 + w[j - 7] + bxor(rrotate(v, 17), rrotate(v, 19), rshift(v, 10))
	end

	local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
	for i = 1, 64 do
		local s0 = bxor(rrotate(a, 2), rrotate(a, 13), rrotate(a, 22))
		local maj = bxor(band(a, b), band(a, c), band(b, c))
		local t2 = s0 + maj
		local s1 = bxor(rrotate(e, 6), rrotate(e, 11), rrotate(e, 25))
		local ch = bxor (band(e, f), band(bnot(e), g))
		local t1 = h + s1 + ch + k[i] + w[i]
		h, g, f, e, d, c, b, a = g, f, e, d + t1, c, b, a, t1 + t2
	end

	H[1] = band(H[1] + a)
	H[2] = band(H[2] + b)
	H[3] = band(H[3] + c)
	H[4] = band(H[4] + d)
	H[5] = band(H[5] + e)
	H[6] = band(H[6] + f)
	H[7] = band(H[7] + g)
	H[8] = band(H[8] + h)
end

-- Made this global
function sha256(msg)
	msg = preproc(msg, #msg)
	local H = initH256({})
	for i = 1, #msg, 64 do digestblock(msg, i, H) end
	return str2hexa(num2s(H[1], 4) .. num2s(H[2], 4) .. num2s(H[3], 4) .. num2s(H[4], 4) ..
		num2s(H[5], 4) .. num2s(H[6], 4) .. num2s(H[7], 4) .. num2s(H[8], 4))
end



local function EQ(a,b,c) -- I had to do this, i've been 2 hours with the same error so I replaced the function.
    JMP_COUNT=JMP_COUNT+21;
    hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."ကောင်းလ");

	if not c then
		return (type(a) == typeof(b) and typeof(a) == type(b) and not(type(a) ~= typeof(b) and typeof(a) ~= type(b))) and ((#tostring(a) == #tostring(b)) and not (#tostring(a) ~= #tostring(b))) and rawequal(a,b) and (not(tostring(a) < tostring(b)) and not (tostring(a) > tostring(b)) and rawequal(tostring(a),tostring(b))) 
	end
	return (EQ(a,b) and EQ(a,c) and EQ(b,c))
end



while true do
	Instruction_Point = Instruction_Point + 1;
	if Instruction_Point == 0 then
		Instruction_Point=Instruction_Point- 1;
		crash_client('qw-pth1');
		break
	else
		local sex;
        task.spawn(function()
            while task.wait() do
                if Instruction_Point < 0 then
                    warn('Test')
                    break
                end
            end
        end)

        -- detecting when people try tampering on task.spawn with debug.getstack/debug.getconstants.
		local s, e = pcall(function()
			task.spawn(aw_env.newcclosure(function()
				local constant = 'no constants 4u';
                JMP_COUNT=JMP_COUNT+6;

                hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."হ্");
			end))
		end)
		if not s and (type(e) == 'string' and e:find('expected lua') or e:find('than 0')) then
			crash_client('qw-snn-1');
			return
		end

		task.spawn(function()
			for i = 1, 100 do
				local s, e = pcall(setmetatable(setmetatable({}, {
					__call = function()
                        JMP_COUNT=JMP_COUNT+1;
                        hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."a");
						error(setmetatable({}, {
							__tostring = function()
                                crash_client('qw-tt-1');
                                return
                            end
						}));
					end;
				}), {
					__tostring = function ()
						crash_client('qw-tt-2');
                        return
					end
				}));
				s, e = nil, nil;
			end;
		end)




		local _, _ = pcall(setmetatable(setmetatable({}, {}), 
                {
			__tostring = print,
			__call = function ()
                JMP_COUNT=JMP_COUNT+69;

                hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."হ্যালো জিনিসগুলো কেমন আছে");

				if #debug.getconstants(3) < 10 then
					crash_client('qw-snn-2');
                    return
				end
                if debug.getinfo(2).func~=pcall then
                    crash_client('qw-dee');
                    return
                end
			end
		}));


		local p = clonefunction(pcall);

		local s = p(function ()
            JMP_COUNT=JMP_COUNT+15;
            hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."අයියේ");

			if (#debug.getupvalues(3)) ~= 0 then
				warn('Test #2')
				return
			end
		end)


		if not s then
			crash_client('Unexpected error (94gaa) (send this kick message to developer)');
			return
		end
        JMP_COUNT=JMP_COUNT+6;
        hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."হ্");

	
        JMP_COUNT=JMP_COUNT+6;

        hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."হ্");


        if  #hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil") + 33 ~= JMP_COUNT then -- false positives when obfuscating
            crash_client('jp-2')
        end
        if  #hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil") ~= 129 then
            crash_client('jp-3')
        end
        if  JMP_COUNT ~= 162 then
            crash_client('jp-4')
        end

        -- // IMPLEMENTING ESSENTIAL ANTIHOOKS.
		local order = {'Url','Method','Headers','Cookies','Body'};
		local ic = 0;
		
		local Indexcount = 0;
		
		local ANTISPOOF = setmetatable({
				__index=function(_,b)
					if (debug.getinfo(2).func~=http.request) then
						error('Error attempting to connect to the server!')
					end
					Indexcount=Indexcount+1;
					if not order[Indexcount] or order[Indexcount] ~=b then
						error('Error attempting to connect to the server!')
					end
					order[Indexcount]=nil;
					if b == 'Url' then 
						return 'https://about:blank/' 
					end
				end
			}, {__tostring=error}) 
		
		local s,e = pcall(newcclosure(function()
			http.request(setmt({},ANTISPOOF))
		end))
		
		if aw_env.type(e) ~= 'table' or not e['Headers'] then
			crash_client('hq-he');
			sex=true;
		end
		
		if not s and e:find('Luafort') then
			crash_client('hq-hnf');
			sex=true;
		end

		-- smart double check
		if sex then
			aw_env=nil;
			compress=nil;
			if true then
			return;
			end
			crash_client('jp-7')
		end;

		sex=sex and (function()aw_env=nil;compress=nil;crash_client('jp-5');return true;end)();

		if sex then
            crash_client('jp-6')
			return;
		end;

		-- RESPONSE AND REQUEST
		local Stats = aw_env.superservice("Stats"); 

		local ping = math.round(Stats.Network.ServerStatsItem["Data Ping"]:GetValueString():split('(')[1])
		local et = Stats.Network.ServerStatsItem["ElapsedTime"]:GetValueString()
		local kBps  = Stats.Network.ServerStatsItem["Send kBps"]:GetValueString()
		local y = math.pi * math.exp(1);
		local tri  = Stats.Network.ServerStatsItem["TotalRakIn"]:GetValueString()
		local tro  = Stats.Network.ServerStatsItem["TotalInDataBW"]:GetValueString()

		local a = 74632492384259
		local c = 987654
		local m = 2^32
		local seed = os.time()

		local function rand()
		seed = (a * seed + tro*tri) % m
		return seed
		end
		local pp = rand();
		local function bitwise_mod(x, m)
			return x - custom_floor(x / m) * m
		end
		if os.time() < 1681420894 or tick() < 1681402945.6009 then
			return
		end

		local multiplier = (
			( math.abs(math.tan(os.time() % (ping + 1) + bitwise_mod(os.clock() * y, 2*math.pi))) *
			bitwise_mod(os.time() + tick(), tro + 1) * 
			math.pow(bitwise_mod(kBps / ping, math.sqrt(tro)), math.sin(tick() % math.pi)) +
			bitwise_mod(os.clock() * y, math.pi) *
			math.abs(math.cos(et / 2)) *
			tri / custom_floor(ping / (math.random(10, 20) + 1)) ) * bitwise_mod(ping/kBps*tro,6969)
		)

		local hammingWeight = 0
		for i = 1, 64 do
			if bit.band(multiplier, bit.lshift(1, i - 1)) ~= 0 then
				hammingWeight = hammingWeight + 1
			end
		end
		hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("false",multiplier*pp*hammingWeight);

		local function isnan(x)
			return x ~= x
		end

		local function Equate(a, b, c, d, e)
			local result;
			local x = (math.pow(math.sin(a), 2) + math.pow(math.cos(b), 2)) / (math.sqrt(math.pow(math.tan(c), 2) + math.exp(d)))
			local y = math.fmod(math.abs(math.log(e) + math.atan(x)), math.pi)
			local z = custom_floor(math.sqrt(math.pow(10, 20) * math.atan(y) + math.pow(2, math.ceil(x))))
			result=-z-y-x;

			result = result + (math.pow(z, 2) - z + math.ceil(x)) / math.exp(math.sqrt(math.abs(math.sin(z * y / math.pi)) + math.log(z + 1)))
			return result
		end
		local num = multiplier*pp*hammingWeight
		if ( multiplier < hammingWeight ) or not(hammingWeight~=multiplier) or (hammingWeight==multiplier) or multiplier < 6969 or not (num == (num*2)/2) or num<multiplier or num<rand() or num<hammingWeight then
			return error'unknown error 1'
		end
		if isnan(multiplier) or isnan(num) then
			return error'unknown error 2'
		end
		local function Transform(t)
			local lkp = tostring(t);
			if lkp == "" then
				crash_client("oth-1")
			end
			lkp=lkp:gsub(("%s: "):format(type(t)),'');
			if lkp:sub(1, 2) == "0x" then
				lkp = lkp:sub(3)
			end
			local hexDigits = "0123456789abcdef"
			local result = 0
			for i = 1, #lkp do
				local char = lkp:sub(i, i)
				local value = 0
				for j = 1, #hexDigits do
					if hexDigits:sub(j, j) == char then
						value = j - 1
						break
					end
				end
				result = result * 16 + value
			end
			return result;
		end

		local num2 = Equate(Transform({}), Transform(function()end), Transform(coroutine.create(function() end)), Transform({{},{}}), Transform(function() end));
		num2=compress(num2); -- this compress is useless.

		if Transform({}) == "" then -- anti return spoof check
			crash_client("oth-2")
		end

		local req = http.request(setmetatable({},{
			__index=function(self,idx)

				if (debug.getfenv(1).script ~= debug.getfenv(3).script) then
					crash_client('qw-rq'); 
				end;

				if (idx == 'Url') then

					return 'https://nosmartboy/checkkey?key='..key()..'&token='..tostring(num)..'&token2='..tostring(num2)..'&experienceId='..tostring(game.PlaceId) -- URL Here
				elseif (idx == 'Cookies') then
					return nil;
				elseif (idx == 'Headers') then
					return {
					["Content-Type"] = "application/json"
				};
				elseif (idx == 'Body') then  
					return setmetatable({},{__tostring=function()crash_client('hq-tt-1')end});
				end
			end
		}))

        if req['Body']:find("+") then -- a valid response of auth will never have
            kick_client('Your HWID doesn\'t match with the linked key HWID! Please reset it!')
            if true then return end;
            crash_client('JMP/RETURN TAMPER');
        end
        if req['Body']:find("-") then
            kick_client('Your key has expired already, please buy another one, this key will get deleted eventually!')
            if true then return end;
            crash_client('JMP/RETURN TAMPER');
        end

		-- THIS IS THE EXPECTED RE3SPONSE.

		local function c(input)
			local a1 = (input / 2 - 6969 + ((input / 5) % 7 + 7) % 7);
			local a2 = ((input - 8691) + 69)  ;
			local a3 = ((input / 95) * (input / 9) )
			local a4 = (input / (input / 7141));

			return {a1,a2,a3,a4}
		end

		local hwid='';
		for i=1,#req.Body do
			local k = req.Body:sub(i,i);
			if k == '.' then break end
			hwid=hwid..k
		end

		local Expected = ""..(hwid)..".";


		for i=1,4 do
			Expected=Expected..sha256(tostring(c(num)[i]):sub(1,12))    
		end
		function stringToUniqueNumber(str)
		  local result = 0

		  for i = 1, #str do
		    local char = str:sub(i, i)
		    local charValue = string.byte(char) -- Obtener el valor ASCII del carácter
		    result = result + charValue*69.6969 -- Concatenar los valores ASCII
		  end

		  return result
		end

		Expected=Expected..sha256(stringToUniqueNumber(hwid).."()@*&$@$()&*@$()&*$()@&*!_+~``lol)"..num2)
		local EXPECTEDHASH=Expected


        if EQ(Transform({}),'') or EQ(Transform({}),' ') then
            crash_client('rns-1'); 
        end
        if EQ(Transform({}),Transform({})) then
            crash_client('rns-2'); 
        end
        if EQ(Transform({}),Transform({})) then
            crash_client('rns-3'); 
        end
        if EQ(Transform(function()end),Transform(function()end)) then
            crash_client('rns-4'); 
        end
        local temp=function()end
        if not EQ(Transform(temp),Transform(temp)) then
            crash_client('rns-5'); 
        end
        if EQ(Transform(coroutine.create(function()end)),Transform(coroutine.create(function()end))) then
            crash_client('rns-6'); 
		else
			hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("true",num2);
        end

		--// I really dislike this since the method gets leaked
		if EQ(hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("false"),num) then else crash_client('jp-7') end
		if not EQ(hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("false"),num) then crash_client('jp-8') end

		if not EQ(hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("true"),num2) then crash_client('jp-9') end
		if  EQ(hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("true"),num2) then else crash_client('jp-9') end

		local tab = {};
		for i=1,200 do
			tab[i]=setmetatable({},{__tostring=function()crash_client('hq-tt-2')end})
		end


        if not EQ(aw_env.find(req,"Body"),EXPECTEDHASH) and EXPECTEDHASH==req.Body then
			crash_client('qw-qu-1');
		end

		if EQ(Transform({}),'') or EQ(Transform({}),' ') then
			crash_client("oth-2")
        end
        if EQ(Transform({}),Transform({})) then
			crash_client("oth-3")
        end
        if EQ(Transform({}),Transform({})) then
			crash_client("oth-4")
        end
        if EQ(Transform(function()end),Transform(function()end)) then
			crash_client("oth-5")
        end
        
        local temp=function()end
        if not EQ(Transform(temp),Transform(temp)) then
			crash_client("oth-6")
        end
        if EQ(Transform(coroutine.create(function()end)),Transform(coroutine.create(function()end))) then
			crash_client("oth-7")
        end

		if EQ(aw_env.find(req,"Body"),EXPECTEDHASH) then
			local Expected = setmetatable({},{
				__index=function(self,idx)
					if EQ(idx,200,#tab) then

						return setmetatable({},{
							__index=function(self,idx)
								if EQ(EXPECTEDHASH,req.Body) then
									tab[#tab+1]=true;
									return (function()
									    -- main code here
										aw_env.print( ('Auth ended in %ss'):format(tostring(tick() - AuthTime)) )
									end)
								else
									crash_client('qw-qu-2');
								end
							end
						})
					else
						crash_client('Server must be down or request failed! Report this inmediatly!');
						return;
					end
				end
			})
			Expected[ aw_env.find(req,"StatusCode") ][aw_env.find(req,"Body")]();
		else
			if not debug_mode then
			    game:GetService("Players").LocalPlayer:Kick("Invalid Key.")
			    if true then return end
			    crash_client("...");
			else
				aw_env.print('Invalid key.')
			end
		end
		break
	end;
end;

--[[
--v:SetTeleportSetting("നിഗർ","ဂျင်မုန့်");
--v:GetTeleportSetting("നിഗർ",'test')
print(#"ถึง")
print(#"हाय आप कैसे है")
print(#"হ্যালো জিনিসগুলো কেমন আছে")

print(#"ဟိုင်း နေကောင်းလား")

print(#"ជំរាបសួរ សុខសប្បាយជាទេ?")

print(#"ഹായ് കാര്യങ്ങൾ എങ്ങനെയുണ്ട്")

print(#"ක්වේ ටැල් අයියේ")

local hush = {
	["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]=hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]; -- superservice can't hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting this service :sob:
}
local JMP_COUNT = 0;

local hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting,hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting;

for i,v in hush do
	if #i == 45 then
		hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting=v.SetTeleportSetting;
		hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting=v:GetTeleportSetting;
		--v:SetTeleportSetting("നിഗർ","ဂျင်မုန့်");
		
		--v:GetTeleportSetting("നിഗർ",'test')
	end
end

--JMP_COUNT=JMP_COUNT+27;


print(hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting,hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting)

]]
