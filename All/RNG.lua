-- To ensure each execution is different and users not sharing key or the request being spoofed, we must make it dynamic!

-- So just an example of this would be always hashing the hwid, and the valid key will be the hashed hwid, which means users cant share key anymore.
-- to make it more dynamic you can add a randomness to it, so it's half of the hwid + a random number or anything you want! Localhour, etc.

-- We must check if key has not been spoofed somehow, how do we do this? Just send a number to the server, for example the number "5", and now both client and server will do math, for example client and server will both do hash(number^2) or in this case hash(5^2), the RNG will generate this number but both client and backednd will do math over it, now you just check if both client and backend maths are the same, if they are not the same it means key invalid or tampered with response. 

-- I will provide the most advanced rng I figured out :shrug:

local function customFloor(number)
	return number - (number % 1)
end
function compress(a)
    return a
end
local Stats = game:GetService("Stats");
local ping = math.round(Stats.Network.ServerStatsItem["Data Ping"]:GetValueString():split('(')[1])
local et = Stats.Network.ServerStatsItem["ElapsedTime"]:GetValueString()
local kBps  = Stats.Network.ServerStatsItem["Send kBps"]:GetValueString()
local y = math.pi * math.exp(1);
local tri  = Stats.Network.ServerStatsItem["TotalRakIn"]:GetValueString()
local tro  = Stats.Network.ServerStatsItem["TotalInDataBW"]:GetValueString()
local a = 74632492384259
local c = 987654
local m = 2 ^ 32
local seed = os.time()
local function rand()
	seed = (a * seed + tro * tri) % m
	return seed
end
local pp = rand();
local function bitwise_mod(x, m)
	return x - customFloor(x / m) * m
end
if os.time() < 1681420894 or tick() < 1681402945.6009 then
	return
end
local multiplier = (
	( math.abs(math.tan(os.time() % (ping + 1) + bitwise_mod(os.clock() * y, 2 * math.pi))) *
		bitwise_mod(os.time() + tick(), tro + 1) * 
		math.pow(bitwise_mod(kBps / ping, math.sqrt(tro)), math.sin(tick() % math.pi)) +
		bitwise_mod(os.clock() * y, math.pi) *
		math.abs(math.cos(et / 2)) *
		tri / customFloor(ping / (math.random(10, 20) + 1)) ) * bitwise_mod(ping / kBps * tro, 6969)
)
local hammingWeight = 0
for i = 1, 64 do
	if bit.band(multiplier, bit.lshift(1, i - 1)) ~= 0 then
		hammingWeight = hammingWeight + 1
	end
end
local function Equate(a, b, c, d, e)
	local result;
	local x = (math.pow(math.sin(a), 2) + math.pow(math.cos(b), 2)) / (math.sqrt(math.pow(math.tan(c), 2) + math.exp(d)))
	local y = math.fmod(math.abs(math.log(e) + math.atan(x)), math.pi)
	local z = customFloor(math.sqrt(math.pow(10, 20) * math.atan(y) + math.pow(2, math.ceil(x))))
	result = -z - y - x;
	result = result + (math.pow(z, 2) - z + math.ceil(x)) / math.exp(math.sqrt(math.abs(math.sin(z * y / math.pi)) + math.log(z + 1)))
	return result
end
local function Transform(t)
	local lkp = tostring(t);
	if lkp == "" then
		crash_client("Invalid input, so its being used by externals!")
	end
	lkp = lkp:gsub(("%s: "):format(type(t)), '');
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

local num2 = Equate(Transform({}), Transform(function()
end), Transform(coroutine.create(function()
end)), Transform({
	{},
	{}
}), Transform(function()
end));


local rng = customFloor(multiplier*hammingWeight^8)-- compress(multiplier * pp * hammingWeight*num2)  --num2 -- multiplier * pp * hammingWeight, The other RNG is just to trick people with constants;
local rng2 = compress(num2)
function EQ(a,b) return a==b end; -- go to VMSECURITY for better EQ.

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
    hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("true",num2); -- go to VMSECURITY to understand this.
end
