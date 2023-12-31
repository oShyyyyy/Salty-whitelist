--[[
	What this containts?
	- It contains two RNG, diversal antihooks (15 methods, anyways most of them wont work when obfuscated due to levels)

	What exploits support this whitelist?
	- Every single exploit that supports http.request
]]

local key = ''



local AuthTime = tick();
newcclosure=newcclosure or nil;
cloneref=cloneref or nil;
islclosure=islclosure or nil;
syn=syn or nil;
http=http or nil;
bit=bit or nil;
request=request or syn.request or http.request

do
	local valids = {
		request,newcclosure,cloneref,islclosure,bit
	};
	if #valids ~= 5 then
		print"Your exploit does not support the enough functions that make the whitelist work!";
		return
	end
	valids=nil;
end


local CurrentENV =  debug.getfenv();
local anti_tampers = {};

--// Basic Init.
function UnexpectedJMP()
	local a,b=1,50 or 70;
	while a and b do
		if a > b then
			error"TAMPER DETECTED!"; -- I know this will make mad more than one person rofl.
			break
		else
			a=b-20;
			for i=1,6969 do
				a=b;
				break
			end
			break
		end
	end
	if a ~= b then
		error"TAMPER DETECTED!"; -- I know this will make mad more than one person rofl.
	end
end
function Newcclosure(func)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
	return coroutine.wrap(function(...)
        while true do
            coroutine.yield(func(...))
        end
    end)
end
function Print(...)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
	return newcclosure(function(...)
		print(...)
	end)(...)
end
function GetChildren(obj)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
	local mo = Instance.new("Part");
	task.spawn(function()
		task.wait(1);
		mo:Destroy();
		mo=nil;
	end)
	return mo.GetChildren(obj);
end
function Type(arr)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
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
	while true do
		h=h+1;
		local s,_ = ypcall(Newcclosure(function(arr,h) -- ypcall is exactly the same as pcall, but ypcall is a bit unknown, so it has less chance of being hooked.
			if h == 1 then
				arr=arr+1;
			elseif h == 2 then
				arr=arr..'';
			elseif h == 3 then
				local _=arr[5];
			elseif h == 4 then
				local _,_,_=coroutine.create(arr), islclosure(arr), xpcall(function() end, arr);

			elseif h == 5 then
				local _=false==arr and not (false~=arr) or error'err';
			elseif h == 6 then
				local _=true==arr and not (true~=arr) or error'err';
			elseif h == 7 then
				local _=nil==arr and not (nil~=arr) or error'err';
			end;
		end),arr,h)
		if s or h > 7 then
			break;
		end
		s=nil;
	end;
	return possibles[h];
end;
anti_tampers[#anti_tampers+1]=Type;
UnexpectedJMP();
function len(obj)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
	if Type(obj) == "string" then
        return #obj
    elseif Type(obj) == "table" then
        local count = 0
        while true do
            count = count + 1;
			if obj[count] == nil and obj[count+1]==nil and obj[count+2]==nil then
				break
			end
		end
        return count-1
    else
        return nil
    end
end
function Find(Table,value)
	for i,v in Pairs(Table) do
		if i == value or v == value then
			return true
		end
	end
	return false;
end
anti_tampers[#anti_tampers+1]=Find;
anti_tampers[#anti_tampers+1]=len;
function Pairs(arr)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
    local index = 0
    return function()
        index = index + 1
        if index <= len(arr) then
            return index, arr[index]
        end
    end
end
UnexpectedJMP();


local function customFloor(num)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
    return num - num % 1;
end
local function bitwise_mod(x, m)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
    return x - customFloor(x / m) * m;
end

UnexpectedJMP();
anti_tampers[#anti_tampers+1]=Pairs;
UnexpectedJMP();
anti_tampers[#anti_tampers+1]=customFloor;
UnexpectedJMP();
anti_tampers[#anti_tampers+1]=bitwise_mod;

local ConstantPool = { -- This not only makes harder reverse engeneering, but also allows chance to make each script diferent to reverse engeneering.
	'Stats',
	''
};

local ServicesStack = {};
UnexpectedJMP();

function SuperService(Name)
	if debug.getfenv(2) ~= CurrentENV then
		return nil;
	end
	for i,v in Pairs(GetChildren(game)) do
		if i == Name or tostring(v) == Name then
			return cloneref(v);
		end;
	end;
end;


ServicesStack[1]=SuperService(ConstantPool[1]);

if len(ServicesStack) < 1 or len(ServicesStack) > 1 or len(ServicesStack) ~=1 or not(len(ServicesStack)==1) then
	warn'Anti Tamper. [Services]'
	return
else
	if len(anti_tampers) < 6 or len(anti_tampers) > 6 or len(anti_tampers) ~= 6 or not( len(anti_tampers) == 6) then
		warn'Anti Tamper. [Count]'
		return
	end
end


--// Loading Anti-Hooks.
local Co_ = '..';

function err()
	Co_ = 'sex'; -- My last braincell died here, so don't judge this antihook type...
end
local count = 0;
local meta = {};
local SavedMeta = debug.getmetatable("");
local mt = SavedMeta;

-- Advanced ToString Check. (This is so smart!)
mt.__tostring = function(a,b)Co_=a if a == "http://auto:blank" then err('ASDGDUSIGSYT') return 'http://auto:blank' end return a end
debug.setmetatable("",mt)

meta=setmetatable(meta, {
	__index  = function(_,abc)
		if abc == 'Url' then
			if count == 0 then
				count = count+1;
				return 'http://auto:blank'
			else
				error('qwerty')
			end
		end
	end
})

local data = {Url = 'https://about:blank'}
local keta = 0
request(
    setmetatable({Url = 'https://about:blank'},{
        __index = function(_,b)
            keta = keta +1
            return data[b]
        end
    })
)
if keta > 4 or keta < 4 then
    warn('Detection -1!')
    return
end

local x,y = pcall(function()
	request(meta)
end)
local u,k = pcall(function()
    request({Url = 'http://auto:blank'})
end)
if Co_ == 'sex' then
    warn('detection 0.')
    return
end
if not x then
	if y:find('qwerty') then
		warn('detection  1.')
		return
	end
	warn('detection  2.')
	return
else
    if count > 1 then
        warn('detection  3.')
        return
    end
end

debug.setmetatable("",SavedMeta)

local setmt = setmetatable({}, {
	__call = function (_,...)
		return setmetatable(...)
	end,
	__tostring = error
})

local steps = {'Url','Method','Headers','Cookies','Body'};
local ic = 0;

local Indexcount = 0;

local ANTISPOOF =  setmetatable({
		__index=function(_,b)
			if (debug.getinfo(2).func~=request) then
				error('Luafort: Error attempting to connect to the server!')
			end
			Indexcount=Indexcount+1;
			if not steps[Indexcount] then
				error('Luafort: Error attempting to connect to the server!')
			end
			steps[Indexcount]=nil;
			if b == 'Url' then 
				return 'https://about:blank' 
			end
		end
	}, {__tostring=error}) 

local s,e = ypcall(newcclosure(function()
	request(setmt({},ANTISPOOF))
end))

if type(e) ~= 'table' or not e['Headers'] then
	print'detection 4'
end

if not s and e:find('Luafort') then
	print'detection 5'
end


local payload = request(setmetatable({Method="GET"},{
	__index=function(_,k)
		ic=ic+1;

		if not Find(debug.getstack(3),"Url") and ic == 1 then
			print('Detection 14')
		end
		if not Find(debug.getconstants(3),'GET') or not Find(debug.getconstants(3),'request') then
			print('Detection 15')
		end

		if islclosure(debug.getinfo(3).func) == false then
			print('detection 6')
		end
		if islclosure(debug.getinfo(1).func) == true and islclosure(debug.getinfo(2).func) == true and islclosure(debug.getinfo(3).func) == false then
			print('detection 7')
		end

		if (debug.getfenv(1).script ~= debug.getfenv(3).script) then
			print'detection 8';
        end;

		-- I tested the down below from luaguard old detections.

		local pattern = ":(%d+)\n:(%d+)"
		local pre1,pre2;
--[[
		if #debug.traceback('12345678', 1) > 36 or #debug.traceback('12345678', 1) < 36 then
			pre1=true;
		end
		if #debug.traceback('87654321', 2) > 14 or #debug.traceback('87654321', 2) < 14 then
			pre2=true;
		end

		if pre1 and pre2 then
			print('DEFINITIVE DETECTION')
		else
			if pre1 then
				print('detection 12')
			end
			if pre2 then
				print('detection 13')
			end
		end
]]
		if debug.traceback('12345678', 1):match(pattern) then
			print'detection 9'
		end
		if debug.traceback('87654321', 2):match(pattern) then
			print'detection 10'
		end

		if (k == 'Url') then
        	return 'https://about:blank'; -- URL Here
        elseif (k == 'Cookies') then
            return nil;
        elseif (k == 'Headers') then
            return {
            ["Content-Type"] = "application/json"
        };
        elseif (k == 'Body') then  
            return nil;
        end
	end
}))

if ic < 4 or ic > 4 then-- checking indexes of both requests
	print('detection 11')
end
if Indexcount < 5 or Indexcount > 5 then
	print('detection 11')
end




function EQ(a,b,c)
    if not c then
        return (type(a) == typeof(b) and typeof(a) == type(b) and not(type(a) ~= typeof(b) and typeof(a) ~= type(b))) and ((#tostring(a) == #tostring(b)) and not (#tostring(a) ~= #tostring(b))) and rawequal(a,b) and (not(tostring(a) < tostring(b)) and not (tostring(a) > tostring(b)) and rawequal(tostring(a),tostring(b))) 
    end
    return (EQ(a,b) and EQ(a,c) and EQ(b,c))
end


local LuaFort = {
	GetService = SuperService;
}

function Equate(a, b, c, d, e)
	local result;
	local x = (math.pow(math.sin(a), 2) + math.pow(math.cos(b), 2)) / (math.sqrt(math.pow(math.tan(c), 2) + math.exp(d)))
	local y = math.fmod(math.abs(math.log(e) + math.atan(x)), math.pi)
	local z = math.floor(math.sqrt(math.pow(10, 20) * math.atan(y) + math.pow(2, math.ceil(x))))
	result=-z-y-x;

	result = result + (math.pow(z, 2) - z + math.ceil(x)) * math.exp(math.sqrt(math.abs(math.sin(z * y / math.pi)) + math.log(z + 1)))
	return result
end

function Transform(t)
	local lkp = tostring(t);
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
	return(result);
end

num2 = Equate(Transform({}), Transform(function()end), Transform(coroutine.create(function() end)), Transform({{},{}}), Transform(function() end));


print('IS VALID KEY:',EQ(payload.Body,key));
print('RNG_1 RESULT:',num);
print('RNG_2 RESULT:',num2);

print('MACROS:',#LuaFort);


Co_=nil;
count=nil;
SavedMeta=nil;
meta=nil;
mt=nil;
Equate=nil;
Transform=nil;


ServicesStack=nil;
len=nil;
SuperService=nil;
ConstantPool=nil;
Pairs=nil;
customFloor=nil;
bitwise_mod=nil;
UnexpectedJMP=nil;
anti_tampers=nil;
Type=nil;
CurrentENV=nil;
GetChildren=nil;
Find=nil;

print(('Authernticated in %ss!'):format(tostring(tick()-AuthTime):sub(1, -12)))


--[[
local p = clonefunction(request)

local old;
old=hookfunction(p, newcclosure(function(a)
	return old(a);
end))

local steps = {'Url','Method','Headers','Cookies','Body'};
local Indexcount = 0;

local s,e = ypcall(newcclosure(function()
	p(setmetatable({}, {
		__index=function(_,b)
			if (debug.getinfo(2).func~=p) then
				error('Luafort: Error attempting to connect to the server!')
			end
			Indexcount=Indexcount+1;
			if not steps[Indexcount] then
				error('Luafort: Error attempting to connect to the server!')
			end
			steps[Indexcount]=nil;
			if b == 'Url' then 
				return 'https://about:blank' 
			end
		end
	}))
end))

if not s and e:find('Luafort') then
	print'detected'
	return 
end

]]

