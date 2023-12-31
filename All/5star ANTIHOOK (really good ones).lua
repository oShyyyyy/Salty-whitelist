--discord.gg/boronide, code generated using luamin.js™





-- Dedicated antihooks for request function:
-- First sanity checks, never add the real url before the antihooks or sanity checks, you just risk the url leaked.

local order = {
	'Url',
	'Method',
	'Headers',
	'Cookies',
	'Body'
}; -- Exploits will always implicitly index in this exact order, if theres an extra index = tamper.
local Indexcount = 0; -- exploits will always index hte same ammount on every single execution, unless its hooked and a person tampered with the url table.

local ANTISPOOF = setmetatable({
	__index = function(_, b)
		if (debug.getinfo(2).func ~= http.request) then -- if the table is indexed by the exploit and it's unhooked, then debug.getinfo(2).func will be always the http.request, but if it is hooked that function wont be anymore debug.getinfo(2).func, this applies to every single exploit and 0 performance cost and never throws false positives.
			error('Error attempting to connect to the server!') -- in order to prevent the request completting.
		end
		if (debug.getfenv(1).script ~= debug.getfenv(3).script) then -- same as above check, bassically.
			error('Error attempting to connect to the server!')
		end;
		Indexcount = Indexcount + 1;
		if not order[Indexcount] or order[Indexcount] ~= b then -- if the order of the indexes doesnt matches or is indexed with the same multiple times then it has been tampered
			error('Error attempting to connect to the server!')
		end
		order[Indexcount] = nil; -- removes it from the table, so it can never repeat the same index.
		if b == 'Url' then
			return 'https://about:blank/'; -- pro tip: All exploits allow requesting to empty websites, in this case about:blank loads in 0.0000 secconds, so its almost perfect zero performance lost but with really effective antihooks! Also adding about blank allows the real url not being spied and if somehow people get the url it will never be the real one.
		end
	end
}, {
	__tostring = error
}) 

local s, e = pcall(newcclosure(function()
	http.request(setmetatable({}, ANTISPOOF)) -- does the request with pcall so we can check the error, you can just skip this part and instead of doing errors inside the metatable you just crash them.
end))

if type(e) ~= 'table' or not e['Headers'] then -- I recommend you doing your own type() function, if you want me to give it to you just dm me!
	crash_client('hooked');
	return
end

if not s and e:find('Luafort') then
	crash_client('hooked!');
	return
end



-- General Antihooks:  (This antihooks will protect constantly, which means they cant be disabled somehow unless you remove them from looping, but then it will have no sense)
-- Alright, these antihooks maybe is a bit more performance consuming.

local function Stack(StackCount, Function, ...)
	if StackCount == 0 then
		task.wait()
	end
	Function(...)
	Stack(StackCount + 1, Function)
end

task.spawn(function()
	local Function = function()
	end
	pcall(Stack, 0, Function)
	while task.wait(1) do
		local Success1, Error1 = pcall(Stack, 0, Function)
		local Success2, Error2 = pcall(Stack, 0, http.request) -- put the funciton you want to be protected here on http.request

        --print(Error1,'http')
        --print(Error2,'http')
	end
end)

-- This just causes an overflow, if its hooked then the exploit will crash and the error of Error1 and Error2 will change, so you can easly predicting the crash by checking if the error has changed.
-- It's not neccesary checking the error, by default this will always crash the hook anyway.


-- General Antihook #2:

local CStackOverflow = function(Function, Offset)
	for _ = 1, 200 - Offset do
		Function = coroutine.wrap(Function)
	end
	return Function;
end
task.spawn(function()
	while task.wait() do
		if pcall(CStackOverflow(http.request, 3)) == "C stack overflow" then -- replace http.request with the function you want protected
			crash_client('hooked!');
			return
		end
	end
end)


-- Works similar to the other overflow, but this one instead of crashing it throws error, so youc can handle the error as above and check if it has been hooked.
-- Curious right? if its hooked it will throw error, if its not hooked it will work normally.




-- Other dedicated antihooks:


-- task.spawn :

local s, e = pcall(function()
	task.spawn(newcclosure(function()
		local constant = 'no constants 4u';
	end))
end)
if not s and (type(e) == 'string' and e:find('expected lua') or e:find('than 0')) then
	crash_client('hook');
	return
end

-- Explanation: When someone tries using getconstants or getupvalues it will throw error, since these functions can't be used on cclosures, if you notce above we are using a cclosure.



-- Pcall antihook:  
local _, _ = pcall(setmetatable(setmetatable({}, {}), 
{
	__tostring = print, -- replace this to another crash idk
	__call = function ()
		if #debug.getconstants(3) < 10 then
			crash_client('hooks');
			return
		end
		if debug.getinfo(2).func ~= pcall then
			crash_client('hooks');
			return
		end
	end
}));

-- Explanation: pcalls implicitly call the functions, so we can replace the funciton with metatables, so we can check if the env of the pcall has ben altered or not by checking its constants or if implicitly the funciton has been changed (hooked)

local p = clonefunction(pcall); -- I don't remember this antihook tbh, anwyays it works :shrug:

local s = p(function ()
    if (#debug.getupvalues(3)) ~= 0 then
        warn('Test #2') -- when hooked.
        return
    end
end)
