-- uh sorry for awful syntax.


-- stack security:


local StackInsideOfStack = setmetatable({},{__index=function()

end}) -- it will be allocated inside of the stack of most obfuscators, so by checking if it has been indexed or any metamethod has been called then its tampered.










-- Deep stack obfuscation:

local StackInsideOfStackTwo = {
    {},{{}},setmetatable({},{__index=crash_client}),function() return {} end,{{},{}},"Just add junk and mix it with your real values inside the table"
}


-- As the string said, just mix junk tables and metatables with your real stuff, you can automize it by using 'for' loops or doing it manually.

-- Quick example from a friend;
--[[
Compiler takes this:
local http_request = syn and syn.request
print(http_request)

local Stack1 = {
    {},
    {
        {
            
        },
        {

        }
    },
    {
        {

        }
    }
}

Converts it into:
Stack1[2][1] = syn and syn.request
print(Stack1[2][1])
]]





-- wohoho, want your values not being leaked or printed?????!?!?!!?
-- i got a solution for it!!
-- it's the same method luraph uses to protect constants.

print('\0\0\0\0put your real stirng here');
warn('\0\0\0\0put your real stirng here');
setclipboard('\0\0\0\0put your real stirng here'); 

-- simply all of the above are never being leaked unless you remove the \0\0\0\0, you're welcome!










-- Add junk opcode instructions to the code, just go to dsf github for LuaShield and you will find that he has custom macros that just randomly put junk on the code, it's jsut copy pasting useless stuff that has troll stirngs like "hook detected" or "insert fake link here" or whatever, keep in mind that making the code harder to read, makes it secure on the VM when reverse engeneering.












-- AntiJMP or anti EQ bypasses.

-- for anti JMP you just mantain a counting for the JMPS on the script, what I mean is for example:

local CallAmmount = 0;
function call()
    CallAmmount+=1;
end;


-- later on the script you use:
if CallAmmount ~= 1 then
    -- maybe JMP or hook.
end

-- so just do that kind of things but on all the script, try using math or stuff that confuses the person on what the number or string does.

-- Thats just some basic anti JMP, but you get the idea, try making your code only work when all other pieces have been executed.

-- so your main script cant work unless some part of the whitelist/key system code changes a number or replaces a stirng!


-- You maybe noticed that you can easly just change CaollAmmount to "1" but never calling the function, just changing the local, so how we protect against that?? check below.



-- ANTI STACK REPLAY :
-- Brief explanation of stack on obfuscators: It handles all the values of your script, functions, sometimes constants, etc. 
-- A stack replay attack is just copying the stack when some user is whitelisted, which means they will do some kind of "record" of the table when one user is whitelisted, then just "play" that table on every user, even if its not whitelisted it will work since its just copying all correct values.

-- The way we prevent this is storing stuff off the stack, but if you know something about VMs you maybe are wondering how? Theorically the obfuscator always will handle this values.
-- Yes, the obfuscator will always handle them, but what if you store a value on getgenv().nil ? it is valid! Or what if you store a value inside of TELEPORTSERVICESETTINGS? IT IS ALSO VALID! So for example on the anti JMP before explained, instead of using a local variable, you will use getgenv or teleport service! heres a quick example:



local hush = {
	["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"] = game:GetService("TeleportService"); -- superservice can't hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting this service :sob:
}

hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil","\0\0\0\0NOT ACTIVATED")
local JMP_COUNT = '\0\0\0\0NOT ACTIVATED';

function call()
    hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil","\0\0\0\0 ACTIVATED")
    JMP_COUNT = '\0\0\0\0 ACTIVATED';
end

call();

if hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil") ~= JMP_COUNT then
    print('Some tamper happened on the local variable, but the backup outside the stack didnt changed.')
end


-- uh I will explain how to prevent EQ spoofs......         just do a custom equality function without using "==" or just using alot of confusion.
-- for example using rawequal or checking if the sum of bytes match, etc.

-- my custom EQ:

local function EQ(a,b,c)
    JMP_COUNT=JMP_COUNT+21;
    hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:SetTeleportSetting("nil",hush["ह𝒪̷̄̋̐रा𝒪̷̄̋̐̈̒̚मी"]:GetTeleportSetting("nil").."ကောင်းလ");

	if not c then
		return (type(a) == typeof(b) and typeof(a) == type(b) and not(type(a) ~= typeof(b) and typeof(a) ~= type(b))) and ((#tostring(a) == #tostring(b)) and not (#tostring(a) ~= #tostring(b))) and rawequal(a,b) and (not(tostring(a) < tostring(b)) and not (tostring(a) > tostring(b)) and rawequal(tostring(a),tostring(b))) 
	end
	return (EQ(a,b) and EQ(a,c) and EQ(b,c))
end

-- cyclops EQ:  (I wont explain it, it's pretty intuitive and self-explanatory)
local SECURE_EQ = function(a, ...)
    local rawequal = rawequal;
    local rng = math.random;
    local FAST_EQ = LPH_JIT(function(a, b) return a == b end);
  
    -- confuse attackers through entropic equality checks
    -- also use vararg to prevent call inspection
    local proxied_entropic_results = {(function(a, b) 
       local entropy = rng(1, 2) == 1;
       return entropy and rawequal(a, b) or rawequal(b, a), entropy and FAST_EQ(a, b) or FAST_EQ(b, a), entropy
    end)(a, ...)};
  
    -- confuse attacker through an arbitrary scheduler honeypot
    local arbitrary_scheduler_honeypot = false;
    coroutine.wrap(function() arbitrary_scheduler_honeypot = true; end)()
    proxied_entropic_results[#proxied_entropic_results + 1] = arbitrary_scheduler_honeypot
  
    -- prevent arbitrary pc modification during equality assertion
    local vm_integrity_flag = false
  
    -- assert the results without directly using equality
    for i, assumption in proxied_entropic_results do 
      -- skip entropic honeypot
      if i == 3 then vm_integrity_flag = true; break; end;
   
      while not assumption do LPH_CRASH(); end;
    end
     
    -- check virtual machine sanity
    while not vm_integrity_flag do LPH_CRASH(); end;
    
    -- check for equality flipping
    if (a ~= a) or (not a == a) then LPH_CRASH(); end;
    
    -- equality checking has completed
    return true
  end

-- thats it? just mix it all together in the same script.
