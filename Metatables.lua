--[[
	
Copyright 2023 Liam Matthews

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

	http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

]]

local variable_pattern = "[%w_][%w_%d%[%]%.:]*"

local debug_enabled = false

local target_pattern_defs = {
	artihmetic_two_variables = {
		{
			match = {
				pattern = ("%s ?%%1 ?%%2"):format(variable_pattern),
				params = {
					"percent_literal_char",
					"percent_literal_var"
				}
			},
			find = {
				pattern = ("(%s) ?%%1 ?(%s)"):format(variable_pattern, variable_pattern),
				params = {
					"percent_literal_char"
				}
			}
		},
		{
			match = {
				pattern = ("%%1 ?%%2 ?%s"):format(variable_pattern),
				params = {
					"percent_literal_var",
					"percent_literal_char"
				}
			},
			find = {
				pattern = ("(%s) ?%%1 ?(%s)"):format(variable_pattern, variable_pattern),
				params = {
					"percent_literal_char"
				}
			}
		}
	},
	artihmetic_one_variable = {
		{
			match = {
				pattern = ("[^%s]? %%1 ?%%2"):format(variable_pattern),
				params = {
					"percent_literal_char",
					"percent_literal_var"
				}
			},
			find = {
				pattern = ("%%1 ?(%s)"):format(variable_pattern),
				params = {
					"percent_literal_char"
				}
			}
		}
	}
}

local supported_metamethods = {
	{
		name = "__unm",
		replace_pattern = "MT__unm(%s)",
		target_patterns = target_pattern_defs.artihmetic_one_variable,
		chars = {
			"-"
		},
		handling = "arithmetic"
	},
	{
		name = "__add",
		replace_pattern = "MT__add(%s, %s)",
		target_patterns = target_pattern_defs.artihmetic_two_variables,
		chars = {
			"+"
		},
		handling = "arithmetic"
	},
	{
		name = "__sub",
		replace_pattern = "MT__sub(%s, %s)",
		target_patterns = target_pattern_defs.artihmetic_two_variables,
		chars = {
			"-"
		},
		handling = "arithmetic"
	},
	{
		name = "__mul",
		replace_pattern = "MT__mul(%s, %s)",
		target_patterns = target_pattern_defs.artihmetic_two_variables,
		chars = {
			"*"
		},
		handling = "arithmetic"
	},
	{
		name = "__div",
		replace_pattern = "MT__div(%s, %s)",
		target_patterns = target_pattern_defs.artihmetic_two_variables,
		chars = {
			"/"
		},
		handling = "arithmetic"
	},
	{
		name = "__mod",
		replace_pattern = "MT__mod(%s, %s)",
		target_patterns = target_pattern_defs.artihmetic_two_variables,
		chars = {
			"%%"
		},
		handling = "arithmetic"
	},
	{
		name = "__index",
		replace_pattern = "MT__index(%s, %s)",
		chars = {
			"%.",
			"%[",
			":"
		},
		handling = "index"
	}
}

local parsed_data = {
	strings = {},
	comments = {},
	variable_graph = {},
	function_references = {},
	aliases = {}
}

local SWAMS_code = [[
-- Stormworks Addon Metatable Support (0.0.1.9) (SWAMS), by Toastery

function MT__lua_error(error) -- TODO: print line number, also try to figure out a way to get this to work with vehicle lua.
	server.announce(server.getAddonData(server.getAddonIndex()).path_id, error, -1)
end

function setmetatable(self, TEA_metatable_id)
	-- verify param 1
	local self_type = type(self)
	if self_type ~= "table" then
		MT__lua_error(("bad argument #1 to \'setmetatable\' (table expected, got %s"):format(self_type))
		return
	end

	-- verify param 2
	local metatable_id_type = type(TEA_metatable_id)
	if metatable_id_type ~= "nil" and metatable_id_type ~= "number" then
		MT__lua_error(("bad argument #2 to \'setmetatable\' (nil or number expected, got %s)"):format(metatable_id_type))
		return
	end

	-- check if metatable is protected
	if self.__TEA_metatable_id and TEA.metatables[self.__TEA_metatable_id].__metatable ~= nil then
		MT__lua_error("cannot change a protected metatable")
		return
	end

	self.__TEA_metatable_id = TEA_metatable_id

	return self
end

function MT__add(param1, param2)
	if param1.__TEA_metatable_id and TEA.metatables[param1.__TEA_metatable_id].__add then
		return TEA.metatables[param1.__TEA_metatable_id].__add(param1, param2)
	elseif param2.__TEA_metatable_id and TEA.metatables[param2.__TEA_metatable_id].__add then
		return TEA.metatables[param2.__TEA_metatable_id].__add(param1, param2)
	end

	return param1 + param2
end

function MT__sub(param1, param2)
	if param1.__TEA_metatable_id and TEA.metatables[param1.__TEA_metatable_id].__sub then
		return TEA.metatables[param1.__TEA_metatable_id].__sub(param1, param2)
	elseif param2.__TEA_metatable_id and TEA.metatables[param2.__TEA_metatable_id].__sub then
		return TEA.metatables[param2.__TEA_metatable_id].__sub(param1, param2)
	end

	return param1 - param2
end

function MT__mul(param1, param2)
	if param1.__TEA_metatable_id and TEA.metatables[param1.__TEA_metatable_id].__mul then
		return TEA.metatables[param1.__TEA_metatable_id].__mul(param1, param2)
	elseif param2.__TEA_metatable_id and TEA.metatables[param2.__TEA_metatable_id].__mul then
		return TEA.metatables[param2.__TEA_metatable_id].__mul(param1, param2)
	end

	return param1 * param2
end

function MT__div(param1, param2)
	if param1.__TEA_metatable_id and TEA.metatables[param1.__TEA_metatable_id].__div then
		return TEA.metatables[param1.__TEA_metatable_id].__div(param1, param2)
	elseif param2.__TEA_metatable_id and TEA.metatables[param2.__TEA_metatable_id].__div then
		return TEA.metatables[param2.__TEA_metatable_id].__div(param1, param2)
	end

	return param1 / param2
end

function MT__mod(param1, param2)
	if param1.__TEA_metatable_id and TEA.metatables[param1.__TEA_metatable_id].__mod then
		return TEA.metatables[param1.__TEA_metatable_id].__mod(param1, param2)
	elseif param2.__TEA_metatable_id and TEA.metatables[param2.__TEA_metatable_id].__mod then
		return TEA.metatables[param2.__TEA_metatable_id].__mod(param1, param2)
	end

	return param1 % param2
end

function MT__unm(param)
	if param.__TEA_metatable_id and TEA.metatables[param.__TEA_metatable_id].__unm then
		return TEA.metatables[param.__TEA_metatable_id].__unm(param)
	end

	return -param
end

-- __index metamethod
function MT__index(t, index)
	local value = t[index]
	-- if value already exists, dont execute __index metamethod, just return the value
	if value then
		return value
	end
	
	-- if there is no metatable, return the value
	if not t.__TEA_metatable_id then
		return value
	end
	
	local __index = TEA.metatables[t.__TEA_metatable_id].__index
	
	-- if this metamethod doesnt exist, return value.
	if not __index then
		return value
	end

	local __index_type = type(__index)
	
	-- return the value in the metamethod if its a table
	if __index_type == "table" then
		return __index[index]
	end
	
	-- return this metamethods function if its a function
	if __index_type == "function" then
		return __index(t, index)
	end
	
	-- it should never get to this point but alright.
	return value
end]]

old_print = print
print = function(str, force)
	if debug_enabled or force then
		old_print(str)
	end
end

local variable_amount = 0
local amount_per_print = 0
local last_print_count = 0
local variable_amount_setup = 0

--- @param x number the number to check if is whole
--- @return boolean is_whole returns true if x is whole, false if not, nil if x is nil
function math.isWhole(x) -- returns wether x is a whole number or not
	return math.tointeger(x)
end

--# returns the number of elements in the table
---@param t table table to get the size of
---@return number count the size of the table
function table.length(t)
	if not t or type(t) ~= "table" then
		return 0 -- invalid input
	end

	local count = 0

	for _ in pairs(t) do -- goes through each element in the table
		count = count + 1 -- adds 1 to the count
	end

	return count -- returns number of elements
end

table.copy = {

	iShallow = function(t, __ENV)
		__ENV = __ENV or _ENV
		return {__ENV.table.unpack(t)}
	end,
	shallow = function(t, __ENV)
		__ENV = __ENV or _ENV

		local t_type = __ENV.type(t)

		local t_shallow

		if t_type == "table" then
			for key, value in __ENV.next, t, nil do
				t_shallow[key] = value
			end
		end

		return t_shallow or t
	end,
	deep = function(t, __ENV)

		__ENV = __ENV or _ENV

		local function deepCopy(T)
			local copy = {}
			if __ENV.type(T) == "table" then
				for key, value in __ENV.next, T, nil do
					copy[deepCopy(key)] = deepCopy(value)
				end
			else
				copy = T
			end
			return copy
		end
	
		return deepCopy(t)
	end
}

--- Returns a string in a format that looks like how the table would be written.
---@param t table the table you want to turn into a string
---@return string str the table but in string form.
function string.fromTable(t)

	if type(t) ~= "table" then
		print(("(string.fromTable) t is not a table! type of t: %s t: %s"):format(type(t), t))
		return t
	end

	local function tableToString(T, S, ind)
		S = S or "{"
		ind = ind or "  "

		local table_length = table.length(T)
		local table_counter = 0

		for index, value in pairs(T) do

			table_counter = table_counter + 1
			if type(index) == "number" then
				S = ("%s\n%s[%s] = "):format(S, ind, tostring(index))
			elseif type(index) == "string" and tonumber(index) and math.isWhole(tonumber(index)) then
				S = ("%s\n%s\"%s\" = "):format(S, ind, index)
			else
				S = ("%s\n%s%s = "):format(S, ind, tostring(index))
			end

			if type(value) == "table" then
				S = ("%s{"):format(S)
				S = tableToString(value, S, ind.."  ")
			elseif type(value) == "string" then
				S = ("%s\"%s\""):format(S, tostring(value))
			else
				S = ("%s%s"):format(S, tostring(value))
			end

			S = ("%s%s"):format(S, table_counter == table_length and "" or ",")
		end

		S = ("%s\n%s}"):format(S, string.gsub(ind, "  ", "", 1))

		return S
	end

	return tableToString(t)
end

function string:toLiteral(literal_percent)
	if literal_percent then
		return self:gsub("([%(%)%.%%%+%-%*%?%[%^%$])", "%%%%%1")
	end

	return self:gsub("([%(%)%.%%%+%-%*%?%[%^%$])", "%%%1")
end

function string:advancedMatch(patterns, params)

	--print(self)

	local function translateParam(pattern, param_index)
		local param = pattern.params[tonumber(param_index)]

		--print("param type: "..param)

		if param == "var" then
			return params.var
		elseif param == "literal_var" then
			return params.var:toLiteral()
		elseif param == "percent_literal_var" then
			return params.var:toLiteral(true)
		elseif param == "char" then
			return params.char
		elseif param == "literal_char" then
			return params.char:toLiteral()
		elseif param == "percent_literal_char" then
			return params.char:toLiteral(true)
		end

		error("unknown param specified, specified type: "..tostring(param))
	end

	local function buildInParams(pattern)

		local built_string = pattern.pattern

		for param_index in built_string:gmatch("%%(%d+)") do
			--print("translated param: "..translateParam(pattern, param_index))
			built_string = built_string:gsub("%%"..param_index.."([^%d]*)", translateParam(pattern, param_index).."%1")
		end

		--print("built_string: "..built_string)

		return built_string
	end

	for pattern_index = 1, #patterns do
		local pattern = patterns[pattern_index]

		local built_pattern = buildInParams(pattern.match)

		--print("self:toLiteral(): "..self:toLiteral())

		if self:match(built_pattern) then
			--print("test: "..buildInParams(pattern.find))
			return self:find(buildInParams(pattern.find))
		end
	end
end

--- returns the number of instances of that character in the string
---@param str string the string we are wanting to check
---@param char any the character(s) we are wanting to count for in str, note that this is as a lua pattern
---@return number count the number of instances of char, if there was an error, count will be 0, and is_success will be false
---@return boolean is_success if we successfully got the number of instances of the character
function string.countCharInstances(str, char)

	if type(str) ~= "string" then
		--d.print(("(string.countCharInstances) str is not a string! type of str: %s str: %s"):format(type(str), str), true, 1)
		return 0, false
	end

	char = tostring(char)

	local _, count = string.gsub(str, char, "")

	return count, true
end

local function isComment(start)
	for i = 1, #parsed_data.comments do
		local s = parsed_data.comments[i]

		-- if the last index of this comment is greater or equal than the given index
		if s.location.last >= start then
			-- if the first index of this comment is less or equal than the given index
			if s.location.start <= start then
				-- then this comment contains the given index, so its a comment
				return true
			else
				-- this means that we've passed over where the comment could be, so we know we will never get a match past this, so return false early
				return false
			end
		end
	end

	-- its not in a comment, return false
	return false
end

local function getAllComments(text)

	local start_time = os.clock()

	--[[
		get all line comments

		line comments are pretty simple, everything after the line comment is now a comment
		up until the next line.
	]]
	local pos = 0

	for _ in text:gmatch("%-%-[%[]?[^%[]") do
		pos, _ = text:find("%-%-[%[]?[^%[]", pos)

		if not pos then break end

		local comment_last, _ = text:find("\n", pos)

		table.insert(parsed_data.comments, {
			location = {
				start = pos,
				last = comment_last
			},
			string = text:sub(pos, comment_last),
			type = "line_comment"
		})

		pos = comment_last
	end

	--[[
		get all block comments

		block comments are a little more complex, the block has to start outside of any comment
		otherwise its ignored
	]]
	pos = 0

	for _ in text:gmatch("%-%-%[%[") do
		pos, _ = text:find("%-%-%[%[", pos)

		if not pos then break end

		local comment_last, _ = text:find("]]", pos)

		if not comment_last then
			comment_last = text:len()
		end

		if not isComment(pos) then
			table.insert(parsed_data.comments, {
				location = {
					start = pos,
					last = comment_last
				},
				string = text:sub(pos, comment_last),
				type = "block_comment"
			})
		end

		pos = comment_last
	end

	-- sort the comments by first occuring
	table.sort(parsed_data.comments, 
		function(a, b)
			return a.location.start < b.location.start
		end
	)

	print(("Parsed all comments, took %0.3fs"):format(os.clock() - start_time), true)
end

local function getAllStrings(text)

	local start_time = os.clock()
	-- get all of the double quote "" strings

	local in_string = false

	local pos = 0

	local string_start = 0

	for s in text:gmatch("\"") do
		pos, _ = text:find("\"", pos + 1)

		if not isComment(pos) then
			-- check to make sure this quote is not cancelled out
			if text:sub(pos - 1, pos - 1) ~= "\\" then
				if in_string then
					table.insert(parsed_data.strings, {
						location = {
							start = string_start,
							last = pos
						},
						string = text:sub(string_start, pos),
						type = "double_quotes"
					})
				elseif not in_string then -- this is the start of a string
					string_start = pos
				end

				in_string = not in_string
			end
		end
	end

	-- get all of the double quote '' strings

	in_string = false

	pos = 0

	string_start = 0

	for s in text:gmatch("'") do
		pos, _ = text:find("'", pos + 1)

		if not isComment(pos) then

			-- check to make sure this quote is not cancelled out
			if text:sub(pos - 1, pos - 1) ~= "\\" then
				if in_string then
					table.insert(parsed_data.strings, {
						location = {
							start = string_start,
							last = pos
						},
						string = text:sub(string_start, pos),
						type = "single_quotes"
					})
				elseif not in_string then -- this is the start of a string
					string_start = pos
				end

				in_string = not in_string
			end
		end
	end

	-- TODO: get all of the double square bracket [[]] strings

	-- sort the strings by first occuring
	table.sort(parsed_data.strings, 
		function(a, b)
			return a.location.start < b.location.start
		end
	)

	print(("Parsed all strings, took %0.3fs"):format(os.clock() - start_time), true)
end

local function isString(start)
	for i = 1, #parsed_data.strings do
		local s = parsed_data.strings[i]

		-- if the last index of this string is greater or equal than the given index
		if s.location.last >= start then
			-- if the first index of this string is less or equal than the given index
			if s.location.start <= start then
				-- then this string contains the given index, so its a string
				return true
			else
				-- this means that we've passed over where the string could be, so we know we will never get a match past this, so return false early
				return false
			end
		end
	end

	-- its not in a string, return false
	return false
end

local function findPreviousNewline(pos, text)
	for char_index = pos, 1, -1 do
		if text:sub(char_index, char_index):match("\n") then
			return char_index
		end
	end

	return 1
end

local checked_aliases = {}

local function findAliases(variable_name, script_text)
	local aliases = {
		variable_name
	}

	if parsed_data.aliases[variable_name] then
		return parsed_data.aliases[variable_name]
	end

	if checked_aliases[variable_name] then return {} end

	checked_aliases[variable_name] = true

	for alias in script_text:gmatch("("..variable_pattern..")[ \n]*=[ \n]*"..variable_name) do
		if parsed_data.aliases[alias] then
			for _, alias_alias in pairs(parsed_data.aliases[alias]) do
				table.insert(aliases, alias_alias)
			end

		else
			local alias_aliases = findAliases(alias, script_text)
			for i = 1, #alias_aliases do
				print(("found alias of %s: %s"):format(variable_name, alias_aliases[i]))
				table.insert(aliases, alias_aliases[i])
			end
		end
	end

	parsed_data.aliases[variable_name] = aliases

	return aliases
end

local function getVariableGraph(text)

	local function scopeToString(scope)
		local s = tostring(scope[1])

		for depth = 2, #scope do
			s = ("%s.%s"):format(s, scope[depth])
		end

		return s
	end

	print("creating variable graph", true)
	local start_time = os.clock()
	-- graph of all variables, to avoid getVariableScopeGraph needing to constantly iter all variables

	local scope_depth = 1

	local scope = {
		tree = {
			0
		},
		graph = {}
	}

	local scope_counts = {
		0
	}

	local previous_end = 0

	local lua_words = 0

	-- go through all "lua words"
	for lua_word in text:gmatch(variable_pattern) do

		variable_amount_setup = variable_amount_setup + 1

		if variable_amount_setup - last_print_count >= amount_per_print then
			local percentage = variable_amount_setup/variable_amount
			local time_since_start = os.clock() - start_time
			print(("%0.2f%% complete creating variable graph (%s/%s) about %0.2fs remaining"):format(percentage*100, variable_amount_setup, variable_amount, variable_amount/variable_amount_setup*time_since_start-time_since_start), true)
			last_print_count = variable_amount_setup
		end

		local word_start, word_last = text:find(string.toLiteral(lua_word), previous_end + 1)

		if not word_start then
			error("word_start is nil! "..text:sub(previous_end, text:len()))
		end

		previous_end = word_last

		-- filter out comments
		if isComment(word_start) then
			--print("skipping "..lua_word.." as its a comment.")
			goto getVariableGraph_nextWord
		end

		-- filter out strings
		-- TODO: (also may want to trace strings if they match our variable name, they may be used to index the metatable)
		if isString(word_start) then
			--print("skipping "..lua_word.." as its a string.")
			goto getVariableGraph_nextWord
		end

		local line_start, line_last = text:find("[\n]?[^\n]*"..string.toLiteral(lua_word).."[^\n]*[\n]?", findPreviousNewline(word_start, text))

		-- trim the new lines
		line_start = line_start + 1
		line_last = line_last - 1
		--print("debug: "..text:sub(previous_uncommented_end, word_last))

		--print("lua word: "..lua_word)
		-- filter out table indicies
		--[[local prev_char = text:sub(word_start-1, word_start-1)
		if prev_char == "." or prev_char == ":" then
			goto getVariableGraph_nextWord
		end]]

		-- check if it was set to a function via =
		--[[if lua_word ~= "function" and text:find(lua_word.."[ \n]*=[ \n]*function", word_start) then
			print("function defininition found, skipping")
			goto getVariableGraph_nextWord
		end]]

		-- remove anything after a period or colon
		--[[local new_lua_word = lua_word:gsub("([^:%.])([:%.].*)", "%1")

		-- adjust the last char from the modified string
		if new_lua_word ~= lua_word then
			word_last = word_last - (lua_word:len() - new_lua_word:len())
		end

		lua_word = new_lua_word]]

		--dbg_previous_end = previous_end

		--print("modified lua word: "..lua_word)

		

		local prev_scope = scopeToString(scope.tree)

		if lua_word == "if" or lua_word == "do" then
			scope_depth = scope_depth + 1
			scope_counts[scope_depth] = scope_counts[scope_depth] and scope_counts[scope_depth] + 1 or 1
			scope.tree[scope_depth] = scope_counts[scope_depth]

			goto getVariableGraph_nextWord
		elseif lua_word == "else" or lua_word == "elseif" then
			scope_counts[scope_depth] = scope_counts[scope_depth] and scope_counts[scope_depth] + 1 or 1
			--scope.tree[scope_depth] = scope.tree[scope_depth] + 1

			goto getVariableGraph_nextWord
		elseif lua_word == "end" then
			scope.tree[scope_depth] = nil
			scope_depth = scope_depth - 1

			goto getVariableGraph_nextWord
		end

		if lua_word == "function" then
			scope_depth = scope_depth + 1
			scope_counts[scope_depth] = scope_counts[scope_depth] and scope_counts[scope_depth] + 1 or 1
			scope.tree[scope_depth] = scope_counts[scope_depth]

			table.insert(parsed_data.function_references, {
				scope = scopeToString(scope.tree),
				defined_in_scope = prev_scope,
				location = {
					start = word_start,
					last = word_last
				}
			})

			goto getVariableGraph_nextWord
		end

		local line_string = text:sub(line_start, line_last)

		-- dont add for function definitions
		--[[if line_string:match("function ?"..lua_word) then
			print("function defininition " ..lua_word.." found, skipping")
			--goto getVariableGraph_nextWord
		end]]

		if not parsed_data.variable_graph[lua_word] then
			print("found new lua_word: "..lua_word)
			parsed_data.variable_graph[lua_word] = {}

			lua_words = lua_words + 1
		end

		local scope_string = scopeToString(scope.tree)

		local node_data = {
			location = {
				start = word_start,
				last = word_last
			},
			line_location = {
				start = line_start,
				last = line_last
			},
			line_string = line_string,
			word_string = lua_word,
			scope = scope_string,
			prev_function = #parsed_data.function_references
		}



		if lua_word:sub(lua_word:len(), lua_word:len()) == "[" then
			node_data.word_string = node_data.word_string:sub(1, lua_word:len() - 1)
			node_data.location.last = node_data.location.last - 1
			lua_word = node_data.word_string
		end

		--[[ 
			insert into all indicies
			for example, if the lua word is g_savedata.foo.bar
			we want to insert this data in g_savedata, g_savedata.foo, and g_savedata.foo.bar
			this is for performance, so we can just index this table directly later, rather than do iterations through it.
		]]

		local built_lua_word = ""
		for word in lua_word:gmatch("[:%.]?[%w_][%w_%d%[%]]*") do
			built_lua_word = built_lua_word..word
			parsed_data.variable_graph[built_lua_word] = parsed_data.variable_graph[built_lua_word] or {}

			node_data = table.copy.deep(node_data)

			node_data.word_string = built_lua_word
			node_data.location.last = node_data.location.start + built_lua_word:len()

			table.insert(parsed_data.variable_graph[built_lua_word], node_data)
		end

		::getVariableGraph_nextWord::

	end

	print(("parsed variable graph, unique lua_words: %s. took %0.2fs"):format(lua_words, os.clock()-start_time), true)
end

local function findClosure(char_index, text)
	local openings_dict = {
		["("] = {
			")"
		},
		["["] = {
			"]"
		},
		["{"] = {
			"}"
		}
	}

	local opening = text:sub(char_index, char_index)

	local closures = openings_dict[opening]

	local text_length = text:len()

	local text_crop = text:sub(char_index + 1, text_length)

	local lowest_closure = text_length

	for closure_index = 1, #closures do
		local closure = closures[closure_index]

		local relative_scope_depth = 1

		for step_char_index = 1, text_length - (char_index + 1) do
			local char = text_crop:sub(step_char_index, step_char_index)

			if char == opening then
				relative_scope_depth = relative_scope_depth + 1
			end

			if char == closure then
				relative_scope_depth = relative_scope_depth - 1
			end

			if relative_scope_depth <= 0 then
				lowest_closure = math.min(lowest_closure, step_char_index)
				break
			end
		end
	end

	return lowest_closure + char_index
end

local function findOpener(char_index, text)
	local closures_dict = {
		[")"] = {
			"("
		},
		["]"] = {
			"["
		},
		["}"] = {
			"{"
		}
	}

	local closing = text:sub(char_index, char_index)

	local openers = closures_dict[closing]

	local text_length = text:len()

	local text_crop = text:sub(1, char_index - 1)

	local highest_opener = 0

	for opener_index = 1, #openers do
		local opener = openers[opener_index]

		local relative_scope_depth = 1

		for step_char_index = char_index - 1, 1, -1 do
			local char = text_crop:sub(step_char_index, step_char_index)

			if char == closing then
				relative_scope_depth = relative_scope_depth + 1
			end

			if char == opener then
				relative_scope_depth = relative_scope_depth - 1
			end

			if relative_scope_depth <= 0 then
				highest_opener = math.min(highest_opener, step_char_index)
				break
			end
		end
	end

	return highest_opener
end

--- Ugly function that works, it returns a table of the parametres sent to the function.
local function getParams(funct_open, funct_close, text)

	local params = {}

	local param_string = text:sub(funct_open, funct_close):gsub(", ", ",")

	print("commaspace_param_string: "..text:sub(funct_open, funct_close))

	--[[if text:sub(funct_open, funct_close) == "TEAMatrix:cloneSWMatrix(self.tea_component.component_data.transform)" then
		print(
		)
	end]]

	local _, comma_count = param_string:gsub(",", "")

	local param_start = 0
	local param_end = 0

	local last_str_len = 0

	local scope_depth = 0

	-- the last index that was outside of any scope.
	local last_scopeless_start = 0

	for comma_index = 1, comma_count+1 do
		param_start, param_end = param_string:find("[^,]+", param_start + last_str_len)

		if not param_start then
			break
		end

		last_str_len = (param_end - param_start) + 1

		-- the parametre string
		local param_str = param_string:sub(param_start, param_end)

		-- make sure this isnt just a comment
		if param_str:match("^%-%-") then
			goto getParams_continue_param
		end

		if scope_depth == 0 then
			last_scopeless_start = param_start
		end

		-- increment scope_depth by how many scopes we're entering
		local _, scope_enter_count = param_str:gsub("[{%(%[]+", "")
		scope_depth = scope_depth + scope_enter_count

		-- decrement scope_depth by how many scopes we're exiting
		local _, scope_exit_count = param_str:gsub("[}%)%]]+", "")
		scope_depth = scope_depth - scope_exit_count

		if scope_depth == 0 then
			local name = param_string:sub(last_scopeless_start, param_end)

			local _, comma_space_count = text:sub(funct_open, funct_open+param_end):gsub(", ", "")

			local cropped_text_len = text:sub(funct_open, funct_open+param_end):len() - 1

			--print(text:sub(funct_open, funct_open+param_end)..comma_space_count)

			--print("debug test: \""..text:sub(, ).."\"")

			table.insert(params, {
				name = name,
				location = {
					start = funct_open+cropped_text_len + comma_space_count - name:len(),
					--last = findClosure(last_scopeless_start+funct_open - 1, text)
					last = funct_open+cropped_text_len - 1 + comma_space_count
				}
			})

			local first_char = text:sub(last_scopeless_start+funct_open, last_scopeless_start+funct_open)
			--print("first_char: "..first_char)
			if first_char == "{" then
				params[#params].location.last = findClosure(last_scopeless_start+funct_open, text)
			end
			print("param name: "..params[#params].name)
			--print("param name from loc: "..text:sub(params[#params].location.start, params[#params].location.last))
		end

		::getParams_continue_param::
	end

	return params
end

local function getUseData(first, last, text)
	local use_data = {
		name = text:sub(first, last),
		use = "unknown",
		location = {
			start = first,
			last = last
		}
	}

	if text:sub(last+1,last+1) == "(" then -- function call
		use_data.use = "call"

		local funct_close = findClosure(last+1, text)

		print("Found usage: "..text:sub(first, funct_close))

		use_data.params = getParams(last+2, funct_close-1, text)

		for param_index, param_value in pairs(use_data.params) do
			print("param_index: "..param_index.." param_name: "..param_value.name)
		end
	end

	return use_data
end

local function mergeVariableScopeGraphReferences(scope_graph)
	if not scope_graph then
		error("(mergeVariableScopeGraphReferences) scope_graph is nil?")
		return nil 
	end

	scope_graph = table.copy.deep(scope_graph)

	local merged_scope_graph = scope_graph.references
	scope_graph.references = nil

	table.insert(merged_scope_graph, 1, scope_graph)

	return merged_scope_graph
end

-- filters the scope to the tree which the variable matches.
local function filterVariableScopeGraph(scope_graphs, variable)
	-- filter out any variables which have the same name, but are not actually the same.
	for graph_id = 1, #scope_graphs do
		local scope_graph = scope_graphs[graph_id]

		if variable.location.start == -1 then
			return scope_graph
		end

		if scope_graph.location.start == variable.location.start and scope_graph.location.last == variable.location.last then
			return scope_graph
		end

		-- go through all references in this graph
		for reference_index = 1, #scope_graph.references do
			local reference = scope_graph.references[reference_index]

			if reference.location.start == variable.location.start and reference.location.last == variable.location.last then
				return scope_graph
			end
		end
	end
end

local function getVariableScopeGraph(variable_name, text, avoids)

	local function canIter(variable, category)
		if not avoids then
			return true
		end

		if not avoids[category] then
			return true
		end

		for avoid_index = 1, #avoids[category] do
			local avoid_var = avoids[category][avoid_index]

			if avoid_var.name == variable.name and avoid_var.location.start == variable.location.start and avoid_var.location.last == variable.location.last then
				return false
			end
		end

		return true
	end

	--local scope_depth = 1

	local scope = {
		tree = {
			0
		},
		graph = {}
	}

	--[[local scope_counts = {
		0
	}]]

	local defined_graph = {}

	local forced_locals = {}
	
	--local previous_uncommented_end = 0
	local previous_end = 0

	if not parsed_data.variable_graph[variable_name] then
		print("Variable \""..variable_name.."\" does not exist. Skipping.")
		return {}
	end

	-- go through all "lua words"
	for _, node_data in ipairs(parsed_data.variable_graph[variable_name]) do

		local word_start = node_data.location.start
		local word_last = node_data.location.last

		if not word_start then
			error("word_start is nil! "..text:sub(previous_end, text:len()))
		end

		local line_start = node_data.line_location.start
		local line_last = node_data.line_location.last

		-- trim the new lines
		line_start = line_start + 1
		line_last = line_last - 1
		--print("debug: "..text:sub(previous_uncommented_end, word_last))

		--print("lua word: "..lua_word)
		-- filter out table indicies
		--local prev_char = text:sub(word_start-1, word_start-1)

		-- remove anything after a period or colon
		--[[local new_lua_word = variable_name:gsub("([^:%.])([:%.].*)", "%1")

		-- adjust the last char from the modified string
		if new_lua_word ~= variable_name then
			word_last = word_last - (variable_name:len() - new_lua_word:len())
		end]]

		--variable_name = new_lua_word

		--dbg_previous_end = previous_end

		--print("modified lua word: "..lua_word)

		local scope_string = node_data.scope

		--[[local node_data = {
			location = {
				start = word_start,
				last = word_last
			},
			line_location = {
				start = line_start,
				last = line_last
			},
			line_string = text:sub(line_start, line_last),
			word_string = variable_name,
			scope = scope_string
		}]]

		--[[if variable_name:sub(variable_name:len(), variable_name:len()) == "[" then
			node_data.word_string = node_data.word_string:sub(1, variable_name:len() - 1)
			node_data.location.last = node_data.location.last - 1
			variable_name = node_data.word_string
		end

		if lua_word == "a" then
			print()
		end]]

		local is_var_defined = false
		local graph_id = #scope.graph

		for _, defined_scope in ipairs(defined_graph) do
			if scope_string:match("^"..defined_scope.scope) then

				local forced_local = false
				for forced_local_index = 1, #forced_locals do
					if forced_locals[forced_local_index].scope_string == scope_string then
						forced_local = true
					end
				end


				if not forced_local or defined_scope.scope:match("^"..scope_string) then
					--print("def scope: "..defined_scope.scope.." scope_string: "..scope_string)
					is_var_defined = true
					graph_id = defined_scope.graph_id
					break
				end
			end
		end

		-- check if this a function param
		local prev_function = parsed_data.function_references[node_data.prev_function]

		--prev_function = nil

		local forced_local = false

		if prev_function then
			local open_bracket, _ = text:find("%(", prev_function.location.last)

			-- if this is a param
			if word_last < findClosure(open_bracket, text) then
				is_var_defined = false
				forced_local = true
			end

			-- this is a function's name
			if text:find("function "..variable_name, line_start) then
				is_var_defined = false
				scope_string = prev_function.defined_in_scope
				node_data.scope = scope_string
			end
		end

		--local prev_funct_start, prev_funct_last = text:find("function", )

		if is_var_defined then
			table.insert(scope.graph[graph_id].references, node_data)
		else
			node_data.references = node_data.references or {}
			node_data.graph_id = #scope.graph + 1
			table.insert(scope.graph, node_data)

			table.insert(defined_graph, {
				pos = word_last,
				scope = scope_string,
				graph_id = node_data.graph_id
			})

			if forced_local then
				table.insert(forced_locals, {
					pos = word_last,
					scope_string = scope_string
				})
			end
		end

		-- currently check for is_var_defined is kinda broken, so lets just assume everything is defined (seems to work fine...)
		if is_var_defined or true then

			local equals_start, _, equals_str = node_data.line_string:find("( *= *).*"..variable_name:toLiteral())

			if equals_start then
				-- we have to calculate this manually, as it doesn't understand we only want the length of the equals bit
				local equals_last = equals_start + equals_str:len() - 1

				local search_start = 1

				-- adjust param search if this is declared local.
				local set_local_start, set_local_last = node_data.line_string:find("%s*local%s*")
				if set_local_start then
					if set_local_start < equals_start then
						search_start = set_local_last + 1
					end
				end

				-- find the param we're aliasing our variable to
				local set_to = getParams(equals_last + 1, node_data.line_string:len(), node_data.line_string)
				local set_as = getParams(search_start, equals_start - 1, node_data.line_string)

				for set_to_index = 1, #set_to do
					--if set_to[set_to_index].name:gsub("("..variable_name..")(%(.*%))", "%1") == variable_name then
					--TODO: support no spaces in equations

					local set_to_name = set_to[set_to_index].name:gsub("[()]", "")
					local word_string = node_data.word_string:toLiteral()

					if not set_to_name:match(word_string) and set_to[set_to_index].name == node_data.word_string then
						print(("(\"%s\"):match(\"%s\") = %s"):format(set_to_name, word_string, tostring(set_to_name:match(word_string) ~= nil)))
					end

					if set_to_name:match(word_string) then

						if not set_as[set_to_index] then
							print("Theres a param to set this value to, but nothing to set it as! "..set_to[set_to_index].name)
							goto getVariableScopeGraph_continue_setTo
						end

						if not canIter(set_as[set_to_index], "alias") then
							print("Cannot iter variable \""..set_as[set_to_index].name.."\" as its already been itered")
							goto getVariableScopeGraph_continue_setTo
						end

						avoids = avoids or {}

						avoids.alias = avoids.alias or {}

						table.insert(avoids.alias, set_as[set_to_index])

						local alias_scope_graphs = getVariableScopeGraph(set_as[set_to_index].name, text, avoids)

						--print(string.fromTable(alias_scope_graphs))

						--[[local adjusted_alias_variable = {
							name = set_as[set_to_index].name,
							location = {
								start = set_as[set_to_index].location.start + line_start - 2,
								last = set_as[set_to_index].location.last + line_start - 2
							}
						}]]

						--local filtered_scope_graph = filterVariableScopeGraph(alias_scope_graphs, adjusted_alias_variable)

						for alias_scope_graph_id = 1, #alias_scope_graphs do
							local alias_scope_graph = mergeVariableScopeGraphReferences(alias_scope_graphs[alias_scope_graph_id])

							if not alias_scope_graph then
								error("Failed to get alias scope graph?")
							else
								for alias_scope_graph_index = 1, #alias_scope_graph do

									if graph_id == 0 then
										graph_id = 1
									end

									scope.graph[graph_id].references = scope.graph[graph_id].references or {}
									table.insert(scope.graph[graph_id].references, alias_scope_graph[alias_scope_graph_index])
								end
							end
						end
					end

					::getVariableScopeGraph_continue_setTo::
				end
			end

			--TODO: support aliasing table.insert

			--TODO: support multilines

			-- check if this is part of table.insert
			local table_insert_start, table_insert_end = node_data.line_string:find("table.insert%(")
			if table_insert_start then
				print("found insert")

				local closure_index = findClosure(table_insert_end, node_data.line_string)

				if closure_index then
					local params = getParams(table_insert_end + 1, closure_index - 1, node_data.line_string)

					if params then
						local metatable_container = params[1].name

						local metatable_container_pattern = metatable_container.."%["

						if not params[3] then
							metatable_container_pattern = metatable_container_pattern..".*]"
						else
							metatable_container_pattern = metatable_container_pattern..params[2].name.."]"
						end
						
						-- get graph of all variables which match our pattern
						for parsed_variable_name, _ in pairs(parsed_data.variable_graph) do
							if parsed_variable_name:match(metatable_container_pattern) then
								local metatable_container_graph = getVariableScopeGraph(parsed_variable_name, text, avoids)
								
								local filtered_scope_graph = filterVariableScopeGraph(metatable_container_graph, {
									name = parsed_variable_name,
									location = {
										start = -1,
										last = -1
									}
								})

								local alias_scope_graph = mergeVariableScopeGraphReferences(filtered_scope_graph)

								if alias_scope_graph then
									scope.graph[graph_id].references = scope.graph[graph_id].references or {}
									for alias_scope_graph_index = 1, #alias_scope_graph do
										table.insert(scope.graph[graph_id].references, alias_scope_graph[alias_scope_graph_index])
									end
								end
							end
						end
					end
				end
			end

			-- check for a return statement

			local return_start, _, return_string = node_data.line_string:find("(return *).*"..variable_name)

			if return_start then

				-- local return_last = return_start + return_string:len()

				--local returning_params = getParams(return_last, node_data.line_string:len(), node_data.line_string)

				-- find the function this return statement would be returning to

				-- find the function linked to this return
				local function_reference = nil
				for reference_depth = #parsed_data.function_references, 1, -1 do
					local reference = parsed_data.function_references[reference_depth]
					if scope_string:match(reference.scope) then
						function_reference = reference
						break
					end
				end

				
				if not function_reference then
					error("failed to find function reference?")
				else
					local function_name_start, function_name_last = text:find(variable_pattern, function_reference.location.last+1)
					local function_name = text:sub(function_name_start, function_name_last)

					local function_variable = {
						name = function_name,
						location = {
							start = function_name_start,
							last = function_name_last
						}
					}

					-- check if we can iter this function, used to avoid a never ending loop with function returns (as when it traces the function, it will then go through the function again)
					if canIter(function_variable, "functions") then

						avoids = avoids or {}

						avoids.functions = avoids.functions or {}

						table.insert(avoids.functions, function_variable)

						local function_scope_graphs = getVariableScopeGraph(function_name, text, avoids)

						-- filter out references to this function
						for graph_index = #function_scope_graphs, 1, -1 do
							local graph = function_scope_graphs[graph_index]

							if graph.location.start == function_name_start and graph.location.last == function_name_last then
								table.remove(function_scope_graphs, graph_index)
							end
						end

						-- merge all of the scope graphs returned
						--for graph_index = 1, #function_scope_graphs do
							--function_scope_graphs[graph_index] = mergeVariableScopeGraphReferences(function_scope_graphs[graph_index])
						--end

						--local x = 0

						-- insert these into the graph
						for graph_index = 1, #function_scope_graphs do
							table.insert(scope.graph, function_scope_graphs[graph_index])
						end

						-- go through all of these graphes, in search for more metamethod references.
						--for graph_index = 1, #function_scope_graphs do
							
						--end

						--function_scope_graph = mergeVariableScopeGraphReferences(filterVariableScopeGraph(function_scope_graph, function_variable))
					end
				end
			end
		end

		--print(lua_word..": "..scope.tree[scope_depth])

		--::next_word::
	end

	--print(string.fromTable(scope.graph))

	--print(string.fromTable(forced_locals))

	return scope.graph
end

--[[local function getVariableScopeGraph(variable_name, text, avoids)

	local function scopeToString(scope)
		local s = tostring(scope[1])

		for depth = 2, #scope do
			s = ("%s.%s"):format(s, scope[depth])
		end

		return s
	end

	local function canIter(variable, category)
		if not avoids then
			return true
		end

		if not avoids[category] then
			return true
		end

		for avoid_index = 1, #avoids[category] do
			local avoid_var = avoids[category][avoid_index]

			if avoid_var.name == variable.name and avoid_var.location.start == variable.location.start and avoid_var.location.last == variable.location.last then
				return false
			end
		end

		return true
	end

	local scope_depth = 1

	local scope = {
		tree = {
			0
		},
		graph = {}
	}

	local scope_counts = {
		0
	}

	local function_references = {}

	local defined_graph = {}

	local forced_locals = {}
	
	local previous_uncommented_end = 0
	local previous_end = 0

	-- go through all "lua words"
	for lua_word in text:gmatch(variable_pattern) do

		local word_start, word_last = text:find(string.toLiteral(lua_word), previous_end + 1)

		

		if not word_start then
			error("word_start is nil! "..text:sub(previous_end, text:len()))
		end

		local line_start, line_last = text:find("[\n]?[^\n]*"..string.toLiteral(lua_word).."[^\n]*[\n]?", findPreviousNewline(word_start, text))

		-- trim the new lines
		line_start = line_start + 1
		line_last = line_last - 1
		--print("debug: "..text:sub(previous_uncommented_end, word_last))

		--print("lua word: "..lua_word)
		-- filter out table indicies
		local prev_char = text:sub(word_start-1, word_start-1)
		if prev_char == "." or prev_char == ":" then
			goto next_word
		end

		-- remove anything after a period or colon
		local new_lua_word = lua_word:gsub("([^:%.])([:%.].*)", "%1")

		-- adjust the last char from the modified string
		if new_lua_word ~= lua_word then
			word_last = word_last - (lua_word:len() - new_lua_word:len())
		end

		lua_word = new_lua_word

		dbg_previous_end = previous_end

		--print("modified lua word: "..lua_word)

		previous_end = word_last

		--TODO: filter out block comments

		-- filter out strings
		-- TODO: (also may want to trace strings if they match our variable name, they may be used to index the metatable)
		if isString(word_start, text) then
			goto next_word
		end

		-- filter out line comments
		if text:sub(line_start, word_start):match("%-%-") then
			goto next_word
		end

		previous_uncommented_end = word_last

		local prev_scope = scopeToString(scope.tree)

		if lua_word == "function" or lua_word == "if" or lua_word == "do" then
			scope_depth = scope_depth + 1
			scope_counts[scope_depth] = scope_counts[scope_depth] and scope_counts[scope_depth] + 1 or 1
			scope.tree[scope_depth] = scope_counts[scope_depth]
		elseif lua_word == "else" or lua_word == "elseif" then
			scope_counts[scope_depth] = scope_counts[scope_depth] and scope_counts[scope_depth] + 1 or 1
			--scope.tree[scope_depth] = scope.tree[scope_depth] + 1
		elseif lua_word == "end" then
			scope.tree[scope_depth] = nil
			scope_depth = scope_depth - 1
		end

		if lua_word == "function" then
			table.insert(function_references, {
				scope = scopeToString(scope.tree),
				defined_in_scope = prev_scope,
				location = {
					start = word_start,
					last = word_last
				}
			})
		end

		if lua_word:match("^"..variable_name.."$") then

			local scope_string = scopeToString(scope.tree)

			local node_data = {
				location = {
					start = word_start,
					last = word_last
				},
				line_location = {
					start = line_start,
					last = line_last
				},
				line_string = text:sub(line_start, line_last),
				word_string = lua_word,
				scope = scope_string
			}

			if lua_word:sub(lua_word:len(), lua_word:len()) == "[" then
				node_data.word_string = node_data.word_string:sub(1, lua_word:len() - 1)
				node_data.location.last = node_data.location.last - 1
				lua_word = node_data.word_string
			end

			if lua_word == "a" then
				print()
			end

			local is_var_defined = false
			local graph_id = #scope.graph

			for _, defined_scope in ipairs(defined_graph) do
				if scope_string:match("^"..defined_scope.scope) then

					local forced_local = false
					for forced_local_index = 1, #forced_locals do
						if forced_locals[forced_local_index].scope_string == scope_string then
							forced_local = true
						end
					end


					if not forced_local or defined_scope.scope:match("^"..scope_string) then
						--print("def scope: "..defined_scope.scope.." scope_string: "..scope_string)
						is_var_defined = true
						graph_id = defined_scope.graph_id
						break
					end
				end
			end

			-- check if this a function param
			local prev_function = function_references[#function_references]

			local forced_local = false

			if prev_function then
				local open_bracket, _ = text:find("%(", prev_function.location.last)

				-- if this is a param
				if word_last < findClosure(open_bracket, text) then
					is_var_defined = false
					forced_local = true
				end

				-- this is a function's name
				if text:find("function "..lua_word, line_start) then
					is_var_defined = false
					scope_string = prev_function.defined_in_scope
					node_data.scope = scope_string
				end
			end

			--local prev_funct_start, prev_funct_last = text:find("function", )

			if is_var_defined then
				if lua_word == "vec" then
					print()
				end
				table.insert(scope.graph[graph_id].references, node_data)
			else
				if lua_word == "vec" then
					print()
				end
				node_data.references = {}
				node_data.graph_id = #scope.graph + 1
				table.insert(scope.graph, node_data)

				table.insert(defined_graph, {
					pos = word_last,
					scope = scope_string,
					graph_id = node_data.graph_id
				})

				if forced_local then
					table.insert(forced_locals, {
						pos = word_last,
						scope_string = scope_string
					})
				end
			end

			if is_var_defined then
				local equals_start, _, equals_str = node_data.line_string:find("( *= *).*"..variable_name)
				if lua_word == "vec" then
					print()
				end

				if equals_start then
					-- we have to calculate this manually, as it doesn't understand we only want the length of the equals bit
					local equals_last = equals_start + equals_str:len() - 1

					local search_start = 1

					-- adjust param search if this is declared local.
					local set_local_start, set_local_last = node_data.line_string:find("%s*local%s*")
					if set_local_start then
						if set_local_start < equals_start then
							search_start = set_local_last + 1
						end
					end

					-- find the param we're aliasing our variable to
					local set_to = getParams(equals_last + 1, node_data.line_string:len(), node_data.line_string)
					local set_as = getParams(search_start, equals_start - 1, node_data.line_string)

					for set_to_index = 1, #set_to do
						--if set_to[set_to_index].name:gsub("("..variable_name..")(%(.*%))", "%1") == variable_name then
						--TODO: support no spaces in equations
						if set_to[set_to_index].name:match(""..variable_name.."") then

							if not set_as[set_to_index] then
								print("Theres a param to set this value to, but nothing to set it as! "..set_to[set_to_index].name)
								goto getVariableScopeGraph_continue_setTo
							end

							if not canIter(set_as[set_to_index], "functions") then
								goto getVariableScopeGraph_continue_setTo
							end

							avoids = avoids or {}

							avoids.functions = avoids.functions or {}

							table.insert(avoids.functions, set_as[set_to_index])

							local alias_scope_graphs = getVariableScopeGraph(set_as[set_to_index].name, text, avoids)

							--print(string.fromTable(alias_scope_graphs))

							local adjusted_alias_variable = {
								name = set_as[set_to_index].name,
								location = {
									start = set_as[set_to_index].location.start + line_start - 1,
									last = set_as[set_to_index].location.last + line_start - 1
								}
							}
							
							local alias_scope_graph = mergeVariableScopeGraphReferences(filterVariableScopeGraph(alias_scope_graphs, adjusted_alias_variable))

							if not alias_scope_graph then
								print("Failed to get alias scope graph?")
							else
								for alias_scope_graph_index = 1, #alias_scope_graph do
									table.insert(scope.graph[graph_id].references, alias_scope_graph[alias_scope_graph_index])
								end
							end

							print()
						end

						::getVariableScopeGraph_continue_setTo::
					end

					print()
				end

				print()

				-- check for a return statement

				local return_start, _, return_string = node_data.line_string:find("(return *).*"..variable_name)

				if return_start then

					local return_last = return_start + return_string:len()

					local returning_params = getParams(return_last, node_data.line_string:len(), node_data.line_string)

					-- find the function this return statement would be returning to

					-- find the function linked to this return
					local function_reference = nil
					for reference_depth = #function_references, 1, -1 do
						local reference = function_references[reference_depth]
						if scope_string:match(reference.scope) then
							function_reference = reference
							break
						end
					end

					
					if not function_reference then
						print("failed to find function reference?")
					else
						local function_name_start, function_name_last = text:find(variable_pattern, function_reference.location.last+1)
						local function_name = text:sub(function_name_start, function_name_last)

						local function_variable = {
							name = function_name,
							location = {
								start = function_name_start,
								last = function_name_last
							}
						}

						-- check if we can iter this function, used to avoid a never ending loop with function returns (as when it traces the function, it will then go through the function again)
						if canIter(function_variable, "functions") then

							avoids = avoids or {}

							avoids.functions = avoids.functions or {}

							table.insert(avoids.functions, function_variable)

							local function_scope_graphs = getVariableScopeGraph(function_name, text, avoids)

							-- filter out referencea to this function
							for graph_index = #function_scope_graphs, 1, -1 do
								local graph = function_scope_graphs[graph_index]

								if graph.location.start == function_name_start and graph.location.last == function_name_last then
									table.remove(function_scope_graphs, graph_index)
								end
							end

							-- merge all of the scope graphs returned
							for graph_index = 1, #function_scope_graphs do
								function_scope_graphs[graph_index] = mergeVariableScopeGraphReferences(function_scope_graphs[graph_index])
							end

							-- go through all of these graphes, in search for more metamethod references.
							for graph_index = 1, #function_scope_graphs do
								
							end

							--function_scope_graph = mergeVariableScopeGraphReferences(filterVariableScopeGraph(function_scope_graph, function_variable))
							print()
						end
					end
				end
			end

			--print(lua_word..": "..scope.tree[scope_depth])
		end

		::next_word::
	end

	--print(string.fromTable(scope.graph))

	--print(string.fromTable(forced_locals))

	return scope.graph
end]]

local function getAllVariableUses(variable, script_text)

	local variable_uses = {}

	local aliases = findAliases(variable.name, script_text)

	for alias_index = 1, #aliases do
		local name = aliases[alias_index]

		local _, usage_count = script_text:gsub(name, "")

		local first = 0
		local last = 0

		for _ = 1, usage_count do
			first, last = script_text:find(name, first + 1)

			local use_data = getUseData(first, last, script_text)

			table.insert(variable_uses, use_data)
		end
	end

	return variable_uses
end

local function findVariableDefinition(variable, text)
	-- first, check if its defined where its being used

	if text:sub(variable.location.start, variable.location.start) == "{" then
		return {
			name = variable.name,
			location = {
				start = variable.location.start,
				last = variable.location.last
			}
		}
	end

	local definition = {}

	-- next, we will look through all references to this variable.
	local _, name_references = text:gsub(variable.name.." *= *", "")

	local name_start = 0
	local name_last = 0

	local last_str_len = 1

	for _ = 1, name_references do
		name_start, name_last = text:find(variable.name.." *= *", name_start + last_str_len)

		if not name_start then
			break
		end

		if name_last > variable.location.start then
			break
		end

		last_str_len = (name_last - name_start) + 1

		definition = {
			name = text:sub(name_start, name_last),
			location = {
				start = name_start,
				last = name_last
			}
		}

		if text:sub(name_last+1,name_last+1) == "{" then
			definition.location.start = name_last+1
			definition.location.last = findClosure(name_last+1, text)
			definition.name = text:sub(definition.location.start, definition.location.last)
		else
			local _, var_end = text:find("[^"..variable_pattern.."]", name_last+1)
			definition = findVariableDefinition({
				name = text:sub(name_last+1, var_end),
				location = {
					start = name_last+1,
					last = var_end
				}
			}, text)
		end

		--table.insert(definitions, definition)
	end

	if name_references == 0 then
		return variable
	end

	--definition.name = definition.name:gsub("( * = *)", "")

	return definition
end

local function findMetatableDefinitions(setmetatables, text)

	local definitions = {}

	local metatable_id = 1

	for setmetatable_index = #setmetatables, 1, -1 do
		local setmetatable = setmetatables[setmetatable_index]
		local definition = findVariableDefinition(setmetatable.params[2], text)

		--print(("Found definition for %s: %s"):format(setmetatable.params[2].name, text:sub(definition.location.start, definition.location.last)), true)

		-- check if this definition already exists
		setmetatable.metatable_id = metatable_id

		local exists = false

		for def_id, def in pairs(definitions) do
			if def.location.start == definition.location.start then
				exists = true
				setmetatable.metatable_id = def_id
				break
			end
		end

		if not exists then
			definitions[setmetatable_index] = definition

			metatable_id = metatable_id + 1
		end
	end

	local built_definitions = {}
	--local built_setmetatables = {}

	for _, definition in pairs(definitions) do
		table.insert(built_definitions, definition)
	end

	return built_definitions, setmetatables
end

local string_location_translations = {}

local function translatePosition(pos, text)
	local length_modifier = 0
	
	for start_index, amount in pairs(string_location_translations) do
		if start_index <= pos then
			length_modifier = length_modifier + amount
		end
	end

	return length_modifier
end

-- inserts a string, replaces between the start and last param
local function insertString(string, start, last, text, disable_length_modifier)
	local before_length = text:len()

	local length_modifier = translatePosition(start, text)

	if disable_length_modifier then
		length_modifier = 0
	end

	text = text:sub(1, start - 1 + length_modifier)..string..text:sub(last + 1 + length_modifier, before_length)

	local length_change = text:len() - before_length
	
	if length_change ~= 0 then
		if not string_location_translations[start] then
			string_location_translations[start] = length_change
		else
			string_location_translations[start] = string_location_translations[start] + length_change
		end
	end

	return text
end

local metamethods_to_write = {}

local function findMetamethods(graph, metatable_definitions, setmetatable_data, script_text)


	-- create a new graph, except theres no references, its all just in one table.
	local variable_instances = graph.references
	local variable_definition = graph
	variable_definition.references = nil
	table.insert(variable_instances, 1, variable_definition)

	-- search for metamethods its using

	-- find the uses of the metamethods
	for variable_instance_id = 1, #variable_instances do
		local instance = variable_instances[variable_instance_id]

		-- trim down word_string just before any . or :
		--[[if instance.word_string:match("[%.:]") then
			local char_pos, _ = instance.word_string:find("[%.:]")
			
			instance.word_string = instance.word_string:sub(1, char_pos - 1)

			instance.location.last = instance.location.start + instance.word_string:len()
		end]]
			

		--TODO: support multi-line equations.

		--TODO: Follow Order of Operations, currently, it probably doesn't give a shit.

		--TODO: support brackets

		for _, metamethod_data in ipairs(supported_metamethods) do
			for i = 1, #metamethod_data.chars do
				local char = metamethod_data.chars[i]
				if metamethod_data.handling == "arithmetic" then
					local equation_start, equation_last, eq_param1, eq_param2 = instance.line_string:advancedMatch(metamethod_data.target_patterns, {
						var = instance.word_string,
						char = char
					})
					if eq_param1 then

						local replace_str

						if eq_param2 then
							replace_str = metamethod_data.replace_pattern:format(eq_param1, eq_param2)
						else
							replace_str = metamethod_data.replace_pattern:format(eq_param1)
						end

						--print("debug!!! but might work??? !!!"..instance.line_string:sub(equation_start, equation_last))

						-- translate the equation start and last to the entire script's text location
						equation_start = equation_start + instance.line_location.start - 1
						equation_last = equation_last + instance.line_location.start - 1

						--print("debug!!! "..script_text:sub(equation_start, equation_last))

						-- check if we've already got this one
						local has_written = false
						-- backwards search as its probably more likely it was closer to the end than the start.
						for i = #metamethods_to_write, 1, -1 do
							local mtw = metamethods_to_write[i]

							if mtw.location.start == equation_start or mtw.location.last == equation_last or mtw.replace_str == replace_str then
								has_written = true
								break
							end

							-- our variable has been moved, so we should try to move ourselves.

							--[[if not eq_param2 then
								if mtw.location.start == equation_last then
									local start_move_amount, last_move_amount = mtw.replace_str:find(eq_param1)
									print()

									if start_move_amount then
										equation_last = equation_last + last_move_amount
										equation_start = equation_start + start_move_amount
									end
								elseif mtw.location.last == equation_start then
									local start_move_amount, last_move_amount = mtw.replace_str:find(eq_param1)
									print()

									if start_move_amount then
										equation_last = equation_last + last_move_amount
										equation_start = equation_start + start_move_amount
									end
								end
							end]]
						end

						if not has_written then

							table.insert(metamethods_to_write, {
								location = {
									start = equation_start,
									last = equation_last
								},
								replace_str = replace_str
							})
						end
					end
				elseif metamethod_data.handling == "index" then
					if instance.line_string:match(instance.word_string:toLiteral()..char) then
						--print(instance.line_string..":match("..instance.word_string:toLiteral()..char..")")
						--print("line_str: "..script_text:sub(instance.line_location.start, instance.line_location.last))
						--print("str: "..script_text:sub(instance.location.start, instance.location.last))
						print(script_text:sub(instance.line_location.start, math.max(instance.location.start - 1, instance.line_location.start)).." > "..script_text:sub(instance.location.start, instance.location.last).." < "..script_text:sub(math.min(instance.location.last + 1, instance.line_location.last), instance.line_location.last))
						local start_table, start_index = instance.line_string:find(instance.word_string:toLiteral()..char, instance.location.start - instance.line_location.start)
						local last_index

						if not start_index then
							break
						end

						if char == "%[" then
							last_index = findClosure(start_index, instance.line_string)
						else
							_, last_index = instance.line_string:find(variable_pattern, start_index)
						end

						if not last_index then
							break
						end

						-- ignore this if this is on the left side of an equals, as it will result in a syntax error
						local equal_start, _ = instance.line_string:find("[^=<>~]=[^=<>]")
						if equal_start then
							-- check if we are on the left of the equals, if so, skip this one
							if equal_start > start_table then
								break
							end
						end

						--start_table = start_table - 1

						local table_string = instance.line_string:sub(start_table, start_index - 1)

						local index_string = instance.line_string:sub(start_index + 1, last_index)

						-- modify index_string to have it send the name of the variable, not the value
						if char == "%." or char == ":" then
							index_string = ("\"%s\""):format(index_string)
						end

						if char == "%[" then
							index_string = index_string:sub(1, index_string:len() - 1)
						end

						local replace_str = metamethod_data.replace_pattern:format(table_string, index_string)

						local g_table_start = start_table + instance.line_location.start - 1
						local g_index_last = last_index + instance.line_location.start - 1

						-- check if we've already got this one
						local has_written = false
						-- backwards search as its probably more likely it was closer to the end than the start.
						for i = #metamethods_to_write, 1, -1 do
							local mtw = metamethods_to_write[i]

							if mtw.location.start == g_table_start and mtw.location.last == g_index_last and mtw.replace_str == replace_str then
								has_written = true
								break
							end
						end

						if not has_written then
							table.insert(metamethods_to_write, {
								location = {
									start = g_table_start,
									last = g_index_last
								},
								replace_str = replace_str
							})

							-- if this is via :, then we should put the metatable as the first parametre
							if char == ":" then
								-- check if the character after the index string is a (, that means its a table call
								if instance.line_string:sub(last_index + 1, last_index + 1) == "(" then
									-- find the closing bracket
									local closure = findClosure(last_index + instance.line_location.start, script_text)

									if closure then
										-- get the params
										local params = getParams(last_index + instance.line_location.start + 1, closure - 1, script_text)

										--print(script_text:sub(last_index + instance.line_location.start + 1, closure - 1))

										-- make sure we got the params
										if params then

											-- the string we're going to insert
											local insert_string = table_string

											-- if theres a param already, we will want to add a comma and a space after the text we're inserting
											if params[1] then
												insert_string = insert_string..", "
											end

											table.insert(metamethods_to_write, {
												location = {
													start = last_index + instance.line_location.start + 1,
													last = last_index + instance.line_location.start
												},
												replace_str = insert_string
											})
										end
									end
								end
							end
						end
					end
				end
			end
		end

		--[[if used_metamethods.__add then

			
		end

		if used_metamethods.__sub then

			if instance.line_string:match(" ?- ?"..instance.word_string) or instance.line_string:match(instance.word_string.." ?- ?") then
				local equation_start, equation_last, eq_param1, eq_param2 = instance.line_string:find("("..variable_pattern..") ?- ?("..variable_pattern..")")

				local replace_str = supported_metamethods.__sub:format(eq_param1, eq_param2)

				print("debug!!! but might work??? !!!"..instance.line_string:sub(equation_start, equation_last))

				-- translate the equation start and last to the entire script's text location
				equation_start = equation_start + instance.line_location.start - 1
				equation_last = equation_last + instance.line_location.start - 1

				print("debug!!! "..script_text:sub(equation_start, equation_last))

				-- check if we've already got this one
				local has_written = false
				-- backwards search as its probably more likely it was closer to the end than the start.
				for i = #metamethods_to_write, 1, -1 do
					local mtw = metamethods_to_write[i]

					if mtw.location.start == equation_start and mtw.location.last == equation_last and mtw.replace_str == replace_str then
						has_written = true
						break
					end
				end

				if not has_written then

					table.insert(metamethods_to_write, {
						location = {
							start = equation_start,
							last = equation_last
						},
						replace_str = replace_str
					})
				end
			end
		end]]
	end
end


function setupMetatables(script_text, script_path, metatable_usage_detection_mode)

	if not script_text then
		return
	end

	local start_time = os.clock()

	print("setting up metatables for script "..script_path, true)
	--local definitions = {}

	--[[definitions.setmetatable = findAliases("setmetatable", script_text)

	for _, alias in ipairs(definitions.setmetatable) do
		print(alias)
	end]]

	if not script_text:match("setmetatable") and metatable_usage_detection_mode ~= "everything" then
		print("no references to setmetatable, skipping metatable support setup.", true)
		return
	end

	getAllComments(script_text)

	getAllStrings(script_text)

	variable_amount = script_text:countCharInstances(variable_pattern)
	amount_per_print = math.floor(variable_amount * 0.05)

	getVariableGraph(script_text)

	local setmetatable_definition = {
		name = "setmetatable",
		location = {
			start = "global",
			last = "global"
		}
	}

	local setmetatables = getAllVariableUses(setmetatable_definition, script_text)

	-- remove all non setmetatable calls
	for setmetatable_index = #setmetatables, 1, -1 do
		if setmetatables[setmetatable_index].use ~= "call" then
			table.remove(setmetatables, setmetatable_index)
		end
	end

	local metatable_definitions, setmetatables = findMetatableDefinitions(setmetatables, script_text)

	print("finding possible metamethod calls", true)

	local find_possible_metamethod_calls_start = os.clock()

	if metatable_usage_detection_mode == "everything" then
		for variable_name, _ in pairs(parsed_data.variable_graph) do
			local scope_graph = getVariableScopeGraph(variable_name, script_text)


			local variable_pattern_variable = {
				name = variable_name,
				location = {
					start = -1,
					last = 0
				}
			}

			local graph = filterVariableScopeGraph(scope_graph, variable_pattern_variable)

			if graph then
				findMetamethods(graph, metatable_definitions, {}, script_text)
			end
		end
	end

	if metatable_usage_detection_mode == "smart" then
		for setmetatable_index = 1, #setmetatables do
			-- local metatabled_table_uses = getVariableGraph(setmetatables[setmetatable_index].params[1].name, script_text)

			local setmetatable_data = setmetatables[setmetatable_index]

			print("finding possible metamethod calls for "..setmetatable_data.params[1].name, true)

			local param1 = setmetatable_data.params[1]

			local scope_graphs = getVariableScopeGraph(param1.name, script_text)

			--local graph = filterVariableScopeGraph(scope_graphs, param1)

			if scope_graphs then
				for graph_id = 1, #scope_graphs do
					local graph = scope_graphs[graph_id]
					findMetamethods(graph, metatable_definitions, setmetatable_data, script_text)
				end
			else
				print("Failed to find the graph?")
			end
		end
	end

	print(("found %s possible metamethod calls, took %0.2fs"):format(#metamethods_to_write, os.clock() - find_possible_metamethod_calls_start), true)

	print(string.fromTable(metamethods_to_write))

	print("preparing to modify script...", true)

	-- prepare metatable definitions

	local metatable_string = "TEA = {\n	metatables = {}\n}"

	--[[for metatable_id = 1, #metatable_definitions do
		local metatable = metatable_definitions[metatable_id]
		
		if metatable_id ~= 1 then
			metatable_string = metatable_string..",\n"
		end

		metatable_string = metatable_string.."		"

		local str = script_text:sub(metatable.location.start, metatable.location.last)

		local mt_start, _ = str:find("{")

		str = str:sub(mt_start, str:len())

		str = str:gsub("(\n)", "\n		")

		metatable_string = ("%s%s"):format(metatable_string, str)
	end

	metatable_string = metatable_string.."\n	}\n}"]]

	-- modify the setmetatable functions

	--[[local adjusted_length = 0
	
	for setmetatable_index = 1, #setmetatables do
		local setmetatable = setmetatables[setmetatable_index]

		--local previous_length = script_text:len()

		print("replacing "..script_text:sub(setmetatable.params[2].location.start, setmetatable.params[2].location.last))

		script_text = insertString(setmetatable.metatable_id, setmetatable.params[2].location.start, setmetatable.params[2].location.last, script_text)
		--script_text = script_text:sub(1, setmetatable.params[2].location.start - 1)..setmetatable.metatable_id..script_text:sub(setmetatable.params[2].location.last + 1, script_text:len())

		--adjusted_length = adjusted_length + ((previous_length - script_text:len()) + string.len(setmetatable.metatable_id))

		print("adjusted_length: "..adjusted_length)
	end]]

	-- setup up metamethods
	print("setting up metamethods", true)
	for mtw_id = 1, #metamethods_to_write do
		local mtw = metamethods_to_write[mtw_id]

		script_text = insertString(mtw.replace_str, mtw.location.start, mtw.location.last, script_text)

		--script_text = script_text:sub(1, mtw.location.start - 1)..mtw.replace_str..script_text:sub(mtw.location.last + 1, script_text:len())
	end

	-- remove redudant code

	--TODO: properly handle multi variable definitions.
	for metatable_definition_id = 1, #metatable_definitions do
		local metatable_definition = metatable_definitions[metatable_definition_id]

		--local length_modifier = translatePosition(metatable_definition.location.start, script_text)

		--local full_definition_start, full_definition_last = script_text:find(variable_pattern.."= *"..string.toLiteral(metatable_definition.name))


		script_text = insertString(metatable_definition_id, metatable_definition.location.start, metatable_definition.location.last, script_text)

		-- insert the metatable definition

		local metatable_definition_string = ("\nTEA.metatables[%s] = %s\n"):format(metatable_definition_id, metatable_definition.name)

		-- location is + character length of the metatable id to avoid overwriting the id set.
		local metatable_defininition_insert_location = metatable_definition.location.last + tostring(metatable_definition_id):len()
		script_text = insertString(metatable_definition_string, metatable_defininition_insert_location, metatable_defininition_insert_location, script_text)
		-- handle local
		--[[if script_text:sub(full_definition_start - 6, full_definition_start - 1) == "local " then
			full_definition_start = full_definition_start - 6
		end

		-- remove from string
		script_text = insertString("", full_definition_start, full_definition_last, script_text, true)]]
	end

	-- add metatable definitions
	print("adding metatable definitions", true)
	script_text = ("%s\n%s"):format(metatable_string, script_text)

	print("adding SWAMS code", true)
	script_text = ("%s\n\n%s"):format(SWAMS_code, script_text)

	print("writing to "..script_path, true)
	LifeBoatAPI.Tools.FileSystemUtils.writeAllText(LifeBoatAPI.Tools.Filepath:new(script_path, true), script_text)

	print(("(SWAMS) completed setting up metatables! Took %ss"):format(os.clock() - start_time), true)

	return script_text
end