return (function()
	local function decode_bytecode(bytecode)
		local L_29_, L_30_, L_31_ = "", "", {}
		local L_32_ = 256
		local L_33_ = {}

		for i = 0, L_32_ - 1 do
			L_33_[i] = string.char(i)
		end

		local L_34_ = 1
		local function L_35_func()
			local L_37_ = tonumber(string.sub(bytecode, L_34_, L_34_), 36)
			L_34_ = L_34_ + 1
			local L_38_ = tonumber(string.sub(bytecode, L_34_, L_34_ + L_37_ - 1), 36)
			L_34_ = L_34_ + L_37_
			return L_38_
		end

		L_29_ = string.char(L_35_func())
		L_31_[1] = L_29_

		while L_34_ < #bytecode do
			local L_39_ = L_35_func()
			if L_33_[L_39_] then
				L_30_ = L_33_[L_39_]
			else
				L_30_ = L_29_..string.sub(L_29_, 1, 1)
			end
			L_33_[L_32_] = L_29_..string.sub(L_30_, 1, 1)
			L_31_[#L_31_ + 1], L_29_, L_32_ = L_30_, L_30_, L_32_ + 1
		end

		return table.concat(L_31_)
	end

	local bytecode = decode_bytecode("1S1R2751R27427522X23I22Y23223627A1R1V27623G23E23A23I1R1H27622K23I23322023I22X23123623C27M1M27622A23922W27U27S27U27W27Y27G27623E2322332371R28027527T27V27U22023323822X23E23G27Z27622G23823923H23623G23228P23323628V2761R1L27619192761Q29827529C1P27621N29E1R29C1O1R29A1R29M27H29529K29P1R29G29I29T29C1U29S29V2A229O29C29C27529O29G29G27529X2A91R1T1R21N29M2AD2781G29T2AA2752972952A61A29B295")

	local function get_bits(bits, start, finish)
		if finish then
			local bit = (bits / 2 ^ (start - 1)) % 2 ^ ((finish - 1) - (start - 1) + 1)
			return bit - bit % 1
		else
			local bit = 2 ^ (start - 1)
			return (bits % (bit + bit) >= bit) and 1 or 0
		end
	end

	-- This is the string index which the data is read from
	local st_index = 1

	-- The get_int32 reads 4 bytes off the bytecode and converts them to a 32 bit intiger
	local function get_int32()
		local byte1, byte2, byte3, byte4 = string.byte(bytecode, st_index, st_index + 3)

		byte1 = bit32.bxor(byte1, 27)
		byte2 = bit32.bxor(byte2, 27)
		byte3 = bit32.bxor(byte3, 27)
		byte4 = bit32.bxor(byte4, 27)
		st_index = st_index + 4

		return (byte4 * 16777216) + (byte3 * 65536) + (byte2 * 256) + byte1
	end

	-- The get_byte reads 1 bytes off the bytecode to create a byte also known as an 8 bit intiger
	local function get_byte()
		local byte1 = bit32.bxor(string.byte(bytecode, st_index, st_index), 27)

		st_index = st_index + 1

		return byte1
	end

	-- The get_int16 reads 2 bytes off the bytecode and converts them to a 16 bit intiger
	local function get_int16()
		local byte1, byte2 = string.byte(bytecode, st_index, st_index + 2)

		byte1 = bit32.bxor(byte1, 27)
		byte2 = bit32.bxor(byte2, 27)
		st_index = st_index + 2

		return (byte2 * 256) + byte1
	end

	-- The get_float reads a floating point number value
	local function get_float()
		local left = get_int32()
		local right = get_int32()
		local is_normal = 1
		local mantissa = (get_bits(right, 1, 20) * (2 ^ 32)) + left
		local exponent = get_bits(right, 21, 31)
		local sign = ((-1) ^ get_bits(right, 32))

		if exponent == 0 then
			if mantissa == 0 then
				return sign * 0
			else
				exponent = 1
				is_normal = 0
			end
		elseif exponent == 2047 then
			return (mantissa == 0) and (sign * (1 / 0)) or (sign * (0 / 0))
		end

		return math.ldexp(sign, exponent - 1023) * (is_normal + (mantissa / (2 ^ 52)))
	end

	-- The get_string functions reads a string from the bytecode
	local function get_string(string_lenght)
		local encoded_string
		if not string_lenght then
			string_lenght = get_int32()
			if string_lenght == 0 then
				return ""
			end
		end

		encoded_string = string.sub(bytecode, st_index, st_index + string_lenght - 1)
		st_index = st_index + string_lenght

		local final_string = {}
		for i = 1, #encoded_string do
			final_string[i] = string.char(bit32.bxor(string.byte(string.sub(encoded_string, i, i)), 27))
		end

		return table.concat(final_string)
	end

	local function pack_args(...)
		return {
			...
		}, select("#", ...)
	end

	local function bytecode_to_protos()
		local instructions = {}
		local subProtos = {}
		local L_72_ = {} -- Unused ??
		local protos = {
			[1] = instructions,
			[2] = subProtos,
			[3] = nil,
			[4] = L_72_,
		}
		local constLenght = get_int32()
		local constants = {}

		-- Decode constants
		for i = 1, constLenght do
			local constantType = get_byte()
			local const

			if constantType == 1 then -- Boolean constants
				const = (get_byte() ~= 0)
			elseif constantType == 2 then -- Number constants (are doubles floats??)
				const = get_float()
			elseif constantType == 0 then -- String constants
				const = get_string()
			end

			constants[i] = const
		end

		-- Decode subfunction protos
		for i = 1, get_int32() do
			subProtos[i - 1] = bytecode_to_protos()
		end

		-- Decode instructions
		for i = 1, get_int32() do
			local L_81_ = get_byte()

			if get_bits(L_81_, 1, 1) == 0 then
				local regType = get_bits(L_81_, 2, 3)
				local constReg = get_bits(L_81_, 4, 6)
				local inst = {
					get_int16(),
					get_int16(),
					nil,
					nil
				}

				-- Decode instruction registers
				if regType == 0 then
					inst[3] = get_int16()
					inst[4] = get_int16()
				elseif regType == 1 then
					inst[3] = get_int32()
				elseif regType == 2 then
					inst[3] = get_int32() - (2 ^ 16)
				elseif regType == 3 then
					inst[3] = get_int32() - (2 ^ 16)
					inst[4] = get_int16()
				end

				-- Inline constants to instruction
				if get_bits(constReg, 1, 1) == 1 then
					inst[2] = constants[inst[2]]
				end
				if get_bits(constReg, 2, 2) == 1 then
					inst[3] = constants[inst[3]]
				end
				if get_bits(constReg, 3, 3) == 1 then
					inst[4] = constants[inst[4]]
				end

				instructions[i] = inst
			end
		end

		protos[3] = get_byte()

		return protos
	end

	local function lua_wrap_state(protos, upvalues, env)
		protos = (protos == true and bytecode_to_protos()) or protos

		return (function(...)
			local instructions = protos[1]
			local max_arg_count = protos[3]
			local subProtos = protos[2]
			local pc = 1
			local top_index = -1
			local extra_args = {} -- Unused??
			local f_args = {
				...
			}
			local f_arg_count = select("#", ...) - 1
			local L_97_ = {} -- Unused??
			local stack = {}
			for i = 0, f_arg_count do
				if i >= max_arg_count then
					extra_args[i - max_arg_count] = f_args[i + 1]
				else
					stack[i] = f_args[i + 1]
				end
			end
			local L_99_ = f_arg_count - max_arg_count + 1 -- Unused??
			local instruction
			local opcode

			-- Program instructions \/
			--[[
			18	0	require
			0	1	game
			0	1	1	GetService
			0	3	InsertService
			0	1	3	2
			0	1	1	auth
			0	0	2	2
			0	1	require
			0	2	game
			0	2	2	ServerStorage
			3	2	2	Configuration
			11	1	2	0
			14	0	0	1
			17	0	1	0
			]]

			while true do -- Interpreter loop
				instruction = instructions[pc] -- Get the instruction from the intruction list that the program counter points to
				opcode = instruction[1] -- Get the opcode of the instruction

				if opcode == 0 or opcode == 7 then --[[CALL_ONEARG]]
					local opr2 = instruction[2]
					stack[opr2] = stack[opr2](stack[opr2 + 1])
				elseif opcode == 1 or opcode == 11 then --[[CALL]]
					local opr2 = instruction[2]
					local args, arg_count = pack_args(stack[opr2](stack[opr2 + 1]))
					top_index = arg_count + opr2 - 1
					local L_106_ = 0

					for i = opr2, top_index do
						L_106_ = L_106_ + 1
						stack[i] = args[L_106_]
					end
				elseif opcode == 2 or opcode == 9 then --[[CALL_ONERETURN]]
					local opr2 = instruction[2]
					stack[opr2] = stack[opr2](unpack(stack, opr2 + 1, instruction[3]))
				elseif opcode == 3 or opcode == 13 then --[[GETTABLE]]
					stack[instruction[2]] = stack[instruction[3]][instruction[4]]
				elseif opcode == 4 or opcode == 14 then --[[CALL_NORETURN]]
					local opr2 = instruction[2]
					stack[opr2](unpack(stack, opr2 + 1, top_index))
				elseif opcode == 5 or opcode == 10 then --[[SELF]]
					local opr2 = instruction[2]
					local object = stack[instruction[3]]

					stack[opr2 + 1] = object
					stack[opr2] = object[instruction[4]]
				elseif opcode == 6 or opcode == 15 then --[[GETGLOBAL]]
					stack[instruction[2]] = env[instruction[3]]
				elseif opcode == 8 or opcode == 16 then
					stack[instruction[2]] = instruction[3]
				elseif opcode == 12 or opcode == 17 then --[[RETURN]]
					do
						return
					end
				else -- Inlined instructions
					local object
					local opr2

					--[[GETGLOBAL - 0 - require]]
					stack[instruction[2]] = env[instruction[3]] -- ; require
					pc = pc + 1

					--[[GETGLOBAL - 1 - game]]
					instruction = instructions[pc]
					stack[instruction[2]] = env[instruction[3]]
					pc = pc + 1

					--[[SELF - 1 - 1 - GetService]]
					instruction = instructions[pc]
					opr2 = instruction[2]
					object = stack[instruction[3]]
					stack[opr2 + 1] = object
					stack[opr2] = object[instruction[4]] -- ; GetService
					pc = pc + 1

					--[[LOADK]]
					instruction = instructions[pc]
					stack[instruction[2]] = instruction[3]
					pc = pc + 1

					--[[CALL_ONERETURN]]
					instruction = instructions[pc]
					opr2 = instruction[2]
					stack[opr2] = stack[opr2](unpack(stack, opr2 + 1, instruction[3]))
					pc = pc + 1

					--[[GETTABLE]]
					instruction = instructions[pc]
					stack[instruction[2]] = stack[instruction[3]][instruction[4]]
					pc = pc + 1

					--[[CALL_ONEARG]]
					instruction = instructions[pc]
					opr2 = instruction[2]
					stack[opr2] = stack[opr2](stack[opr2 + 1])
					pc = pc + 1

					--[[GETGLOBAL]]
					instruction = instructions[pc]
					stack[instruction[2]] = env[instruction[3]]
					pc = pc + 1

					--[[GETGLOBAL]]
					instruction = instructions[pc]
					stack[instruction[2]] = env[instruction[3]]
					pc = pc + 1

					--[[GETTABLE]]
					instruction = instructions[pc]
					stack[instruction[2]] = stack[instruction[3]][instruction[4]]
				end

				pc = pc + 1
			end
		end)
	end

	return lua_wrap_state(true, {}, getfenv())()
end)()
