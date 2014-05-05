#include <iostream>
#include <string>
#include <cstdint>
#include <vector>
#include <iomanip>

uint64_t read64Hex() {
	std::string str;
	std::cin >> str;
	
	return std::stoull(str, 0, 16);
}

uint32_t read32Hex() {
	std::string str;
	std::cin >> str;
	return std::stoul(str, 0, 16);
}

uint16_t processSelector(uint16_t selector, bool & use_gdt, int & rpl) {
	rpl = selector & 3;
	use_gdt = (selector & (1 << 2)) == 0;
	return (selector >> 3);
}

uint64_t getBits(uint64_t num, int left, int right) {
	num >>= right;
	num &= (1LL << (left - right + 1)) - 1;
	return num;
}

uint32_t process_segment_descriptor(uint64_t descriptor, int & dpl) {
	uint32_t base = (uint32_t)getBits(descriptor, 63, 56);
	base <<= 8;
	base |= getBits(descriptor, 39, 32);
	base <<= 16;
	base |= getBits(descriptor, 31, 16);

	dpl = (int) getBits(descriptor, 46, 45);

	return base;
}

void read_table(std::vector<uint64_t> & vec) {
	size_t n;
	std::cin >> n;
	vec.resize(n);

	for (size_t i = 0; i < n; i++) {
		vec[i] = read64Hex();
	}
}

bool process_linear_address(uint32_t linear_address, uint32_t & physical_address){
	uint32_t offset = (uint32_t)getBits(linear_address, 11, 0);
	uint32_t table_idx = (uint32_t)getBits(linear_address, 21, 12);
	uint32_t directory_idx = (uint32_t)getBits(linear_address, 31, 22);

	std::vector<uint64_t> directory, table;
	read_table(directory);
	read_table(table);

	if (directory_idx >= directory.size() || (directory[directory_idx] & 1) == 0) {
		return false;
	}

	if (table_idx >= table.size() || (table[table_idx] & 1) == 0) {
		return false;
	}

	uint32_t page_base = (uint32_t)getBits(table[table_idx], 31, 12);

	physical_address = page_base + offset;
	return true;
}

bool solve(uint32_t & physical_address) {
	uint32_t logic_offset = read32Hex();
	uint16_t seg_selector = (uint16_t)read32Hex();

	bool use_gdt;
	int rpl;
	uint16_t segment_idx = processSelector(seg_selector, use_gdt, rpl);

	std::vector<uint64_t> gdt, ldt;
	
	read_table(gdt);
	read_table(ldt);

	uint64_t descriptor;
	if (use_gdt) {
		if (segment_idx == 0 || segment_idx >= gdt.size()) {
			return false;
		}

		descriptor = gdt[segment_idx];
	}
	else {
        if(segment_idx >= ldt.size()) {
            return false;
        }

		descriptor = ldt[segment_idx];
	}

	int dpl;
	uint32_t base_address = process_segment_descriptor(descriptor, dpl);
	
	if (rpl < dpl) {
		return false;
	}

	uint32_t linear_address = base_address + logic_offset;
	return process_linear_address(linear_address, physical_address);
}

int main() {
	uint32_t res;

	if (!solve(res)) {
		std::cout << "INVALID" << std::endl;
	}
	else {
		std::cout << std::hex << std::setfill('0') << std::left << std::setw(8) << res << std::endl;
	}

	return 0;
}
