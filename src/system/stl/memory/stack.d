module memory.stack;

import stl.vmm.heap;

/// Allocate per-thread stacks (e.g., 16KB per thread)
struct StackAllocator {
	enum STACK_SIZE = 4096 * 4;

	void* allocStack() {
		ubyte[] stack = Heap.allocate(STACK_SIZE);
		return stack.ptr + stack.length; // Return top of stack
	}
}
