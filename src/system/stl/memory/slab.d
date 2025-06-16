module memory.slab;

import stl.vmm.heap;

/// Basic slab allocator
struct Slab {
	size_t objSize;
	void*[] freeList;

	void init(size_t size) {
		objSize = size;
	}

	void* alloc() {
		if (!freeList.empty)
			return freeList.pop();

		// Allocate new block from Heap
		void* p = cast(void*)Heap.allocate(objSize).ptr;
		return p;
	}

	void free(void* ptr) {
		freeList ~= ptr;
	}
}
