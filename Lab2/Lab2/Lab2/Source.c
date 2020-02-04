

#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>

struct Block
{
	int block_size; // # of bytes in the data section
	int free;
	struct Block *next_block; // in C, you have to use "struct Block" as the type
};

const int block_size = sizeof(struct Block);
const int void_size = sizeof(void*);
struct Block *free_head;

void my_initialize_heap(int);
void* my_alloc(int size);
void my_free(void *data);

void my_initialize_heap(int size)
{
	// use malloc to initialize a buffer of given size to use in Custom Allocator (ONLY TIME ALLOWED TO USE MALLOC IN THE WHOLE PROGRAM)
	// free_head should point to the buffer
	// initialize the header with appropriate values for block_size and next_block
	free_head = malloc(size);
	free_head->free = 1; // added this to flag each block as free or not
	free_head->block_size = size - block_size;
	free_head->next_block = NULL;
}


void* my_alloc(int size)
{
	struct Block *curr = free_head;

	if (size <= 0)
		return NULL;

	// Rounded up size of the data being allocated 
	int roundedUpSize = size + block_size;

	if ((roundedUpSize % void_size) != 0)
		roundedUpSize += void_size - (roundedUpSize % void_size);

	// Use First Fit Heuristic to find the first free block that fits 
	while((curr->block_size < size || curr->free == 0) && curr->next_block != NULL)
	{
		curr = curr->next_block;
	}

	int excessSpace = curr->block_size - roundedUpSize;

	if (curr->block_size == size)
	{
		// can't split
		curr->free = 0;

		return (void*)(++curr);
	}
	else if (curr->block_size > roundedUpSize)
	{
		// splitting the blocks 
		struct Block* new_block = (void*)((char*)(curr) + roundedUpSize);
		new_block->block_size = excessSpace;
		new_block->free = 1;
		new_block->next_block = curr->next_block;

		// sets the block in the free list
		curr->block_size = size;
		curr->free = 0;
		curr->next_block = new_block;

		return (void*)(++curr);
	}
	else
	{
		// if insufficient amount of memory return null
		return NULL;
	}

}


void my_free(void *data)
{
	// set the data block as free
	struct Block* temp = (struct Block*) data - 1;
	temp->free = 1;

	// merge the blocks in the memory to remove the data block 
	struct Block *curr;
	curr = free_head;
	while (curr && curr->next_block) {
		if (curr->free && curr->next_block->free) {
			curr->block_size += (curr->next_block->block_size) + block_size;
			curr->next_block = curr->next_block->next_block;
		}
		curr = curr->next_block;
	}
}

void Test1()
{
	// Test 1
	int *test1_a = my_alloc(sizeof(int));
	printf("%p\n", test1_a);
	my_free(test1_a);
	int *test1_b = my_alloc(sizeof(int));
	printf("%p\n", test1_b);
	my_free(test1_b);
}

void Test2()
{
	// Test 2
	int *test2_a = my_alloc(sizeof(int));
	printf("%p\n", test2_a);
	int *test2_b = my_alloc(sizeof(int));
	printf("%p\n", test2_b);
}

void Test3()
{
	//Test 3
	int *test3_a = my_alloc(sizeof(int));
	int *test3_b = my_alloc(sizeof(int));
	int *test3_c = my_alloc(sizeof(int));
	printf("%p\n", test3_a);
	printf("%p\n", test3_b);
	printf("%p\n", test3_c);
	my_free(test3_b);
	int *test3_d = my_alloc(2 * sizeof(double));
	printf("%p\n", test3_d);
	int *test3_f = my_alloc(sizeof(int));
	printf("%p\n", test3_f);
}

void Test4()
{
	//Test4
	int *test4_a = my_alloc(sizeof(char));
	int *test4_b = my_alloc(sizeof(int));
	printf("%p\n", test4_a);
	printf("%p\n", test4_b);
}

void Test5()
{
	//Test 5
	int *test5_a = my_alloc(80 * sizeof(int));
	int *test5_b = my_alloc(sizeof(int));
	printf("%p\n", test5_a);
	printf("%p\n", test5_b);
	my_free(test5_a);
	printf("%p\n", test5_b);
	

	
}

int main()
{
	printf("The size of struct block : %d\n", block_size);
	printf("The size of void pointer : %d\n", void_size);

	my_initialize_heap(1000);

	//Test1();
	//Test2();
	Test3();
	//Test4();
	//Test5();	

	system("pause");
	return 0;
}
