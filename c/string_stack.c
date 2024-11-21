#include <stdlib.h>
#include <string.h>
#include "string_stack.h"

// Define the Stack structure
typedef struct _Stack {
    char** data;      
    int capacity;     
    int count;        
} _Stack;

// Helper function to resize the stack
static response_code resize(stack s, int new_capacity) {
    if (new_capacity > MAX_CAPACITY) {
        return stack_full;
    }
    char** new_data = (char**)realloc(s->data, new_capacity * sizeof(char*));
    if (new_data == NULL) {
        return out_of_memory;
    }
    s->data = new_data;
    s->capacity = new_capacity;
    return success;
}

// Create a new stack
stack_response create() {
    stack_response res;
    stack s = (stack)malloc(sizeof(_Stack));
    if (s == NULL) {
        res.code = out_of_memory;
        res.stack = NULL;
        return res;
    }

    s->data = (char**)malloc(MIN_CAPACITY * sizeof(char*));
    if (s->data == NULL) {
        free(s);
        res.code = out_of_memory;
        res.stack = NULL;
        return res;
    }

    s->capacity = MIN_CAPACITY;
    s->count = 0;
    res.code = success;
    res.stack = s;
    return res;
}

// Get the current size of the stack
int size(const stack s) {
    return s->count;
}

// Check if the stack is empty
bool is_empty(const stack s) {
    return s->count == 0;
}

// Check if the stack is full
bool is_full(const stack s) {
    return s->count == MAX_CAPACITY;
}

// Push a string onto the stack
response_code push(stack s, char* item) {
    if (strlen(item) >= MAX_ELEMENT_BYTE_SIZE) {
        return stack_element_too_large;
    }
    if (is_full(s)) {
        return stack_full;
    }
    if (s->count == s->capacity) {
        response_code res = resize(s, s->capacity * 2);
        if (res != success) {
            return res;
        }
    }
    s->data[s->count] = strdup(item); // Defensive copy
    if (s->data[s->count] == NULL) {
        return out_of_memory;
    }
    s->count++;
    return success;
}

// Pop a string from the stack
string_response pop(stack s) {
    string_response res;
    if (is_empty(s)) {
        res.code = stack_empty;
        res.string = NULL;
        return res;
    }
    res.string = s->data[--s->count];
    res.code = success;

    // Shrink the stack if needed
    if (s->count < s->capacity / 4 && s->capacity > MIN_CAPACITY) {
        resize(s, s->capacity / 2);
    }
    return res;
}

// Destroy the stack
void destroy(stack* s) {
    if (s == NULL || *s == NULL) {
        return;
    }
    for (int i = 0; i < (*s)->count; i++) {
        free((*s)->data[i]);
    }
    free((*s)->data);
    free(*s);
    *s = NULL;
}
