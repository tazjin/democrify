#ifndef FFI_H
#define FFI_H

typedef unsigned int HsChar;  // on 32 bit machine
typedef int HsInt;
typedef unsigned int HsWord;

// Ensure that we use C linkage for HsFunPtr 
#ifdef __cplusplus
extern "C"{
#endif
	
	typedef void (*HsFunPtr)(void);
	
#ifdef __cplusplus
}
#endif

typedef void *HsPtr;
typedef void *HsForeignPtr;
typedef void *HsStablePtr;

#define HS_BOOL_FALSE 0
#define HS_BOOL_TRUE 1
#endif // FFI_H
