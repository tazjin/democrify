// ================================================================
// Copyright (C) 2010 Tim Scheffler
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ================================================================


#include <objc/objc.h>

#include <Foundation/NSString.h>

#include <objc/runtime.h>

#include <Foundation/NSArray.h>
#include <Foundation/NSRange.h>
#include <Foundation/Foundation.h>
#include "FFI.h"

void releaseId(id object);
id retainId(id object);
id autoreleaseId(id object);

id performMethod0(const char* methodName, id object);
id performMethod1(const char* methodName, id object, id arg1);
int isKindOf(id object, const char *name);

id getValueForKey(id object, NSString *key);
void setValueForKey(id object, NSString *key, id value);


NSString *utf8ToNSString(const char* cstr);
const char *nsStringToUtf8(NSString *str);


double doubleValue(NSNumber *aNumber);
NSNumber *numberWithDouble(double aDouble);
long longValue(NSNumber *aNumber);
NSNumber *numberWithLong(long aLong);

NSArray *arrayWithCArray(id *objects, NSUInteger count);
id *getObjects(NSArray *anArray);
NSUInteger lengthOfArray(NSArray *anArray);

id getKeysAndValues(NSDictionary *aDict);
NSDictionary *dictWithKeysAndValues(NSArray *keys, NSArray *values);



// Arbitrary Haskell values
// (A wrapper for StablePtr)
@interface HSValue : NSObject {
    HsStablePtr *hsValue;
}

-(id)initWithHaskellValue:(HsStablePtr)value;
-(void)clearStablePtr;
-(HsStablePtr)stablePtr;

@end

id newHSValue(const char *name, HsStablePtr value);
HsStablePtr hsValue_getStablePtr(HSValue *hsvalue);


// Functions

// Id -> IO Id
@interface HSFunc1 : HSValue {
}

-(id)callWithArg:(id)arg1;
@end

// Id -> Id -> IO Id
@interface HSFunc2 : HSValue {
}

-(id)callWithArg:(id)arg1 arg2:(id)arg2;
@end


// These functions must be provided by the Haskell Controller
HSValue *initController(NSDictionary *ivars);
NSArray *getMethodNames(HSValue *controller);
id getMethod(HSValue *controller, NSString *methodName);
