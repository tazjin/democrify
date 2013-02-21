#import <Cocoa/Cocoa.h>
#import "FFI.h"
#import "HSObjC_C.h"
#import "AppDelegate.h"

id callFunc1(HsStablePtr func, id arg1)
{
    return nil;
}

id callFunc2(HsStablePtr func, id arg1, id arg2)
{
    return nil;
}

void freeStablePtr(HsStablePtr aStablePointer)
{
    return;
}

HSValue *initController(NSDictionary *ivars)
{
    return nil;
}

NSArray *getMethodNames(HSValue *controller)
{
    return nil;
}

NSString *getNextTrack()
{
    return nil;
}

void setResourcePath(NSString *path)
{
    return;
}

void extEmptyQueue()
{
    return;
}

void loadPlaylist(NSArray *tracks)
{
    return;
}


id getMethod(HSValue *controller, NSString *methodName)
{
    return nil;
}

int main(int argc, char *argv[])
{
    return NSApplicationMain(argc,  (const char **) argv);
}
